#' @include turns.R
NULL

# Type-checking helpers for Content subclasses.
is_tool_request <- function(x) S7_inherits(x, ContentToolRequest)
is_tool_result <- function(x) S7_inherits(x, ContentToolResult)

# Populates the @tool slot on ContentToolRequest and ContentToolResult
# objects within a turn. When turns are deserialized from JSON or
# constructed from API responses, they contain tool requests by name
# only (the @tool slot is NULL). This function matches each request to
# the actual Tool object from the `tools` named list so the tool's R
# function can be called later.
match_tools <- function(turn, tools) {
  if (is.null(turn)) {
    return(NULL)
  }

  turn@contents <- map(turn@contents, function(content) {
    if (is_tool_request(content)) {
      # Look up the Tool by name and attach it to the request.
      content@tool <- content@tool %||% tools[[content@name]]
      return(content)
    }

    if (is_tool_result(content)) {
      # For tool results, the Tool lives on the nested request object.
      content@request@tool <-
        content@request@tool %||% tools[[content@request@name]]
      return(content)
    }

    content
  })

  turn
}

on_load({
  # A coro generator that iterates over tool requests in an assistant
  # turn and invokes each one. Yields ContentToolResult objects (and
  # optionally ContentToolRequest objects when yield_request = TRUE).
  #
  # Called from chat_impl() when the assistant's response contains tool
  # calls. The yielded results are collected and wrapped into a UserTurn
  # to send back to the LLM.
  invoke_tools <- coro::generator(function(
    turn, # The assistant Turn containing ContentToolRequest objects
    echo = "none", # Controls printing of tool calls/results to stdout
    on_tool_request = function(request) invisible(), # Callback before invoke
    on_tool_result = function(result) invisible(), # Callback after invoke
    yield_request = FALSE, # If TRUE, also yield each request before its result
    otel_span = NULL # Parent OpenTelemetry span
  ) {
    # Pull out just the ContentToolRequest objects from the turn.
    tool_requests <- extract_tool_requests(turn)

    for (request in tool_requests) {
      # Print the tool call to stdout if echo="output".
      maybe_echo_tool(request, echo = echo)
      # Yield the request itself so callers (e.g. $stream()) can show
      # tool call progress before the result arrives.
      if (yield_request) {
        yield(request)
      }

      # Run the on_tool_request callback. If the callback calls
      # tool_reject(), we get back an error ContentToolResult and skip
      # actually invoking the tool.
      rejected <- maybe_on_tool_request(request, on_tool_request)
      if (!is.null(rejected)) {
        maybe_echo_tool(rejected, echo = echo)
        on_tool_result(rejected)
        yield(rejected)
        next
      }

      # Actually call the R function and wrap the return value in a
      # ContentToolResult. The raw R return value is stored in @value.
      browser() # BROWSER 6: invoke_tools -- about to call tool, inspect `request`
      result <- invoke_tool(request, otel_span = otel_span)

      # Guard: async tools return promises, which don't work in the
      # synchronous chat path.
      if (promises::is.promise(result@value)) {
        cli::cli_abort(
          c(
            "Can't use async tools with `$chat()` or `$stream()`.",
            i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
          ),
          class = "tool_async_error"
        )
      }

      # Print the result and fire the callback.
      maybe_echo_tool(result, echo = echo)
      on_tool_result(result)
      yield(result)
    }
  })

  # invoke_tools_async is intentionally *not* an _async_ generator, instead it
  # is a generator that returns promises. This lets the caller decide if the
  # tasks should be run in parallel or sequentially.
  invoke_tools_async <- coro::generator(function(
    turn,
    tools,
    echo = "none",
    on_tool_request = function(request) invisible(),
    on_tool_result = function(result) invisible(),
    yield_request = FALSE,
    otel_span = NULL
  ) {
    tool_requests <- extract_tool_requests(turn)

    invoke_tool_async_wrapper <- coro::async(function(request) {
      maybe_echo_tool(request, echo = echo)

      rejected <- coro::await(
        maybe_on_tool_request_async(request, on_tool_request)
      )
      if (!is.null(rejected)) {
        maybe_echo_tool(rejected, echo = echo)
        on_tool_result(rejected)
        return(rejected)
      }

      result <- coro::await(invoke_tool_async(request, otel_span = otel_span))

      maybe_echo_tool(result, echo = echo)
      on_tool_result(result)
      result
    })

    for (request in tool_requests) {
      if (yield_request) {
        yield(request)
      }
      yield(invoke_tool_async_wrapper(request))
    }
  })
})

gen_async_promise_all <- function(generator) {
  promises::promise_all(.list = coro::collect(generator))
}

# Filters a turn's contents to just the ContentToolRequest objects.
extract_tool_requests <- function(turn) {
  if (is.null(turn)) {
    return(NULL)
  }

  turn@contents[map_lgl(turn@contents, is_tool_request)]
}

# Returns TRUE if the turn contains at least one ContentToolRequest.
# Used in chat_impl() to decide whether to enter the tool-invocation
# branch after receiving an assistant response.
turn_has_tool_request <- function(turn) {
  if (is.null(turn)) {
    return(FALSE)
  }
  stopifnot(S7_inherits(turn, Turn))

  some(turn@contents, is_tool_request)
}

# Creates a ContentToolResult from either a successful result or an error.
# Handles three cases:
# 1. Error: wraps the error string/condition in a ContentToolResult
# 2. Already a ContentToolResult: the tool function returned one directly
#    (e.g. via tool_result()), just attach the request
# 3. Normal value: wrap the raw R return value in a new ContentToolResult
#
# NOTE: the raw R object is stored as-is in @value here. JSON conversion
# doesn't happen until much later in tool_string() (see #858).
new_tool_result <- function(request, result = NULL, error = NULL) {
  # Exactly one of result/error must be provided.
  check_exclusive(result, error)

  if (!is.null(error)) {
    ContentToolResult(error = error, request = request)
  } else if (is_tool_result(result)) {
    # Tool already returned a ContentToolResult; just set the request.
    set_props(result, request = request)
  } else {
    # Wrap the raw return value. This could be a string, data frame,
    # list, Content object, etc. No conversion happens here.
    ContentToolResult(value = result, request = request)
  }
}

# Invokes a single tool: calls the R function attached to the request
# with the arguments the LLM provided, and wraps the return value (or
# error) in a ContentToolResult.
#
# Also need to handle edge cases: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(request, otel_span = NULL) {
  # If the tool wasn't found in match_tools() (e.g. the LLM
  # hallucinated a tool name), return an error result.
  if (is.null(request@tool)) {
    return(new_tool_result(request, error = "Unknown tool"))
  }

  # Convert the LLM-provided arguments (JSON-decoded) to R values
  # using the tool's type specifications. If conversion fails (e.g.
  # extra arguments), this returns a ContentToolResult with an error.
  args <- tool_request_args(request)
  if (is_tool_result(args)) {
    # Failed to convert the arguments
    return(args)
  }

  # OpenTelemetry span for this individual tool invocation.
  tool_span <- local_tool_otel_span(request, parent = otel_span)

  tryCatch(
    {
      # Call the actual R function with the converted arguments.
      result <- do.call(request@tool, args)
      browser() # BROWSER 7: invoke_tool -- tool returned, inspect raw `result` and `args`
      # Wrap the raw return value in a ContentToolResult. The value is
      # stored as-is; JSON serialization happens later in tool_string().
      new_tool_result(request, result)
    },
    error = function(e) {
      record_tool_otel_span_error(tool_span, e)
      # Tool threw an error: wrap the condition in a ContentToolResult
      # so it gets reported back to the LLM as a tool failure.
      new_tool_result(request, error = e)
    }
  )
}

on_load(
  invoke_tool_async <- coro::async(function(request, otel_span = NULL) {
    if (is.null(request@tool)) {
      return(new_tool_result(request, error = "Unknown tool"))
    }

    args <- tool_request_args(request)
    if (is_tool_result(args)) {
      # Failed to convert the arguments
      return(args)
    }

    tool_span <- local_tool_otel_span(request, parent = otel_span)

    tryCatch(
      {
        result <- await(do.call(request@tool, args))
        new_tool_result(request, result)
      },
      error = function(e) {
        record_tool_otel_span_error(tool_span, e)
        new_tool_result(request, error = e)
      }
    )
  })
)

# Prepares arguments for calling the tool's R function. The LLM sends
# arguments as JSON, which gets decoded into a named list. If the tool
# has type specifications (tool@convert is TRUE), this function:
# 1. Checks for extra arguments the LLM invented (returns error result)
# 2. Converts each argument from its JSON representation to the
#    expected R type using the tool's TypeObject schema
# 3. Drops NULL arguments (unspecified optional params)
tool_request_args <- function(request) {
  tool <- request@tool
  # The LLM-provided arguments, already parsed from JSON to an R list.
  args <- request@arguments

  # If there's no type conversion needed, use the raw arguments as-is.
  if (is.null(tool) || !isTRUE(tool@convert)) {
    return(args)
  }

  # Check for arguments the LLM sent that don't match any parameter in
  # the tool's schema. This catches hallucinated parameter names.
  extra_args <- setdiff(names(args), names(tool@arguments@properties))
  if (length(extra_args) > 0) {
    e <- catch_cnd(cli::cli_abort("Unused argument{?s}: {extra_args}"))
    return(new_tool_result(request, error = e))
  }

  # Convert each argument to its expected R type (e.g. "integer" string
  # to actual integer) using the tool's TypeObject schema.
  args <- convert_from_type(args, tool@arguments)
  # Drop NULLs (optional parameters the LLM didn't provide).
  args[!map_lgl(args, is.null)]
}

# Runs the on_tool_request callback before invoking a tool. This gives
# users a chance to inspect or reject tool calls. If the callback calls
# tool_reject("reason"), it throws an ellmer_tool_reject condition,
# which we catch and convert to an error ContentToolResult.
#
# Returns NULL if the tool was accepted, or a ContentToolResult with
# an error message if rejected.
maybe_on_tool_request <- function(
  request,
  on_tool_request = function(request) invisible()
) {
  tryCatch(
    {
      on_tool_request(request)
      NULL # Tool accepted, proceed with invocation
    },
    ellmer_tool_reject = function(e) {
      # Tool was rejected by the callback; return an error result
      # instead of invoking the tool.
      ContentToolResult(error = e$message, request = request)
    }
  )
}

on_load(
  maybe_on_tool_request_async <- coro::async(
    function(request, on_tool_request = function(request) invisible()) {
      tryCatch(
        {
          coro::await(on_tool_request(request))
          NULL
        },
        ellmer_tool_reject = function(e) {
          ContentToolResult(error = e$message, request = request)
        }
      )
    }
  )
)

# Wraps a list of ContentToolResult objects into a UserTurn. This turn
# gets sent back to the LLM so it can see the results of its tool
# calls. Returns NULL if there are no results (which causes the tool
# loop in chat_impl() to exit).
tool_results_as_turn <- function(results) {
  if (length(results) == 0) {
    return(NULL)
  }
  is_tool_result <- map_lgl(results, is_tool_result)
  if (!any(is_tool_result)) {
    return(NULL)
  }
  UserTurn(contents = results[is_tool_result])
}

# Splits a turn's contents into tool results and everything else.
# Used by turn_contents_expand() to reorder contents so all tool
# results come before other content (required by some providers).
turn_split_tool_results <- function(turn) {
  is_result <- map_lgl(turn@contents, is_tool_result)
  list(
    tool_results = turn@contents[is_result],
    contents = turn@contents[!is_result]
  )
}

# Extracts all ContentToolResult objects that have errors from a turn.
# Used in chat_impl() to accumulate tool errors when echo="none" so
# they can be reported as a warning after the tool loop completes.
turn_get_tool_errors <- function(turn = NULL) {
  if (is.null(turn)) {
    return(NULL)
  }
  stopifnot(S7_inherits(turn, Turn))

  if (length(turn@contents) == 0) {
    return(NULL)
  }

  is_result <- map_lgl(turn@contents, S7_inherits, ContentToolResult)
  if (!any(is_result)) {
    return(NULL)
  }

  # Filter to just the results that have non-NULL @error.
  is_error <- map_lgl(turn@contents[is_result], tool_errored)

  res <- turn@contents[is_result][is_error]
  if (length(res)) res else NULL
}

# Issues a warning summarizing tool errors that occurred during the
# tool loop. Called via defer() in chat_impl() so it runs after the
# generator finishes, reporting up to 3 errors with tool names and
# messages.
warn_tool_errors <- function(tool_errors) {
  if (length(tool_errors) == 0) {
    return()
  }

  # Format up to 3 error messages for display.
  errs <- map_chr(
    tool_errors[seq_len(min(3, length(tool_errors)))],
    function(result) {
      name <- result@request@name %||% "unknown_tool"
      id <- result@request@id
      error <- tool_error_string(result)
      cli::format_inline("[{.field {name}} ({id})]: {cli_escape(error)}")
    }
  )

  cli::cli_warn(
    c(
      "Failed to evaluate {length(tool_errors)} tool call{?s}.",
      set_names(errs, "x"),
      "i" = if (length(errs) < length(tool_errors)) {
        cli::format_inline(
          "{cli::symbol$ellipsis} and {length(tool_errors) - length(errs)} more."
        )
      }
    ),
    class = "ellmer_tool_failure"
  )
}

# Prints a tool request or result to stdout when echo="output". This
# is what produces the blue "tool call" and green result lines the user
# sees during interactive chat. Only active when echo="output"; does
# nothing for "none" or "all" (which handle output differently).
maybe_echo_tool <- function(x, echo = "output") {
  if (!identical(echo, "output")) {
    return(invisible(x))
  }

  # Print tool requests as: "o [tool call] function_name(args...)"
  if (is_tool_request(x)) {
    cli::cli_text(
      cli::col_blue(cli::symbol$circle),
      " [{cli::col_blue('tool call')}] ",
      cli_escape(format(x, show = "call_short"))
    )
    return(invisible(x))
  }

  if (!is_tool_result(x)) {
    return(invisible(x))
  }

  # Print tool results: red stop icon for errors, green record icon for
  # success. Uses tool_string_preview() which calls tool_string() but
  # catches conversion errors gracefully.
  if (tool_errored(x)) {
    icon <- cli::col_red(cli::symbol$stop)
    header <- cli::col_red("Error: ")
    value <- tool_error_string(x)
  } else {
    icon <- cli::col_green(cli::symbol$record)
    header <- ""
    value <- tool_string_preview(x)
  }

  value <- cli::style_italic(value)

  if (grepl("\n", value)) {
    lines <- strsplit(value, "\n")[[1]]
    lines <- c(
      lines[seq_len(min(5, length(lines)))],
      if (length(lines) > 5) cli::symbol$ellipsis
    )
    lines <- cli::style_italic(lines)
    cli::cli_text("{icon} #> {header}{lines[1]}")
    for (line in lines[-1]) {
      cli::cli_text("\u00a0\u00a0#> {line}")
    }
  } else {
    max_width <- cli::console_width() - 7
    if (nchar(value) > max_width) {
      value <- substring(value, 1, max_width)
      value <- paste0(value, cli::symbol$ellipsis)
    }
    value <- cli::style_italic(value)
    cli::cli_text("{icon} #> {header}{value}")
  }

  invisible(x)
}
