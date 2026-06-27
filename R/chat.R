#' @include utils-coro.R
NULL

#' The Chat object
#'
#' @description
#' A `Chat` is a sequence of user and assistant [Turn]s sent
#' to a specific [Provider]. A `Chat` is a mutable R6 object that takes care of
#' managing the state associated with the chat; i.e. it records the messages
#' that you send to the server, and the messages that you receive back.
#' If you register a tool (i.e. an R function that the assistant can call on
#' your behalf), it also takes care of the tool loop.
#'
#' You should generally not create this object yourself,
#' but instead call [chat_openai()] or friends instead.
#'
#' @return A Chat object
#' @examples
#' \dontshow{ellmer:::vcr_example_start("Chat")}
#' chat <- chat_openai()
#' chat$chat("Tell me a funny joke")
#' \dontshow{ellmer:::vcr_example_end()}
Chat <- R6::R6Class(
  "Chat",
  public = list(
    #' @param provider A provider object.
    #' @param system_prompt System prompt to start the conversation with.
    #' @param echo One of the following options:
    #'   * `none`: don't emit any output (default when running in a function).
    #'   * `output`: echo text and tool-calling output as it streams in (default
    #'     when running at the console).
    #'   * `all`: echo all input and output.
    #'
    #'  Note this only affects the `chat()` method. You can override the default
    #'  by setting the `ellmer_echo` option.
    initialize = function(provider, system_prompt = NULL, echo = "none") {
      private$provider <- provider
      private$echo <- echo
      private$callback_on_tool_request <- CallbackManager$new(args = "request")
      private$callback_on_tool_result <- CallbackManager$new(args = "result")
      self$set_system_prompt(system_prompt)
    },

    #' @description Retrieve the turns that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   turns (if any exists).
    get_turns = function(include_system_prompt = FALSE) {
      if (length(private$.turns) == 0) {
        return(private$.turns)
      }

      if (!include_system_prompt && is_system_turn(private$.turns[[1]])) {
        private$.turns[-1]
      } else {
        private$.turns
      }
    },

    #' @description Replace existing turns with a new list.
    #' @param value A list of [Turn]s.
    set_turns = function(value) {
      private$.turns <- normalize_turns(
        value,
        self$get_system_prompt(),
        overwrite = TRUE
      )
      invisible(self)
    },

    #' @description Add a pair of turns to the chat.
    #' @param user The user [Turn].
    #' @param assistant The system [Turn].
    #' @param log_tokens Should tokens used in the turn be logged to the
    #'   session counter?
    add_turn = function(user, assistant, log_tokens = TRUE) {
      check_turn(user)
      check_turn(assistant)

      if (log_tokens) {
        log_turn(private$provider, assistant)
      }

      private$.turns[[length(private$.turns) + 1]] <- user
      private$.turns[[length(private$.turns) + 1]] <- assistant
      invisible(self)
    },

    #' @description If set, the system prompt, it not, `NULL`.
    get_system_prompt = function() {
      if (private$has_system_prompt()) {
        private$.turns[[1]]@text
      } else {
        NULL
      }
    },

    #' @description Retrieve the model name
    get_model = function() {
      private$provider@model
    },

    #' @description Update the model name. Note that unlike some of the
    #'   `chat_*()` functions, the model name is not validated against available
    #'   models for the provider.
    #' @param model A single string giving the new model name.
    set_model = function(model) {
      check_string(model)
      private$provider@model <- model
      invisible(self)
    },

    #' @description Update the system prompt
    #' @param value A character vector giving the new system prompt
    set_system_prompt = function(value) {
      check_character(value, allow_null = TRUE)
      if (length(value) > 1) {
        value <- paste(value, collapse = "\n\n")
      }

      # Remove prompt, if present
      if (private$has_system_prompt()) {
        private$.turns <- private$.turns[-1]
      }
      # Add prompt, if new
      if (is.character(value)) {
        system_turn <- SystemTurn(value)
        private$.turns <- c(list(system_turn), private$.turns)
      }
      invisible(self)
    },

    #' @description A data frame with token usage and cost data. There are four
    #'   columns: `input`, `output`, `cached_input`, and `cost`. There is one
    #'   row for each assistant turn, because token counts and costs are only
    #'   available when the API returns the assistant's response.
    #' @param include_system_prompt `r lifecycle::badge("deprecated")`
    get_tokens = function(include_system_prompt = deprecated()) {
      if (lifecycle::is_present(include_system_prompt)) {
        lifecycle::deprecate_warn(
          "0.4.0",
          "get_tokens(include_system_prompt)",
          "get_tokens()"
        )
      }

      turns <- self$get_turns()
      assistant_turns <- keep(turns, is_assistant_turn)
      complete_turns <- discard(assistant_turns, is_partial_turn)
      tokens <- map_tokens(complete_turns, \(turn) turn@tokens)
      tokens <- tibble::as_tibble(tokens)
      tokens$cost <- dollars(map_dbl(complete_turns, \(turn) turn@cost))

      user_turns <- keep(turns, is_user_turn)
      tokens$input_preview <- map_chr(user_turns, turn_contents_preview)
      tokens
    },

    #' @description The cost of this chat
    #' @param include The default, `"all"`, gives the total cumulative cost
    #'   of this chat. Alternatively, use `"last"` to get the cost of just the
    #'   most recent turn. Incomplete turns (from cancelled or interrupted
    #'   streams) are excluded because they lack token data.
    get_cost = function(include = c("all", "last")) {
      include <- arg_match(include)

      turns <- self$get_turns()
      assistant_turns <- keep(turns, is_assistant_turn)
      complete_turns <- discard(assistant_turns, is_partial_turn)

      if (length(complete_turns) == 0) {
        return(dollars(0))
      }

      if (include == "last") {
        cost <- complete_turns[[length(complete_turns)]]@cost
      } else {
        cost <- sum(map_dbl(complete_turns, \(turn) turn@cost))
      }

      dollars(cost)
    },

    #' @description The last turn returned by the assistant.
    #' @param role Optionally, specify a role to find the last turn with
    #'   for the role.
    #' @return Either a `Turn` or `NULL`, if no turns with the specified
    #'   role have occurred.
    last_turn = function(role = c("assistant", "user", "system")) {
      role <- arg_match(role)

      n <- length(private$.turns)
      switch(
        role,
        system = if (private$has_system_prompt()) private$.turns[[1]],
        assistant = if (n > 1) private$.turns[[n]],
        user = if (n > 1) private$.turns[[n - 1]]
      )
    },

    #' @description Submit input to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images
    #'   (see [content_image_file()] and [content_image_url()].
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(..., echo = NULL) {
      # If the last assistant turn requested tool calls that were never
      # answered (e.g. because a previous chat() was interrupted), create
      # error ContentToolResults for each one so the API contract is
      # satisfied: every tool request must have a corresponding result.
      finish_tools <- private$complete_dangling_tool_requests()

      # Build a UserTurn from the user's input (...) plus any dangling tool
      # results (spliced in via !!!). This single turn is what gets sent to
      # the LLM first.
      turn <- user_turn(!!!finish_tools, ...)
      browser() # BROWSER 1: chat() entry -- inspect `turn` and `finish_tools`
      # Resolve echo: use the argument if provided, otherwise fall back to
      # the default set when the Chat object was created.
      echo <- check_echo(echo %||% private$echo)

      # chat_impl() is a generator that yields text chunks as they arrive.
      # coro::collect() exhausts the generator, driving the full tool loop
      # to completion (possibly multiple LLM round-trips). We discard the
      # collected chunks here since we only need the final turn text.
      coro::collect(private$chat_impl(
        turn,
        stream = echo != "none",
        echo = echo,
        controller = stream_controller()
      ))

      # Extract the final assistant response text and return it. When
      # echoing is on, the text was already printed, so return invisibly.
      text <- ellmer_output(self$last_turn()@text)
      if (echo == "none") text else invisible(text)
    },

    #' @description Extract structured data.
    #'
    #' Note: tool calling is disabled during structured data extraction. See
    #' `vignette("structured-data")` for details and workarounds.
    #' @param ... The input to send to the chatbot. This is typically the text
    #'   you want to extract data from, but it can be omitted if the data is
    #'   obvious from the existing conversation.
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'   all providers).
    #' @param convert Automatically convert from JSON lists to R data types
    #'   using the schema. For example, this will turn arrays of objects into
    #'   data frames and arrays of strings into a character vector.
    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ..., .check_empty = FALSE)
      echo <- check_echo(echo %||% private$echo)
      check_bool(convert)

      needs_wrapper <- type_needs_wrapper(type, private$provider)
      type <- wrap_type_if_needed(type, needs_wrapper)

      stream <- echo != "none" &&
        !uses_tool_structured_output(private$provider, type)

      coro::collect(private$submit_turns(
        turn,
        type = type,
        stream = stream,
        echo = echo,
        controller = stream_controller()
      ))

      turn <- self$last_turn()
      extract_data(turn, type, convert = convert, needs_wrapper = needs_wrapper)
    },

    #' @description Extract structured data, asynchronously. Returns a promise
    #'   that resolves to an object matching the type specification.
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'   all providers).
    #' @param convert Automatically convert from JSON lists to R data types
    #'   using the schema. For example, this will turn arrays of objects into
    #'   data frames and arrays of strings into a character vector.
    chat_structured_async = function(..., type, echo = "none", convert = TRUE) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ..., .check_empty = FALSE)
      echo <- check_echo(echo %||% private$echo)
      check_bool(convert)

      needs_wrapper <- type_needs_wrapper(type, private$provider)
      type <- wrap_type_if_needed(type, needs_wrapper)

      stream <- echo != "none" &&
        !uses_tool_structured_output(private$provider, type)

      done <- coro::async_collect(private$submit_turns_async(
        turn,
        type = type,
        stream = stream,
        echo = echo,
        controller = stream_controller()
      ))

      promises::then(done, function(dummy) {
        turn <- self$last_turn()
        extract_data(
          turn,
          type,
          convert = convert,
          needs_wrapper = needs_wrapper
        )
      })
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once. Returns a promise that resolves
    #'   to a string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param tool_mode Whether tools should be invoked one-at-a-time
    #'   (`"sequential"`) or concurrently (`"concurrent"`). Sequential mode is
    #'   best for interactive applications, especially when a tool may involve
    #'   an interactive user interface. Concurrent mode is the default and is
    #'   best suited for automated scripts or non-interactive applications.
    chat_async = function(..., tool_mode = c("concurrent", "sequential")) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      tool_mode <- arg_match(tool_mode)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(
          turn,
          stream = FALSE,
          echo = "none",
          tool_mode = tool_mode,
          controller = stream_controller()
        )
      )
      promises::then(done, function(dummy) {
        self$last_turn()@text
      })
    },

    #' @description Submit input to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    #' @param controller An optional [stream_controller()] used to cancel the
    #'   stream from outside the iteration loop.
    stream = function(..., stream = c("text", "content"), controller = NULL) {
      controller <- as_controller(controller)
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      stream <- arg_match(stream)
      private$chat_impl(
        turn,
        stream = TRUE,
        echo = "none",
        yield_as_content = stream == "content",
        controller = controller
      )
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param tool_mode Whether tools should be invoked one-at-a-time
    #'   (`"sequential"`) or concurrently (`"concurrent"`). Sequential mode is
    #'   best for interactive applications, especially when a tool may involve
    #'   an interactive user interface. Concurrent mode is the default and is
    #'   best suited for automated scripts or non-interactive applications.
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    #' @param controller An optional [stream_controller()] used to cancel the
    #'   stream from outside the iteration loop.
    stream_async = function(
      ...,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content"),
      controller = NULL
    ) {
      controller <- as_controller(controller)
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      tool_mode <- arg_match(tool_mode)
      stream <- arg_match(stream)
      private$chat_impl_async(
        turn,
        stream = TRUE,
        echo = "none",
        tool_mode = tool_mode,
        yield_as_content = stream == "content",
        controller = controller
      )
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   Learn more in `vignette("tool-calling")`.
    #' @param tool A tool definition created by [tool()].
    register_tool = function(tool) {
      check_tool(tool)
      if (has_name(private$tools, tool@name)) {
        cli::cli_inform("Replacing existing {tool@name} tool.")
      }

      private$tools[[tool@name]] <- tool
      invisible(self)
    },

    #' @description Register a list of tools.
    #'   Learn more in `vignette("tool-calling")`.
    #' @param tools A list of tool definitions created by [tool()].
    register_tools = function(tools) {
      check_tools(tools)

      for (tool in tools) {
        self$register_tool(tool)
      }
      invisible(self)
    },

    #' @description Get the underlying provider object. For expert use only.
    get_provider = function() {
      private$provider
    },

    #' @description Retrieve the list of registered tools.
    get_tools = function() {
      private$tools
    },

    #' @description Sets the available tools. For expert use only; most users
    #'   should use `register_tool()`.
    #'
    #' @param tools A list of tool definitions created with [ellmer::tool()].
    set_tools = function(tools) {
      check_tools(tools)

      private$tools <- list()
      for (tool_def in tools) {
        self$register_tool(tool_def)
      }
      invisible(self)
    },

    #' @description Register a callback for a tool request event.
    #'
    #' @param callback A function to be called when a tool request event occurs,
    #'   which must have `request` as its only argument.
    #'
    #' @return A function that can be called to remove the callback.
    on_tool_request = function(callback) {
      private$callback_on_tool_request$add(callback)
    },

    #' @description Register a callback for a tool result event.
    #'
    #' @param callback A function to be called when a tool result event occurs,
    #'   which must have `result` as its only argument.
    #'
    #' @return A function that can be called to remove the callback.
    on_tool_result = function(callback) {
      private$callback_on_tool_result$add(callback)
    }
  ),
  private = list(
    provider = NULL,

    .turns = list(),
    echo = NULL,
    tools = list(),
    callback_on_tool_request = NULL,
    callback_on_tool_result = NULL,

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    # The main tool loop. This is a coro generator that alternates between
    # sending turns to the LLM and invoking any tools the LLM requests.
    # The loop continues until the LLM responds without requesting any
    # tools (i.e. it gives a final text answer).
    #
    # Yields: text chunks (strings) or, when yield_as_content = TRUE,
    # Content objects (used by $stream() to emit structured content).
    chat_impl = generator_method(function(
      self,
      private,
      user_turn, # The initial UserTurn to send to the LLM
      stream, # TRUE to stream the response, FALSE to wait for the full response
      echo, # "none", "output", or "all" -- controls what gets printed to stdout
      yield_as_content = FALSE, # If TRUE, yield Content objects instead of text
      controller = NULL # Stream controller that allows cancellation
    ) {
      # Accumulate tool errors so we can warn about them all at once when
      # the generator finishes (via defer).
      tool_errors <- list()
      defer(warn_tool_errors(tool_errors))

      # OpenTelemetry span for the overall agent loop (covers all LLM
      # round-trips and tool calls in this chat_impl invocation).
      agent_span <- local_agent_otel_span(private$provider, activate = FALSE)

      # --- THE TOOL LOOP ---
      # Each iteration: (1) send user_turn to LLM, (2) check if the
      # assistant wants to call tools, (3) if so, invoke them and set
      # user_turn to the results for the next iteration. If no tools
      # requested, user_turn stays NULL and the loop ends.
      while (!is.null(user_turn)) {
        browser() # BROWSER 2: tool loop iteration -- inspect `user_turn`
        # (1) Send the user turn to the LLM and get back the assistant's
        # response. submit_turns() is itself a generator that yields
        # chunks (streaming) or the complete response (non-streaming).
        # It also appends both the user turn and the assistant turn to
        # private$.turns.
        assistant_chunks <- private$submit_turns(
          user_turn,
          stream = stream,
          echo = echo,
          yield_as_content = yield_as_content,
          controller = controller,
          otel_span = agent_span
        )
        # Pass through all chunks/text from submit_turns to our caller.
        for (chunk in assistant_chunks) {
          yield(chunk)
        }

        # Grab the assistant's completed turn (submit_turns stored it).
        assistant_turn <- self$last_turn()
        # Assume no more work to do unless we find tool requests below.
        user_turn <- NULL

        # Don't invoke tools if the stream was cancelled
        if (controller$cancelled) {
          break
        }

        # (2) Check if the assistant's response contains tool requests.
        browser() # BROWSER 3: after LLM response -- inspect `assistant_turn`
        if (turn_has_tool_request(assistant_turn)) {
          # (3) invoke_tools() is a generator that iterates over each
          # ContentToolRequest in the assistant turn, calls the
          # corresponding R function, and yields ContentToolResult
          # objects (and optionally ContentToolRequest objects when
          # yield_request = TRUE).
          tool_calls <- invoke_tools(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke,
            on_tool_result = private$callback_on_tool_result$invoke,
            yield_request = yield_as_content,
            otel_span = agent_span
          )

          # Collect all ContentToolResult objects from the generator.
          tool_results <- list()

          for (tool_step in tool_calls) {
            # When streaming content, forward each tool step (requests
            # and results) to our caller so they can display progress.
            if (yield_as_content) {
              yield(tool_step)
            }
            # Only keep actual results (not requests) for the next turn.
            if (is_tool_result(tool_step)) {
              tool_results <- c(tool_results, list(tool_step))
            }
          }

          # Wrap the tool results in a UserTurn so the loop sends them
          # back to the LLM on the next iteration. If there are results,
          # user_turn will be non-NULL and the while loop continues.
          user_turn <- tool_results_as_turn(tool_results)
          browser() # BROWSER 4: after tool invocation -- inspect `tool_results` and `user_turn`
        }

        # Print the tool results turn if echo="all", or silently collect
        # any tool errors to warn about later if echo="none".
        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
        }
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl_async = async_generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      tool_mode = "concurrent",
      yield_as_content = FALSE,
      controller = NULL
    ) {
      tool_errors <- list()
      defer(warn_tool_errors(tool_errors))

      agent_span <- local_agent_otel_span(private$provider, activate = FALSE)

      while (!is.null(user_turn)) {
        assistant_chunks <- private$submit_turns_async(
          user_turn,
          stream = stream,
          echo = echo,
          yield_as_content = yield_as_content,
          controller = controller,
          otel_span = agent_span
        )
        for (chunk in await_each(assistant_chunks)) {
          yield(chunk)
        }

        assistant_turn <- self$last_turn()
        user_turn <- NULL

        # Don't invoke tools if the stream was cancelled
        if (controller$cancelled) {
          break
        }

        if (turn_has_tool_request(assistant_turn)) {
          tool_calls <- invoke_tools_async(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke_async,
            on_tool_result = private$callback_on_tool_result$invoke_async,
            yield_request = yield_as_content,
            otel_span = agent_span
          )
          if (tool_mode == "sequential") {
            tool_results <- list()
            for (tool_step in await_each(tool_calls)) {
              if (yield_as_content) {
                yield(tool_step)
              }
              if (is_tool_result(tool_step)) {
                tool_results <- c(tool_results, list(tool_step))
              }
            }
          } else {
            tool_results <- coro::collect(tool_calls)
            if (yield_as_content) {
              # Filter out and yield tool requests before awaiting tool results
              is_request <- map_lgl(tool_results, is_tool_request)
              for (tool_step in tool_results[is_request]) {
                yield(tool_step)
              }
              tool_results <- tool_results[!is_request]
            }
            tool_results <- await(promises::promise_all(.list = tool_results))
            if (yield_as_content) {
              for (tool_result in tool_results) {
                yield(tool_result)
              }
            }
          }

          user_turn <- tool_results_as_turn(tool_results)
        }

        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
        }
      }
    }),

    # Handles a single LLM round-trip: sends the user turn to the API and
    # processes the response. Appends both the user turn and the resulting
    # assistant turn to private$.turns.
    #
    # Yields: text chunks (streaming) or the complete text (non-streaming),
    # or Content objects when yield_as_content = TRUE.
    submit_turns = generator_method(function(
      self,
      private,
      user_turn, # The UserTurn to send (user message or tool results)
      stream, # TRUE = streaming mode, FALSE = wait for complete response
      echo, # "none", "output", or "all"
      type = NULL, # Type spec for structured data extraction (disables tools)
      yield_as_content = FALSE, # If TRUE, yield Content objects instead of text
      controller = NULL, # Stream controller for cancellation
      otel_span = NULL # Parent OpenTelemetry span
    ) {
      # When echo="all", print the user turn to stdout so the user can
      # see what's being sent (prefixed with "> ").
      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      # Set up OpenTelemetry tracing for this specific LLM call.
      otel_input <- otel_chat_input(private, user_turn)
      chat_span <- local_chat_otel_span(
        private$provider,
        turns = otel_input$turns,
        system_prompt = otel_input$system_prompt,
        parent = otel_span
      )

      # Make the actual HTTP request to the LLM API. chat_perform() calls
      # chat_request() (which builds the httr2 request with the serialized
      # body via chat_body()) then performs it. In streaming mode, returns
      # a generator of chunks; in value mode, returns the complete response.
      browser() # BROWSER 5: submit_turns -- about to call LLM API, inspect `user_turn` and `private$.turns`
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        # All conversation turns so far plus the new user turn
        turns = c(private$.turns, list(user_turn)),
        # Only send tools if we're not doing structured data extraction
        tools = if (is.null(type)) private$tools,
        type = type,
        controller = controller,
        otel_span = chat_span
      )

      # emit() is a function that prints text to stdout based on the echo
      # setting ("none" = no-op, "output"/"all" = cat the text).
      emit <- emitter(echo)
      # Track whether we've yielded any text (for trailing newline logic).
      any_text <- FALSE
      turn <- NULL
      # TurnAccumulator manages building up the assistant Turn object from
      # streaming chunks and appending turns to private$.turns.
      acc <- TurnAccumulator$new(self, private, controller)

      if (stream) {
        # Record the user turn in the conversation history. finalize_turn()
        # runs on exit to ensure the assistant turn is always saved even if
        # the generator is interrupted mid-stream.
        acc$begin_turn(user_turn)
        on.exit(acc$finalize_turn(), add = TRUE)

        # Iterate over streaming chunks from the API.
        result <- NULL
        for (chunk in response) {
          # Parse the raw chunk into a Content object (e.g. ContentText).
          content <- stream_content(private$provider, chunk)
          if (!is.null(content)) {
            # Extract text from the content for printing/yielding.
            text <- content_text(content)
            emit(text)
            yield(if (yield_as_content) content else text)
            # Feed the content into the accumulator to build the Turn.
            acc$update_turn(content)
            any_text <- TRUE
          }

          # Merge raw API chunks together to build the complete result
          # (needed for token counts, finish reason, etc.).
          result <- stream_merge_chunks(private$provider, result, chunk)
        }

        record_chat_otel_span_status(chat_span, private$provider, result)
        # Finalize the assistant Turn from the accumulated chunks.
        turn <- acc$complete_turn(result, type = type)
        record_chat_otel_span_output(chat_span, turn)
      } else {
        # Non-streaming: parse the complete JSON response body.
        result <- resp_body_json(response)
        duration <- resp_timing(response)[["total"]] %||% NA_real_
        record_chat_otel_span_status(chat_span, private$provider, result)
        # Build and store both the user and assistant turns at once.
        turn <- acc$add_turn(user_turn, result, duration, type = type)
        record_chat_otel_span_output(chat_span, turn)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
          any_text <- TRUE
        }
      }

      if (!is.null(turn) && !is_partial_turn(turn)) {
        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          if (yield_as_content) {
            yield(ContentText("\n"))
          } else {
            yield("\n")
          }
        }

        if (echo == "all") {
          echo_non_text_contents(turn)
        }
        # When `echo="output"`, tool calls are emitted in `invoke_tools()`
      }

      coro::exhausted()
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns_async = async_generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      type = NULL,
      yield_as_content = FALSE,
      controller = NULL,
      otel_span = NULL
    ) {
      otel_input <- otel_chat_input(private, user_turn)
      chat_span <- local_chat_otel_span(
        private$provider,
        turns = otel_input$turns,
        system_prompt = otel_input$system_prompt,
        parent = otel_span
      )

      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        turns = c(private$.turns, list(user_turn)),
        tools = if (is.null(type)) private$tools,
        type = type,
        controller = controller,
        otel_span = chat_span
      )

      emit <- emitter(echo)
      any_text <- FALSE
      turn <- NULL
      acc <- TurnAccumulator$new(self, private, controller)

      if (stream) {
        acc$begin_turn(user_turn)
        on.exit(acc$finalize_turn(), add = TRUE)

        result <- NULL
        for (chunk in await_each(response)) {
          content <- stream_content(private$provider, chunk)
          if (!is.null(content)) {
            text <- content_text(content)
            emit(text)
            yield(if (yield_as_content) content else text)
            acc$update_turn(content)
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }

        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$complete_turn(result, type = type)
        record_chat_otel_span_output(chat_span, turn)
      } else {
        response <- await(response)
        result <- resp_body_json(response)
        duration <- resp_timing(response)[["total"]] %||% NA_real_
        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$add_turn(user_turn, result, duration, type = type)
        record_chat_otel_span_output(chat_span, turn)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
          any_text <- TRUE
        }
      }

      if (!is.null(turn) && !is_partial_turn(turn)) {
        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          if (yield_as_content) {
            yield(ContentText("\n"))
          } else {
            yield("\n")
          }
        }

        if (echo == "all") {
          echo_non_text_contents(turn)
        }
        # When `echo="output"`, tool calls are echoed via `invoke_tools_async()`
      }
      coro::exhausted()
    }),

    has_system_prompt = function() {
      length(private$.turns) > 0 && is_system_turn(private$.turns[[1]])
    },

    # Checks if the last turn in the conversation is an assistant turn
    # with unanswered tool requests ("dangling" requests). This happens
    # when a previous chat() call was interrupted or cancelled before the
    # tool loop could run. LLM APIs require every tool request to have a
    # matching tool result, so we create error results for each one.
    #
    # Returns NULL if there are no dangling requests, or a list of
    # ContentToolResult objects with error messages that get spliced into
    # the next user turn.
    complete_dangling_tool_requests = function() {
      if (length(private$.turns) == 0) {
        return(NULL)
      }

      last_turn <- private$.turns[[length(private$.turns)]]
      # Only assistant turns can have tool requests.
      if (last_turn@role != "assistant") {
        return(NULL)
      }

      # Extract any ContentToolRequest objects from the turn's contents.
      tool_requests <- keep(last_turn@contents, is_tool_request)
      if (length(tool_requests) == 0) {
        return(NULL)
      }

      # Create an error ContentToolResult for each unanswered request.
      lapply(tool_requests, function(req) {
        ContentToolResult(
          error = "Chat ended before the tool could be invoked.",
          request = req
        )
      })
    }
  )
)

#' @export
print.Chat <- function(x, ...) {
  provider <- x$get_provider()
  turns <- x$get_turns(include_system_prompt = TRUE)

  assistant_turns <- keep(turns, \(x) x@role == "assistant")
  complete_turns <- discard(assistant_turns, is_partial_turn)
  total_tokens <- colSums(map_tokens(complete_turns, \(x) x@tokens))
  total_cost <- sum(map_dbl(complete_turns, \(x) x@cost))

  cat(paste_c(
    "<Chat",
    c(" ", provider@name, "/", provider@model),
    c(" turns=", length(turns)),
    turn_cost(total_tokens, total_cost, prefix = " "),
    ">\n"
  ))

  for (i in seq_along(turns)) {
    turn <- turns[[i]]
    if (is_partial_turn(turn)) {
      label <- paste0(" [", turn@reason, "]")
    } else if (turn@role == "assistant") {
      label <- turn_cost(turn@tokens, turn@cost, prefix = " [", suffix = "]")
    } else {
      label <- ""
    }

    cli::cat_rule(cli::format_inline("{color_role(turn@role)}{label}"))
    cat(format(turns[[i]]))
  }

  invisible(x)
}

turn_cost <- function(tokens, cost, prefix, suffix = "") {
  out <- paste0(prefix, "input=")

  if (!is.na(tokens[[3]]) && tokens[[3]] > 0) {
    out <- paste0(out, tokens[[1]], "+", tokens[[3]])
  } else {
    out <- paste0(out, tokens[[1]])
  }
  out <- paste0(out, " output=", tokens[[2]])

  if (!is.na(cost)) {
    out <- paste0(out, " cost=", format(dollars(cost)))
  }
  out <- paste0(out, suffix)
  out
}

TurnAccumulator <- R6::R6Class(
  "TurnAccumulator",
  public = list(
    chat = NULL,
    chat_private = NULL,
    provider = NULL,
    controller = NULL,
    turn_idx = NULL,
    start_time = NULL,

    initialize = function(chat, chat_private, controller) {
      self$chat <- chat
      self$chat_private <- chat_private
      self$provider <- chat$get_provider()
      self$controller <- controller
    },

    begin_turn = function(user_turn) {
      self$chat$add_turn(user_turn, AssistantPartialTurn(), log_tokens = FALSE)
      self$turn_idx <- length(self$chat_private$.turns)
      self$start_time <- proc.time()[["elapsed"]]
      invisible(self)
    },

    update_turn = function(content) {
      idx <- self$turn_idx
      turn <- self$chat_private$.turns[[idx]]
      turn@contents <- c(turn@contents, list(content))
      self$chat_private$.turns[[idx]] <- turn
      invisible(self)
    },

    complete_turn = function(result, type = NULL) {
      if (self$controller$cancelled) {
        return(invisible(self))
      }
      duration <- proc.time()[["elapsed"]] - self$start_time
      turn <- self$value_turn(result, type, duration = duration)
      self$chat_private$.turns[[self$turn_idx]] <- turn
      # log_turn() is called manually here because the streaming path
      # replaces a partial turn in-place rather than using Chat$add_turn(),
      # which handles logging automatically for the non-streaming path.
      log_turn(self$provider, turn)
      turn
    },

    finalize_turn = function() {
      idx <- self$turn_idx
      if (is.null(idx)) {
        return(invisible())
      }
      turn <- self$chat_private$.turns[[idx]]
      if (!is_partial_turn(turn)) {
        return(invisible())
      }
      turn@contents <- merge_content_text(turn@contents)
      turn@reason <- self$controller$reason %||% "interrupted"
      turn@duration <- proc.time()[["elapsed"]] - self$start_time
      self$chat_private$.turns[[idx]] <- turn
      log_turn(self$provider, turn)
    },

    add_turn = function(user_turn, result, duration = NA_real_, type = NULL) {
      turn <- self$value_turn(result, type, duration = duration)
      self$chat$add_turn(user_turn, turn)
      turn
    },

    value_turn = function(result, type, duration = NA_real_) {
      # Check before value_turn() so structured extraction errors before
      # trying to parse truncated JSON
      finish_reason <- value_finish_reason(self$provider, result)
      check_finish_reason(finish_reason, if (is.null(type)) "warn" else "error")

      turn <- value_turn(
        self$provider,
        result,
        has_type = !is.null(type)
      )
      turn@duration <- duration
      match_tools(turn, self$chat$get_tools())
    }
  )
)

echo_non_text_contents <- function(turn) {
  is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
  formatted <- map_chr(turn@contents[!is_text], format)
  cat_line(formatted, prefix = "< ")
}

merge_content_text <- function(contents) {
  reduce(contents, .init = list(), function(acc, item) {
    n <- length(acc)
    if (n > 0 && every(list(acc[[n]], item), S7_inherits, ContentText)) {
      acc[[n]] <- ContentText(paste0(acc[[n]]@text, item@text))
    } else {
      acc <- c(acc, list(item))
    }
    acc
  })
}
method(contents_markdown, new_S3_class("Chat")) <- function(
  content,
  heading_level = 2
) {
  turns <- content$get_turns()
  if (length(turns) == 0) {
    return("")
  }

  hh <- strrep("#", heading_level)

  res <- vector("character", length(turns))
  for (i in seq_along(res)) {
    role <- turns[[i]]@role
    substr(role, 0, 1) <- toupper(substr(role, 0, 1))
    res[i] <- glue::glue("{hh} {role}\n\n{contents_markdown(turns[[i]])}")
  }

  paste(res, collapse = "\n\n")
}
