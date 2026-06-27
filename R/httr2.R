# Builds and performs the HTTP request to the LLM API. This is the
# bridge between the R-side conversation state and the actual network
# call. Not generic -- provider-specific behavior is handled by
# chat_request() and chat_body() which are S7 generics.
#
# The `turns` list includes the full conversation history plus the new
# user turn. chat_request() calls chat_body() which calls as_json()
# on each turn -- and that's where tool_string() runs to convert
# ContentToolResult values to JSON/strings for the API.
chat_perform <- function(
  provider,
  mode = c("value", "stream", "async-stream", "async-value"),
  turns, # Full conversation: all prior turns + the new user turn
  tools = NULL, # Named list of Tool objects (NULL during data extraction)
  type = NULL, # Type spec for structured data extraction
  otel_span = NULL,
  controller = NULL # Stream controller for cancellation
) {
  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")
  tools <- tools %||% list()

  setup_active_promise_otel_span(otel_span)

  # Build the httr2 request. This is where the entire conversation
  # (including tool results) gets serialized to JSON via chat_body()
  # and the provider's as_json() methods.
  browser() # BROWSER 9: chat_perform -- about to build request, inspect `turns` and `tools`
  req <- chat_request(
    provider = provider,
    turns = turns,
    tools = tools,
    stream = stream,
    type = type
  )

  # Perform the request in the appropriate mode.
  switch(
    mode,
    "value" = req_perform(req),
    "stream" = chat_perform_stream(
      provider,
      req,
      controller = controller,
      otel_span = otel_span
    ),
    "async-value" = req_perform_promise(req),
    "async-stream" = chat_perform_async_stream(
      provider,
      req,
      controller = controller,
      otel_span = otel_span
    )
  )
}

on_load(
  chat_perform_stream <- coro::generator(function(
    provider,
    req,
    controller = NULL,
    otel_span = NULL
  ) {
    setup_active_promise_otel_span(otel_span)

    resp <- req_perform_connection(req)
    on.exit(close(resp))

    repeat {
      if (!is.null(controller) && controller$cancelled) {
        break
      }
      event <- chat_resp_stream(provider, resp)
      data <- stream_parse(provider, event)
      if (is.null(data)) {
        break
      } else {
        yield(data)
      }
    }
  })
)

on_load(
  chat_perform_async_stream <- coro::async_generator(function(
    provider,
    req,
    controller = NULL,
    otel_span = NULL
  ) {
    setup_active_promise_otel_span(otel_span)

    resp <- req_perform_connection(req, blocking = FALSE)
    on.exit(close(resp))

    repeat {
      if (!is.null(controller) && controller$cancelled) {
        break
      }
      event <- chat_resp_stream(provider, resp)
      if (is.null(event) && !resp_stream_is_complete(resp)) {
        fds <- resp$body$get_fdset()
        await(promises::promise(function(resolve, reject) {
          later::later_fd(
            resolve,
            fds$reads,
            fds$writes,
            fds$exceptions,
            fds$timeout
          )
        }))
        next
      }

      data <- stream_parse(provider, event)
      if (is.null(data)) {
        break
      } else {
        yield(data)
      }
    }
  })
)

# Request helpers --------------------------------------------------------------

ellmer_req_robustify <- function(req, is_transient = NULL, after = NULL) {
  req <- req_timeout(req, getOption("ellmer_timeout_s", 5 * 60))

  req <- req_retry(
    req,
    max_tries = getOption("ellmer_max_tries", 3),
    is_transient = is_transient,
    after = after,
    retry_on_failure = TRUE
  )

  req
}

ellmer_req_user_agent <- function(req, override = "") {
  ua <- if (identical(override, "")) ellmer_user_agent() else override
  req_user_agent(req, ua)
}
ellmer_user_agent <- function() {
  paste0("r-ellmer/", utils::packageVersion("ellmer"))
}
transform_user_agent <- function(x) {
  gsub(ellmer_user_agent(), "<ellmer_user_agent>", x, fixed = TRUE)
}
