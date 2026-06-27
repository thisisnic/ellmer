# Most LLM APIs only support text/string content in tool results. But
# ellmer tools can return rich Content objects like images. This file
# handles "expanding" such results: the tool result itself becomes a
# text pointer ("See <tool-content> below"), and the actual content is
# placed as separate items in the user turn.
#
# Example transformation:
#   ContentToolResult(value = ContentImageInline(...))
#   ->
#   ContentToolResult("See <content-tool> below")
#   ContentText("<content-tool>")
#   ContentImageInline(...)
#   ContentText("</content-tool>")
#
# This runs inside the provider's as_json(Turn) method, just before
# the turn is serialized for the API request.

# Processes a turn's contents, expanding any tool results that contain
# Content objects (like images) into the pointer + content format
# described above. Also reorders so tool results come before other
# content items (required by some providers like Anthropic).
turn_contents_expand <- function(turn) {
  if (length(turn@contents) == 0) {
    # Early return to avoid unlist(list()) yielding NULL
    return(turn)
  }

  # Expand each content item. Non-tool-results pass through unchanged.
  # Tool results with Content values get expanded to multiple items.
  contents <- map(turn@contents, expand_content_if_needed)
  turn@contents <- unlist(contents, recursive = FALSE)
  # Reorder: all tool results first, then other content items.
  turn_contents <- turn_split_tool_results(turn)
  turn@contents <- c(turn_contents$tool_results, turn_contents$contents)
  turn
}

# Decides whether a content item needs expansion.
# - Non-tool-results: no expansion needed, return as-is in a list.
# - Tool results with a single Content value: expand to pointer + content.
# - Tool results with a list of Content values: expand with multiple items.
# - Tool results with plain values (strings, data, etc.): no expansion.
expand_content_if_needed <- function(content) {
  if (!is_tool_result(content)) {
    return(list(content))
  }

  request <- content@request
  value <- content@value

  if (S7_inherits(value, Content)) {
    # Single Content object (e.g. ContentImageInline)
    expand_tool_value(request, value)
  } else if (is.list(value)) {
    if (all(map_lgl(value, \(x) S7_inherits(x, Content)))) {
      # List of Content objects
      expand_tool_values(request, value)
    } else {
      # List of non-Content items (plain R objects); leave as-is
      list(content)
    }
  } else {
    # Plain value (string, number, etc.); leave as-is
    list(content)
  }
}

# Expands a tool result with a single Content value into:
# 1. A ContentToolResult with a text pointer as its value
# 2. An opening XML tag as ContentText
# 3. The actual Content object (e.g. an image)
# 4. A closing XML tag as ContentText
expand_tool_value <- function(request, value) {
  open <- sprintf('<tool-content call-id="%s">', request@id)
  list(
    ContentToolResult(
      value = sprintf("See %s below.", open),
      request = request
    ),
    ContentText(open),
    value,
    ContentText("</tool-content>")
  )
}

# Same as expand_tool_value but for multiple Content objects. Wraps
# each one in its own <tool-content> tags within an outer
# <tool-contents> container.
expand_tool_values <- function(request, values) {
  open <- sprintf('<tool-contents call-id="%s">', request@id)
  result <- ContentToolResult(
    value = sprintf('See %s below.', open),
    request = request
  )

  contents <- map(values, function(value) {
    list(ContentText("<tool-content>"), value, ContentText("</tool-content>"))
  })
  contents <- unlist(contents, recursive = FALSE)

  open <- ContentText(open)
  close <- ContentText("</tool-contents>")
  c(list(result, open), contents, list(close))
}
