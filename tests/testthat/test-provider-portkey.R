# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_portkey_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_portkey_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_portkey_test
  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_portkey_test
  test_data_extraction(chat_fun)
})

test_that("can use images", {
  # Needs mini to get shape correct
  chat_fun <- \(...) chat_portkey_test(model = "gpt-4.1-mini", ...)

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

# Provider specifics ------------------------------------------------------

test_that("virtual_key is deprecated", {
  expect_snapshot(chat <- chat_portkey(model = "def", virtual_key = "abc"))
  expect_equal(chat$get_provider()@model, "@abc/def")
})

test_that("structured output fails with clear error for Anthropic models", {
  chat <- chat_portkey(
    model = "@vertexai/anthropic.claude-opus-4-6",
    credentials = function() "fake-key"
  )
  type <- type_object(x = type_string())
  expect_error(
    chat$chat_structured("test", type = type),
    "Structured output is not supported for Anthropic models"
  )

  # Direct Anthropic provider routing should also error
  chat2 <- chat_portkey(
    model = "@anthropic/claude-3-5-sonnet",
    credentials = function() "fake-key"
  )
  expect_error(
    chat2$chat_structured("test", type = type),
    "Structured output is not supported for Anthropic models"
  )
})

test_that("non-Anthropic models are not blocked from structured output", {
  provider <- ProviderPortkeyAI(
    name = "PortkeyAI",
    base_url = "https://api.portkey.ai/v1",
    model = "@vertexai/gemini-2.5-flash-lite",
    params = params(),
    credentials = function() "fake-key"
  )
  type <- type_object(x = type_string())
  # Should not throw an Anthropic-specific error (builds the body without error)
  body <- chat_body(provider, stream = FALSE, turns = list(), type = type)
  expect_true(!is.null(body$response_format))
})
