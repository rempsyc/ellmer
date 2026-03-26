test_that("can chat in parallel", {
  vcr::local_cassette("parallel-basic")

  chat <- chat_openai_test()
  chats <- parallel_chat(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  expect_type(chats, "list")
  expect_length(chats, 2)

  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "Chat")

  expect_equal(chats[[1]]$last_turn()@contents[[1]]@text, "2")
  expect_equal(chats[[2]]$last_turn()@contents[[1]]@text, "4")
})

test_that("can just get text parallel ", {
  vcr::local_cassette("parallel-basic")

  chat <- chat_openai_test()
  out <- parallel_chat_text(chat, list("What's 1 + 1?", "What's 2 + 2?"))
  expect_equal(out, c("2", "4"))
})

test_that("can call tools in parallel", {
  vcr::local_cassette("parallel-tool")

  prompts <- rep(list("Roll the dice, please! Reply with 'You rolled ____'"), 2)

  chat <- chat_openai_test()
  chat$register_tool(tool(
    counter(),
    name = "roll",
    description = "Rolls a six-sided die."
  ))
  chats <- parallel_chat(chat, prompts)

  turns_1 <- chats[[1]]$get_turns()
  expect_s3_class(turns_1[[2]]@contents[[1]], "ellmer::ContentToolRequest")
  expect_s3_class(turns_1[[3]]@contents[[1]], "ellmer::ContentToolResult")
  expect_equal(contents_text(turns_1[[4]]), "You rolled 1")

  turns_1 <- chats[[2]]$get_turns()
  expect_equal(contents_text(turns_1[[4]]), "You rolled 2")
})

test_that("can have uneven number of turns", {
  vcr::local_cassette("parallel-tool-uneven")

  prompts <- list(
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'boop'",
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'beep'"
  )

  chat <- chat_openai_test()
  chat$register_tool(tool(
    counter(),
    name = "roll",
    description = "Rolls a six-sided die."
  ))
  chats <- parallel_chat(chat, prompts)

  lengths <- map_int(chats, \(chat) length(chat$get_turns()))
  expect_equal(lengths, c(4, 2, 4, 2))

  text <- map_chr(chats, \(chat) chat$last_turn()@text)
  expect_equal(text, c("You rolled 1", "boop", "You rolled 2", "beep"))
})

# structured data --------------------------------------------------------------

test_that("can extract data in parallel", {
  vcr::local_cassette("parallel-data")

  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person
  )
  expect_equal(data, tibble::tibble(name = c("John", "Jane"), age = c(15, 16)))
})

test_that("can get tokens", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_tokens = TRUE
  )
  # These are pretty weak, but it's hard to know how to do better.
  expect_contains(names(data), c("input_tokens", "output_tokens"))
  expect_equal(data$input_tokens > 0, c(TRUE, TRUE))
  expect_equal(data$output_tokens > 0, c(TRUE, TRUE))
})

test_that("can get cost", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_cost = TRUE
  )
  expect_contains(names(data), "cost")
  expect_equal(data$cost > 0, c(TRUE, TRUE))
})

test_that("can get tokens & cost", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_cost = TRUE,
    include_tokens = TRUE
  )
  expect_contains(names(data), c("input_tokens", "output_tokens", "cost"))
})

test_that("parallel_chat logs tokens", {
  local_tokens()
  vcr::local_cassette("parallel-logging")

  chat <- chat_openai_test()
  prompts <- list("Say 1", "Say 2", "Say 3")
  results <- parallel_chat(chat, prompts)

  input_tokens <- map_dbl(results, \(x) x$get_tokens()$input)
  expect_equal(the$tokens$input, sum(input_tokens))
})

test_that("parallel_chat_structured logs tokens", {
  local_tokens()
  vcr::local_cassette("parallel-structured-logging")

  chat <- chat_openai_test()
  type <- type_object(msg = type_string("A greeting message"))
  prompts <- list("Say hi", "Say bye")
  results <- parallel_chat_structured(
    chat,
    prompts,
    type = type,
    include_tokens = TRUE,
    include_cost = TRUE
  )

  expect_equal(the$tokens$input, sum(results$input_tokens))
  expect_equal(the$tokens$output, sum(results$output_tokens))
})

# error handling ---------------------------------------------------------------

test_that("handles errors and NULLs in parallel functions", {
  chat <- chat_openai(
    credentials = \() "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )
  prompts <- list("prompt1", "prompt2", "prompt3")
  responses <- list(
    AssistantTurn("Success", tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    NULL
  )
  local_mocked_bindings(parallel_turns = function(...) responses)

  chats <- parallel_chat(chat, prompts)
  expect_length(chats, 3)
  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "error")
  expect_null(chats[[3]])

  expect_equal(parallel_chat_text(chat, prompts), c("Success", NA, NA))

  responses <- list(
    AssistantTurn(list(ContentJson(list(x = 1))), tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    NULL
  )
  type <- type_object(x = type_number())
  out <- parallel_chat_structured(chat, prompts, type)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c("x", ".error"))
  expect_equal(out$x, c(1, NA, NA))
})

test_that("errors in conversion become warnings", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  turns <- list(
    AssistantTurn(list(ContentJson(data = list(x = 1)))),
    # no json
    AssistantTurn(list(ContentText("Hello"))),
    # invalid json
    AssistantTurn(list(ContentJson(string = "{")))
  )

  expect_snapshot(out <- multi_convert(provider, turns, type = type))
  expect_equal(out, tibble::tibble(x = c(1, NA, NA)))
})

test_that("assistant turns track duration in parallel", {
  vcr::local_cassette("parallel-duration")

  chat <- chat_openai_test()
  chats <- parallel_chat(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  assistant_duration_1 <- chats[[1]]$last_turn()@duration
  assistant_duration_2 <- chats[[2]]$last_turn()@duration

  # These assistant durations are usually not NA, but are during replay (#479)
  expect_true(is.na(assistant_duration_1) || assistant_duration_1 > 0)
  expect_true(is.na(assistant_duration_2) || assistant_duration_2 > 0)
})

# multi-turn prompts -----------------------------------------------------------

test_that("as_conversation_suffixes handles single-turn prompts", {
  suffixes <- as_conversation_suffixes(list("hello", "world"))
  expect_length(suffixes, 2)
  expect_length(suffixes[[1]], 1)
  expect_length(suffixes[[2]], 1)
  expect_s7_class(suffixes[[1]][[1]], UserTurn)
  expect_s7_class(suffixes[[2]][[1]], UserTurn)
  expect_equal(suffixes[[1]][[1]]@text, "hello")
  expect_equal(suffixes[[2]][[1]]@text, "world")
})

test_that("as_conversation_suffixes handles multi-turn string lists", {
  suffixes <- as_conversation_suffixes(list(
    list("context A", "question A"),
    list("context B", "question B")
  ))
  expect_length(suffixes, 2)
  # Each suffix has two turns
  expect_length(suffixes[[1]], 2)
  expect_length(suffixes[[2]], 2)
  expect_s7_class(suffixes[[1]][[1]], UserTurn)
  expect_s7_class(suffixes[[1]][[2]], UserTurn)
  expect_equal(suffixes[[1]][[1]]@text, "context A")
  expect_equal(suffixes[[1]][[2]]@text, "question A")
  expect_equal(suffixes[[2]][[1]]@text, "context B")
  expect_equal(suffixes[[2]][[2]]@text, "question B")
})

test_that("as_conversation_suffixes handles explicit Turn objects", {
  turns <- list(UserTurn("turn1"), UserTurn("turn2"))
  suffixes <- as_conversation_suffixes(list(turns))
  expect_length(suffixes, 1)
  expect_length(suffixes[[1]], 2)
  expect_s7_class(suffixes[[1]][[1]], UserTurn)
  expect_equal(suffixes[[1]][[1]]@text, "turn1")
  expect_equal(suffixes[[1]][[2]]@text, "turn2")
})

test_that("as_conversation_suffixes keeps mixed Content lists as single turn", {
  img <- content_image_url("https://example.com/img.png")
  suffixes <- as_conversation_suffixes(list(list(img, "describe it")))
  expect_length(suffixes, 1)
  # Should be a single turn with 2 content items
  expect_length(suffixes[[1]], 1)
  expect_s7_class(suffixes[[1]][[1]], UserTurn)
  expect_length(suffixes[[1]][[1]]@contents, 2)
})

test_that("multi-turn parallel_chat builds correct conversations", {
  chat <- chat_openai(
    credentials = \() "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )

  prompts <- list(
    list("context 1", "question 1"),
    list("context 2", "question 2")
  )

  captured_conversations <- NULL
  local_mocked_bindings(
    parallel_turns = function(provider, conversations, ...) {
      captured_conversations <<- conversations
      list(
        AssistantTurn("answer 1", tokens = c(10, 10, 0)),
        AssistantTurn("answer 2", tokens = c(10, 10, 0))
      )
    }
  )

  chats <- parallel_chat(chat, prompts)

  # Each conversation should have system turn + 2 user turns
  expect_length(captured_conversations, 2)
  conv1_roles <- map_chr(captured_conversations[[1]], \(t) t@role)
  expect_equal(conv1_roles, c("system", "user", "user"))
  expect_equal(captured_conversations[[1]][[2]]@text, "context 1")
  expect_equal(captured_conversations[[1]][[3]]@text, "question 1")
  expect_equal(captured_conversations[[2]][[2]]@text, "context 2")
  expect_equal(captured_conversations[[2]][[3]]@text, "question 2")
})

test_that("multi-turn parallel_chat_structured builds correct conversations", {
  chat <- chat_openai(
    credentials = \() "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )

  prompts <- list(
    list("I am Democrat Joe, age 30.", "What is my name and age?"),
    list("I am Republican Jane, age 40.", "What is my name and age?")
  )

  captured_conversations <- NULL
  local_mocked_bindings(
    parallel_turns = function(provider, conversations, ...) {
      captured_conversations <<- conversations
      list(
        AssistantTurn(list(ContentJson(list(name = "Joe", age = 30))),
                      tokens = c(10, 10, 0)),
        AssistantTurn(list(ContentJson(list(name = "Jane", age = 40))),
                      tokens = c(10, 10, 0))
      )
    }
  )

  type <- type_object(name = type_string(), age = type_integer())
  out <- parallel_chat_structured(chat, prompts, type)

  # Each conversation should have system turn + 2 user turns
  expect_length(captured_conversations, 2)
  conv1_roles <- map_chr(captured_conversations[[1]], \(t) t@role)
  expect_equal(conv1_roles, c("system", "user", "user"))
  expect_equal(captured_conversations[[1]][[2]]@text, "I am Democrat Joe, age 30.")
  expect_equal(captured_conversations[[1]][[3]]@text, "What is my name and age?")

  # Output data
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$name, c("Joe", "Jane"))
  expect_equal(out$age, c(30L, 40L))
})
