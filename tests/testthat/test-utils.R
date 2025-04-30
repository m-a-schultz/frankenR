test_that("is_call_or_list detects calls and lists of calls", {
  expect_true(is_call_or_list(quote(mean(x))))
  expect_true(is_call_or_list(list(quote(mean(x)), quote(sum(x)))))
  expect_false(is_call_or_list(list(1, 2, 3)))
  expect_false(is_call_or_list("not a call"))
})

test_that("get_function_name extracts function names correctly", {
  expect_equal(get_function_name(quote(mean(x))), as.name("mean"))
  expect_null(get_function_name(42))
})

test_that("get_arguments extracts arguments correctly", {
  expr <- quote(mean(x, trim = 0.1))
  args <- get_arguments(expr)
  expect_equal(length(args), 2)
  expect_equal(names(args), c("", "trim"))
})

test_that("get_expr_text returns deparsed expressions", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(quote(mean(x)), quote(sum(x))),
    pseudo = list()
  ), class = "code_capture")

  text <- get_expr_text(cap)
  expect_true(any(grepl("mean\\(x\\)", text)))
  expect_true(any(grepl("sum\\(x\\)", text)))
})

test_that("get_all_arguments returns all arguments from capture", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(quote(mean(x, trim = 0.1)), quote(sum(x))),
    pseudo = list()
  ), class = "code_capture")

  args_list <- get_all_arguments(cap)
  expect_length(args_list, 2)
  expect_equal(names(args_list[[1]]), c("", "trim"))
})
