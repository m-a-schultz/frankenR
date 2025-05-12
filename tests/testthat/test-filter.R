test_that("filter_by_function keeps only matching function calls", {
  cap <- format_capture(
    capture_type = "block",
    expr = list(
      quote(mean(x)),
      quote(sum(x)),
      quote(median(x))
    ),
    meta = list()
  )

  filtered <- filter_by_function(cap, fn_names = c("mean", "median"))

  expect_s3_class(filtered, "code_capture")
  expect_length(get_expressions(filtered), 2)
  expect_true(all(sapply(filtered$expressions, function(e) as.character(get_function_name(e)) %in% c("mean", "median"))))
})

test_that("filter_by_predicate selects expressions correctly", {
  cap <- format_capture(
    capture_type = "block",
    expr = list(
      quote(x <- 1),
      quote(sum(x)),
      quote(y <- 2)
    )  )

  filtered <- filter_by_predicate(cap, predicate = is_assignment)

  expect_s3_class(filtered, "code_capture")
  expect_length(get_expressions(filtered), 2)
  expect_true(all(sapply(get_expressions(filtered),is_assignment)))
})

test_that("is_assignment detects assignment expressions", {
  expect_true(is_assignment(quote(x <- 1)))
  expect_false(is_assignment(quote(mean(x))))
})

