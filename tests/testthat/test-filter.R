test_that("filter_by_function keeps only matching function calls", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(
      quote(mean(x)),
      quote(sum(x)),
      quote(median(x))
    ),
    pseudo = list()
  ), class = "code_capture")

  filtered <- filter_by_function(cap, fn_names = c("mean", "median"))

  expect_s3_class(filtered, "code_capture")
  expect_length(filtered$expressions, 2)
  expect_true(all(sapply(filtered$expressions, function(e) as.character(get_function_name(e)) %in% c("mean", "median"))))
})

test_that("filter_by_predicate selects expressions correctly", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(
      quote(x <- 1),
      quote(sum(x)),
      quote(y <- 2)
    ),
    pseudo = list()
  ), class = "code_capture")

  is_assignment_pred <- function(expr) is_assignment(expr)

  filtered <- filter_by_predicate(cap, predicate = is_assignment_pred)

  expect_s3_class(filtered, "code_capture")
  expect_length(filtered$expressions, 2)
  expect_true(all(is_assignment(filtered$expressions)))
})

test_that("is_assignment detects assignment expressions", {
  expect_true(is_assignment(quote(x <- 1)))
  expect_false(is_assignment(quote(mean(x))))
})

