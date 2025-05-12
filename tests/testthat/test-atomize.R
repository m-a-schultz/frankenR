test_that("atomize_expr fully atomizes nested expressions", {
  expr <- quote(sum(mean(x), sd(y)))
  atomized <- atomize_expr(expr)

  # Should create intermediate assignments
  expect_true(any(vapply(atomized, function(e) is_assignment(e), logical(1))))
  expect_true(any(grepl("sum", deparse(atomized[[length(atomized)]]))))
})

test_that("atomize_expr_with_counter returns updated counter", {
  expr <- quote(sum(mean(x), sd(y)))
  result <- atomize_expr_with_counter(expr, counter = 5)

  expect_equal(result$counter, 7)  # Two temporaries created
  expect_length(result$expressions, 3)
})

test_that("atomize_capture fully atomizes all expressions in capture", {
  cap <- format_capture(
    capture_type = "block",
    expr = list(quote(sum(mean(x), sd(y)))),
    meta = list()
  )

  atomized <- atomize_capture(cap)
  expect_s3_class(atomized, "code_capture")
  expect_true(length(get_expressions(atomized)) > 1)
})

test_that("atomize_selective_expr only atomizes specified functions", {
  expr <- quote(sum(mean(x), sd(y)))
  atomized <- atomize_selective_expr(expr, fn_names = "mean")

  # Should only atomize mean(), not sd()
  assigned_fns <- vapply(atomized, function(e) if (is.call(e)) as.character(e[[1]]) else NA, character(1))
  expect_true(any(assigned_fns == "<-"))
  expect_true(grepl("sum", deparse(atomized[[length(atomized)]])))
})

test_that("atomize_selective_expr_with_counter updates counter correctly", {
  expr <- quote(sum(mean(x), sd(y)))
  result <- atomize_selective_expr_with_counter(expr, fn_names = "mean", counter = 10)

  expect_equal(result$counter, 11)
  expect_length(result$expressions, 2)
})

test_that("atomize_selective_capture selectively atomizes a code_capture", {
  cap <- format_capture(
    capture_type = "block",
    expr = list(quote(sum(mean(x), sd(y)))),
    meta = list()
  )

  atomized <- atomize_selective_capture(cap, fn_names = "mean")
  expect_s3_class(atomized, "code_capture")
  expect_true(length(get_expressions(atomized)) > 1)
})

