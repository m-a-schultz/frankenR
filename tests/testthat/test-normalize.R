test_that("normalize_call adds missing argument names correctly", {
  expr <- quote(mean(x, 0.1))
  normalized <- normalize_call(expr)

  args <- as.list(normalized)[-1]
  expect_named(args, c("x", ""))
})

test_that("normalize_call handles non-existent functions gracefully", {
  expr <- call("nonexistent_fn", 1, 2)
  normalized <- normalize_call(expr)

  # Should leave the call unchanged
  expect_equal(deparse(normalized), deparse(expr))
})

test_that("normalize_calls normalizes nested calls recursively", {
  expr <- quote(mean(sum(1, 2), 0.1))
  normalized <- normalize_calls(expr)

  top_fn <- get_function_name(normalized)
  inner_fn <- get_function_name(normalized[[2]])

  expect_equal(as.character(top_fn), "mean")
  expect_equal(as.character(inner_fn), "sum")

  args_outer <- names(as.list(normalized)[-1])
  args_inner <- names(as.list(normalized[[2]])[-1])

  expect_true("x" %in% args_outer)
  expect_true("..." %in% args_inner || is.null(args_inner))  # sum usually uses "..."
})

