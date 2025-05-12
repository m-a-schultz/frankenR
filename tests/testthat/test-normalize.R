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



