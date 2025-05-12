test_that("+.callobj adds or modifies arguments correctly", {
  expr <- new_callobj(quote(mean(x)))

  modified <- expr + list(trim = 0.1)
  expect_equal(modified['trim'], 0.1)

  expr2 <- new_callobj(quote(sum(x)))
  appended <- expr2 + 5
  expect_true(any(sapply(as.list(appended), function(x) x == 5)))
})

test_that("-.callobj removes arguments correctly by name", {
  expr <- new_callobj( quote(mean(x, trim = 0.1)))
  modified <- expr - "trim"
  expect_null(modified$trim)
})

test_that("-.callobj removes unnamed arguments by position", {
  expr <- new_callobj(quote(c(1, 2, 3)))
  modified <- expr - 2
  unnamed_args <- as.list(modified)[-1]
  expect_equal(length(unnamed_args), 2)
  expect_equal(unlist(unnamed_args), c(1, 3))
})

test_that("[.call extracts arguments correctly", {
  expr <- new_callobj(quote(mean(x, trim = 0.1)))
  expect_equal(expr["trim"], 0.1)
  expr2 <- new_callobj(quote(c(10, 20)))
  expect_equal(expr2[2], 20)
})

test_that("accepts_arg handles non-existent functions gracefully", {
  expect_false(accepts_arg("nonexistent_function", "arg"))
})
