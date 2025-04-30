test_that("+.call adds or modifies arguments correctly", {
  expr <- quote(mean(x))
  class(expr) <- 'callobj'
  modified <- expr + list(trim = 0.1)
  expect_equal(modified$trim, 0.1)

  expr2 <- quote(sum(x))
  class(expr2) <- 'callobj'
  appended <- expr2 + 5
  expect_true(any(sapply(as.list(appended), function(x) x == 5)))
})

test_that("-.call removes arguments correctly by name", {
  expr <- quote(mean(x, trim = 0.1))
  class(expr) <- 'callobj'
  modified <- expr - "trim"
  expect_null(modified$trim)
})

test_that("-.call removes unnamed arguments by position", {
  expr <- quote(c(1, 2, 3))
  class(expr) <- 'callobj'
  modified <- expr - 2
  unnamed_args <- as.list(modified)[-1]
  expect_equal(length(unnamed_args), 2)
  expect_equal(unlist(unnamed_args), c(1, 3))
})

test_that("[.call extracts arguments correctly", {
  expr <- quote(mean(x, trim = 0.1))
  class(expr) <- 'callobj'
  expect_equal(expr["trim"], 0.1)
  expr2 <- quote(c(10, 20))
  class(expr2) <- 'callobj'
  expect_equal(expr2[2], 20)
})

test_that("$.call extracts arguments by name (shorthand)", {
  expr <- quote(mean(x, trim = 0.1))
  class(expr) <- 'callobj'
  expect_equal(expr$trim, 0.1)
})
