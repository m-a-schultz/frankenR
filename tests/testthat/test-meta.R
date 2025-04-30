test_that("meta does nothing and returns NULL invisibly", {
  result <- meta(a = 1, b = 2)
  expect_null(result)
})
