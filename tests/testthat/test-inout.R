test_that("get_inputs identifies variables used before definition", {
  cap <- capture({
    y <- x + 1
    x <- 2
  })

  inputs <- get_inputs(cap)
  expect_true(inputs$x$used_before_definition)
})
