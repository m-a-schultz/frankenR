test_that("realize_args evaluates arguments but keeps function", {
  expr <- quote(sum(1 + 1, 2 + 2))
  realized <- realize_args(expr)

  args <- as.list(realized)[-1]
  expect_equal(args[[1]], 2)
  expect_equal(args[[2]], 4)
  expect_equal(get_function_name(realized), as.name("sum"))
})

test_that("realize_args keeps unevaluable arguments intact", {
  expr <- quote(sum(x, 2))
  realized <- realize_args(expr)

  args <- as.list(realized)[-1]
  expect_true(is.symbol(args[[1]]))  # x should remain unevaluated
  expect_equal(args[[2]], 2)
})

test_that("realize_capture realizes all expressions in capture", {
  cap <- format_capture(
    capture_type = "block",
    expr = list(quote(sum(1 + 1, 2 + 2))),
    meta = list()
  )

  realized <- realize_capture(cap)

  expect_s3_class(realized, "code_capture")
  args <- as.list(get_expressions(realized)[[1]])[-1]
  expect_equal(args[[1]], 2)
  expect_equal(args[[2]], 4)
})
