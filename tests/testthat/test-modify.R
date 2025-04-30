test_that("has_arg correctly identifies existing arguments", {
  expr <- quote(mean(x, trim = 0.1))
  expect_true(has_arg(expr, "trim"))
  expect_false(has_arg(expr, "na.rm"))
})

test_that("get_arg retrieves argument values correctly", {
  expr <- quote(mean(x, trim = 0.1))
  expect_equal(get_arg(expr, "trim"), 0.1)
  expect_null(get_arg(expr, "na.rm"))
})

test_that("set_arg sets or adds arguments correctly", {
  expr <- quote(mean(x))
  modified <- set_arg(expr, "trim", 0.2)
  expect_equal(get_arg(modified, "trim"), 0.2)
})

test_that("change_arg changes existing arguments and errors on missing ones", {
  expr <- quote(mean(x, trim = 0.1))
  changed <- change_arg(expr, "trim", 0.2)
  expect_equal(get_arg(changed, "trim"), 0.2)

  expect_error(change_arg(expr, "na.rm", TRUE), "Argument not found")
})

test_that("remove_arg removes arguments correctly", {
  expr <- quote(mean(x, trim = 0.1))
  removed <- remove_arg(expr, "trim")
  expect_null(get_arg(removed, "trim"))
})

test_that("add_arg appends unnamed and named arguments", {
  expr <- quote(sum(x))
  appended <- add_arg(expr, 5)
  unnamed_args <- as.list(appended)[-1]
  expect_true(5 %in% unnamed_args)

  expr_named <- quote(mean(x))
  appended_named <- add_arg(expr_named, value = 0.5, name = "trim")
  expect_equal(get_arg(appended_named, "trim"), 0.5)
})

test_that("add_arg inserts argument at specific position", {
  expr <- quote(sum(1, 2))
  inserted <- add_arg(expr, 100, position = 1)
  inserted_args <- as.list(inserted)[-1]
  expect_equal(inserted_args[[1]], 100)
})

test_that("change_func changes function correctly", {
  expr <- quote(mean(x))
  changed <- change_func(expr, "sum")
  expect_equal(get_function_name(changed), as.name("sum"))
})

test_that("wrap_expr wraps correctly", {
  expr <- quote(x + 1)
  wrapped <- wrap_expr(expr, "sqrt")
  expect_equal(get_function_name(wrapped), as.name("sqrt"))
})

test_that("unwrap_expr unwraps correctly", {
  expr <- quote(sqrt(x + 1))
  unwrapped <- unwrap_expr(expr)
  expect_true(is.call(unwrapped))
  expect_equal(get_function_name(unwrapped), as.name("+"))
})

