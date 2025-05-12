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

test_that("sort_capture reorders expressions correctly", {
  cap <- capture({
    y <- x + 1
    f <- function(z) z * 2
    z <- f(y)
  })

  sorted <- sort_capture(cap)
  sorted_exprs <- get_expressions(sorted)

  # Check that function definitions come first
  expect_true(any(grepl("function", deparse(sorted_exprs[[1]]))))
})

test_that("duplicate_line duplicates expressions correctly", {
  cap <- capture({
    a <- 1
    b <- a + 2
  })

  cap_dup <- duplicate_line(cap, 1)
  exprs <- get_expressions(cap_dup)

  expect_equal(length(exprs), 3)
  expect_equal(deparse(exprs[[1]]), deparse(exprs[[2]]))
})

library(testthat)

test_that("modify.R functions work with call and callobj", {
  # Base call
  expr <- call("plot", quote(x), quote(y))
  call_obj <- new_callobj(expr, meta = list(tag = "test"))

  # ---- set_arg ----
  modified_call <- set_arg(expr, "main", "Title")
  expect_true(is.call(modified_call))
  expect_equal(modified_call$main, "Title")

  modified_obj <- set_arg(call_obj, "main", "Title")
  expect_s3_class(modified_obj, "callobj")
  expect_equal(modified_obj$meta$tag, "test")
  expect_equal(modified_obj$expr$main, "Title")

  # ---- change_arg ----
  changed_call <- change_arg(modified_call, "main", "New Title")
  expect_equal(changed_call$main, "New Title")

  changed_obj <- change_arg(modified_obj, "main", "New Title")
  expect_equal(changed_obj$expr$main, "New Title")
  expect_equal(changed_obj$meta$tag, "test")

  # ---- remove_arg ----
  removed_call <- remove_arg(changed_call, "main")
  expect_null(removed_call$main)

  removed_obj <- remove_arg(changed_obj, "main")
  expect_null(removed_obj$expr$main)
  expect_equal(removed_obj$meta$tag, "test")

  # ---- add_arg ----
  added_call <- add_arg(expr, value = 42, name = "cex")
  expect_equal(added_call$cex, 42)

  added_obj <- add_arg(call_obj, value = 42, name = "cex")
  expect_equal(added_obj$expr$cex, 42)
  expect_equal(added_obj$meta$tag, "test")

  # ---- change_func ----
  changed_func_call <- change_func(expr, "lines")
  expect_equal(as.character(changed_func_call[[1]]), "lines")

  changed_func_obj <- change_func(call_obj, "lines")
  expect_equal(as.character(changed_func_obj$expr[[1]]), "lines")
  expect_equal(changed_func_obj$meta$tag, "test")

  # ---- wrap_expr ----
  wrapped_call <- wrap_expr(expr, "invisible")
  expect_equal(as.character(wrapped_call[[1]]), "invisible")

  wrapped_obj <- wrap_expr(call_obj, "invisible")
  expect_equal(as.character(wrapped_obj$expr[[1]]), "invisible")
  expect_equal(wrapped_obj$meta$tag, "test")

  # ---- unwrap_expr ----
  inner_expr <- call("sqrt", 4)
  wrapped <- call("identity", inner_expr)
  call_obj2 <- new_callobj(wrapped, meta = list(note = "unwrap test"))

  unwrapped_call <- unwrap_expr(wrapped)
  expect_equal(unwrapped_call, inner_expr)

  unwrapped_obj <- unwrap_expr(call_obj2)
  expect_equal(unwrapped_obj$expr, inner_expr)
  expect_equal(unwrapped_obj$meta$note, "unwrap test")
})

