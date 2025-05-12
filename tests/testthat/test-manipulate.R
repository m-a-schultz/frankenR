test_that("get_lhs and get_rhs work", {
  expr <- quote(a <- b + c)
  expect_equal(get_lhs(expr), "a")
  expect_equal(get_rhs(expr), quote(b + c))
})

test_that("set_lhs and set_rhs work", {
  expr <- quote(a <- 1)
  expr2 <- set_lhs(expr, "newvar")
  expr3 <- set_rhs(expr2, quote(2 + 3))
  expect_equal(get_lhs(expr2), "newvar")
  expect_equal(get_rhs(expr3), quote(2 + 3))
})

test_that("get_function and set_function work", {
  expr <- quote(foo(a, b))
  expect_equal(get_function(expr), as.name("foo"))
  expr2 <- set_function(expr, "bar")
  expect_equal(get_function(expr2), as.name("bar"))
})

test_that("get_arguments and get_argument_names work", {
  expr <- quote(fn(x = 1, 2, z = 3))
  args <- get_arguments(expr)
  expect_length(args, 3)
  expect_equal(get_argument_names(expr), c("x", "", "z"))
})

test_that("is_function and is_compound work", {
  expr1 <- quote(mean(x))
  expr2 <- quote(mean(x+2))
  expr3 <- quote(5)
  expect_true(is_function(expr1))
  expect_true(is_function(expr2))
  expect_false(is_function(expr3))
  expect_false(is_compound(expr1))
  expect_true(is_compound(expr2))
  expect_false(is_compound(expr3))
})

test_that("is_assignment works", {
  cap <- capture({
    a <- b
    a = b
    a + b
  })
  expect_true(is_assignment(cap[[1]]))
  expect_true(is_assignment(cap[[2]]))
  expect_false(is_assignment(cap[[3]]))
})

test_that("has_operator and get_operator work", {
  expr1 <- quote(a + b)
  expr2 <- quote(a[1])
  expr3 <- quote(fun(a))
  expect_true(has_operator(expr1))
  expect_true(has_operator(expr2))
  expect_false(has_operator(expr3))

  expect_equal(get_operator(expr1), "+")
  expect_equal(get_operator(expr2), "[")
  expect_null(get_operator(expr3))
})

test_that("replace_variable works", {
  expr <- quote(a + b + c)
  mapping <- c(a = "x", b = "y")
  new_expr <- replace_variable(expr, mapping)
  expect_equal(new_expr, quote(x + y + c))
})

test_that("replace_function works", {
  expr <- quote(oldfunc(a, b))
  new_expr <- replace_function(expr, "oldfunc", "newfunc")
  expect_equal(new_expr, quote(newfunc(a, b)))
})

test_that("replace_operator works", {
  expr <- quote(a + b)
  new_expr <- replace_operator(expr, "+", "*")
  expect_equal(new_expr, quote(a * b))
})

test_that("substitute_symbols works", {
  expr <- quote(a + b + c)
  rename_map <- c(a = "x", b = "y")
  result <- substitute_symbols(expr, rename_map)
  expect_equal(result, quote(x + y + c))
})

test_that("scan_expr_tree works", {
  expr <- quote((a + b) * (c + d))
  results <- scan_expr_tree(expr, is.call)
  expect_true(all(vapply(results, is.call, logical(1))))
  expect_true(any(vapply(results, function(e) identical(e[[1]], as.name("+")), logical(1))))
})

test_that("replace_variable works with call and callobj", {
  expr <- quote(a + b + c)
  mapping <- c(a = "x", b = "y")

  # test with call
  result1 <- replace_variable(expr, mapping)
  expect_equal(result1, quote(x + y + c))

  # test with callobj
  callobj <- new_callobj(expr)
  result2 <- replace_variable(callobj, mapping)
  expect_s3_class(result2, "callobj")
  expect_equal(result2$expr, quote(x + y + c))
})

test_that("get_operator works with call and callobj", {
  expr <- quote(a + b)

  # call
  expect_equal(get_operator(expr), "+")

  # callobj
  callobj <- new_callobj(expr)
  expect_equal(get_operator(callobj), "+")
})

test_that("has_operator works with call and callobj", {
  expr_op <- quote(a * b)
  expr_fn <- quote(mean(a, b))

  # call
  expect_true(has_operator(expr_op))
  expect_false(has_operator(expr_fn))

  # callobj
  callobj_op <- new_callobj(expr_op)
  callobj_fn <- new_callobj(expr_fn)
  expect_true(has_operator(callobj_op))
  expect_false(has_operator(callobj_fn))
})

test_that("substitute_symbols works for list and char vector mappings", {
  expr <- quote(a + b + c)
  rename_list <- list(a = "x", b = "y")
  rename_vector <- c(a = "x", b = "y")

  result1 <- substitute_symbols(expr, rename_list)
  result2 <- substitute_symbols(expr, rename_vector)

  expect_equal(result1, quote(x + y + c))
  expect_equal(result2, quote(x + y + c))
})
