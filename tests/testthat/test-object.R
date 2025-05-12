test_that("[[<- for code_capture replaces elements correctly", {
  cap <- format_capture(expr = list(quote(mean(x)), quote(sum(x))),
    meta = list()
  )

  cap[[1]]$expr <- quote(median(x))
  expect_equal(deparse(get_expressions(cap)[[1]]), "median(x)")
})

test_that("print.code_capture prints summary correctly", {
  cap <-  format_capture(expr = list(quote(mean(x)), quote(sum(x))),
                         meta = list(),
                         capture_type='block')

  expect_output(print(cap), "== Code Capture ==")
  expect_output(print(cap), "\\[1\\] mean\\(x\\)")
})

test_that("update_capture modifies expressions and metadata", {
  cap <- capture({
    x <- 1
  })

  new_expr <- quote(y <- 2)
  new_meta <- list(list(label = "Updated"))
  updated_cap <- update_capture(cap, expr = list(new_expr), meta = new_meta)

  exprs <- get_expressions(updated_cap)
  meta <- get_metadata(updated_cap)

  expect_equal(deparse(exprs[[1]]), "y <- 2")
  expect_equal(meta[[1]]$label, "Updated")
})
