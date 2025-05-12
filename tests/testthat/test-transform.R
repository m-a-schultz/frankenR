test_that("standardize_assignments converts = to <-", {
  cap <- format_capture(
    expr = list(
      call("=", as.name("a"), 5),
      call("<-", as.name("b"), 10)
    ),
    capture_type = "test"
  )

  result <- standardize_assignments(cap)
  exprs <- get_expressions(result)
  ops <- vapply(exprs, function(e) as.character(e[[1]]), character(1))

  expect_true(all(ops == "<-"))
})

test_that("remove_redundant_assignments removes x <- x", {
  cap <- format_capture(
    expr = list(
      call("<-", as.name("a"), as.name("a")),   # redundant
      call("<-", as.name("b"), 5),              # valid
      call("print", as.name("b"))
    ),
    capture_type = "test"
  )

  result <- remove_redundant_assignments(cap)
  exprs <- get_expressions(result)

  expr_text <- vapply(exprs, function(e) paste(deparse(e), collapse = ""), character(1))
  expect_false(any(grepl("a <- a", expr_text)))
  expect_true(any(grepl("b <- 5", expr_text)))
  expect_true(any(grepl("print\\(b\\)", expr_text)))
})

test_that("simplify_capture removes unused constants", {
  cap <- format_capture(
    expr = list(
      call("<-", as.name("a"), 5),
      call("<-", as.name("b"), 10),
      call("<-", as.name("c"), as.name("b")),  # uses b, not a
      call("print", as.name("c"))
    ),
    capture_type = "test"
  )

  simplified <- simplify_capture(cap)
  exprs <- get_expressions(simplified)
  expr_text <- vapply(exprs, function(e) paste(deparse(e), collapse = ""), character(1))

  # Should have removed 'a <- 5' (unused), kept others
  expect_false(any(grepl("a <- 5", expr_text)))
  expect_true(any(grepl("b <- 10", expr_text)))
  expect_true(any(grepl("c <- b", expr_text)))
  expect_true(any(grepl("print\\(c\\)", expr_text)))
})


test_that("export_capture handles meta='code'", {
  cap <- capture({ x <- 1; meta(._note = "Important"); y <- x + 1 })
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  export_capture(cap, path = tmp, meta = "code", overwrite = TRUE)
  contents <- readLines(tmp)
  expect_true(any(grepl("meta\\(.*\\)", contents)))
})

test_that("immutabilize_capture rewrites reassigned vars distinctly", {
  cap <- capture({ x <- 1; x <- x + 1; y <- x + 1 })
  out <- immutabilize_capture(cap)
  exprs <- get_expressions(out)
  names <- vapply(exprs[1:2], function(e) as.character(e[[2]]), character(1))
  expect_equal(names, c("x", "x._1"))
})

test_that("compress_redundant_versions rewrites simple var chains", {
  cap <- capture({ x <- 1; x._1 <- x+1; x._2 <- x._1*2; y <- x._2 + 1 })
  out <- compress_redundant_versions(cap)
  code <- get_expr_text(out)
  expect_false(any(grepl("x._1", code)))
  expect_false(any(grepl("x._2", code)))
})

test_that("isolate_capture injects input assignments", {
  ext <- 42
  cap <- capture({ y <- ext + 1 })
  iso <- isolate_capture(cap)
  exprs <- get_expr_text(iso)
  expect_true(any(grepl("ext <- 42", exprs)))
})


test_that("normalize_capture and accepts_arg work correctly", {
  cap <- capture({ mean(x, 0.1) })
  normed <- normalize_capture(cap)
  expect_true("x" %in% names(get_arguments(get_expressions(normed)[[1]])))
  expect_true(accepts_arg("mean.default", "trim"))
})

test_that("immutabilize_capture + compress_redundant_versions work together", {
  cap <- capture({
    a <- 1
    a <- a + 1
    a <- a + 2
    a <- a + 3
  })


  immut <- immutabilize_capture(cap)
  compressed <- compress_redundant_versions(immut)
  exprs <- get_expressions(compressed)

  # Should have compressed chains where possible
  expect_true(all(grepl("^a(\\._\\d+)?$", as.character(vapply(exprs, get_lhs, character(1))))))
})
