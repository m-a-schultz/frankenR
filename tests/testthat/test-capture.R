test_that("capture_block captures expressions inside a block", {
  cap <- capture({
    { mean(x); sum(x) }
  })

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "block")
  expect_length(cap$expressions, 1)
  expect_equal(get_function_name(cap$expressions[[1]]), as.name("{"))
})
test_that("capture_block captures expressions inside a block", {
  cap <- capture({
     mean(x); sum(x)
  })

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "block")
  expect_length(cap$expressions, 2)
  expect_equal(get_function_name(cap$expressions[[1]]), as.name("mean"))
})


test_that("format_capture wraps calls into a code_capture object", {
  expr <- quote(mean(x))
  cap <- format_capture(expr, capture_type = "test")

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "test")
  expect_equal(length(cap$expressions), 1)
})

test_that("process_expressions attaches metadata correctly", {
  exprs <- list(
    quote(mean(x)),
    call("meta", ._label = "First call"),
    quote(sum(x))
  )
  calls <- process_expressions(exprs)

  expect_equal(length(calls), 2)
  expect_equal(calls[[1]]$meta$label, "First call")
})

test_that("capture_call creates a call capture with empty pseudo", {
  captured <- capture_call(quote(mean(x)))

  expect_true(is.list(captured))
  expect_true(is.call(captured$expr))
  expect_equal(captured$meta, list())
})

test_that("capture_end errors if no active capture session", {
  # Simulate no session active
  .capture_store$active <- FALSE
  expect_error(capture_end(), "No active capture session")
})


test_that("Block capture works", {
  block_cap <- capture({
    x <- 10
    y <- x + 5
    meta(._info = "calculation")
    z <- y * 2
  })

  expect_s3_class(block_cap, "code_capture")
  expect_equal(block_cap$capture_type, "block")
  expect_length(block_cap$expressions, 3)

  expect_equal(block_cap$meta[[2]]$info, "calculation")
})

test_that("Script capture works", {
  tmp_script <- tempfile(fileext = ".R")
  writeLines(c(
    "x <- 5 #@ ._comment = 'First'",
    "y <- x^2",
    "meta(._desc = 'square')",
    "z <- y + 1"
  ), con = tmp_script)

  on.exit(unlink(tmp_script))

  script_cap <- capture(script = tmp_script)
  expect_s3_class(script_cap, "code_capture")
  expect_equal(script_cap$capture_type, "script")
  expect_length(script_cap$expressions, 3)

  expect_equal(script_cap$meta[[2]]$._desc, "square")
})

