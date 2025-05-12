test_that("capture_block captures expressions inside a block", {
  cap <- capture({
    { mean(x); sum(x) }
  })

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "block")
  expect_length(get_expressions(cap), 1)
  expect_equal(get_function_name(get_expressions(cap)[[1]]), as.name("{"))
})
test_that("capture_block captures expressions inside a block", {
  cap <- capture({
     mean(x); sum(x)
  })

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "block")
  expect_length(get_expressions(cap), 2)
  expect_equal(get_function_name(get_expressions(cap)[[1]]), as.name("mean"))
})


test_that("format_capture wraps calls into a code_capture object", {
  expr <- quote(mean(x))
  cap <- format_capture(expr, capture_type = "test")

  expect_s3_class(cap, "code_capture")
  expect_equal(cap$capture_type, "test")
  expect_equal(length(get_expressions(cap)), 1)
})

test_that("process_expressions attaches metadata correctly", {
  exprs <- list(
    quote(mean(x)),
    call("meta", ._label = "First call"),
    quote(sum(x))
  )
  calls <- process_expressions(exprs)

  expect_equal(length(calls), 2)
  expect_equal(calls[[1]]$meta$._label, "First call")
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
  expect_length(get_expressions(block_cap), 3)

  expect_equal(get_metadata(block_cap)[[2]]$._info, "calculation")
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
  expect_length(get_expressions(script_cap), 3)

  expect_equal(get_metadata(script_cap)[[2]]$._desc, "square")
})

