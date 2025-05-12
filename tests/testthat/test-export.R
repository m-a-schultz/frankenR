test_that("export_capture writes metadata as comments", {
  cap <- format_capture(expr = list(quote(x <- 1)),
                        meta = list(list(._note = "Important")))

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  export_capture(cap, path = tmp, meta = "comments", overwrite = TRUE)
  contents <- readLines(tmp)
  expect_true(any(grepl("# ._note = \"Important\"", contents)))
})

test_that("export_capture writes metadata as comments", {
  cap <- format_capture(expr = list(quote(x <- 1)),
                        meta = list(list(._note = "Important")))

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  export_capture(cap, path = tmp, meta = "code", overwrite = TRUE)
  contents <- readLines(tmp)
  expect_true(any(grepl('meta\\( ._note = "Important" \\)', contents)))
})

test_that("export_capture writes metadata as comments", {
  cap <- capture({
    x <- 1
    meta(._note = "Important")
  })

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  export_capture(cap, path = tmp, meta = "comments", overwrite = TRUE)
  contents <- readLines(tmp)
  expect_true(any(grepl('# ._note = "Important"', contents)))
})

test_that("export_capture writes metadata as code", {
  cap <- capture({
    x <- 1
    meta(._note = "Important")
  })

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  export_capture(cap, path = tmp, meta = "code", overwrite = TRUE)
  contents <- readLines(tmp)
  expect_true(any(grepl('meta\\( ._note = "Important" \\)', contents)))
})
