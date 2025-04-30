test_that("capture_object creates an object with correct class", {
  obj <- capture_object(list(quote(mean(x)), quote(sum(x))))
  expect_s3_class(obj, "capture_object")
  expect_length(obj, 2)
})

test_that("Subsetting a capture_object preserves class", {
  obj <- capture_object(list(quote(mean(x)), quote(sum(x))))
  subsetted <- obj[1]
  expect_s3_class(subsetted, "capture_object")
  expect_length(subsetted, 1)
})

test_that("[<- for code_capture replaces elements correctly", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(quote(mean(x)), quote(sum(x))),
    pseudo = list()
  ), class = "code_capture")

  cap[1] <- quote(median(x))
  expect_equal(deparse(cap$expressions[[1]]), "median(x)")
})

test_that("print.code_capture prints summary correctly", {
  cap <- structure(list(
    capture_type = "block",
    expressions = list(quote(mean(x)), quote(sum(x))),
    pseudo = list()
  ), class = "code_capture")

  expect_output(print(cap), "== Code Capture ==")
  expect_output(print(cap), "Type:  block")
  expect_output(print(cap), "\\[1\\] mean\\(x\\)")
})
