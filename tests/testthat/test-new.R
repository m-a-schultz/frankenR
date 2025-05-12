test_that("process_capture respects evaluate and realize flags", {
  cap <- capture({ x <- 1 + 1; y <- x + 1 })
  env <- new.env()
  res <- process_capture(cap, realize = TRUE, evaluate = TRUE, envir = env)
  expect_equal(env$x, 2)
  expect_equal(env$y, 3)
})

test_that("format_capture and update_capture work as expected", {
  calls <- list(new_callobj(quote(mean(x)), list(label = "A")))
  cap <- format_capture(calls, capture_type = "block")
  expect_equal(get_metadata(cap)[[1]]$label, "A")
  cap2 <- update_capture(cap, capture_type = "test")
  expect_equal(cap2$capture_type, "test")
})


test_that("[[ and [[<- for code_capture access expressions", {
  cap <- capture({ a <- 1; b <- 2 })
  expr <- cap[[1]]
  expect_equal(as.character(expr$expr[[2]]), "a")
  cap[[1]] <- new_callobj(quote(a <- 100))
  expect_equal(as.character(cap[[1]]$expr[[3]]), "100")
})

test_that("get_top_function_names returns expected output", {
  cap <- capture({ mean(x); sum(y) })
  fns <- get_top_function_names(cap)
  expect_equal(fns, c("mean", "sum"))
})

test_that("null-coalescing operator %||% works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(4 %||% 5, 4)
})

test_that("verify_capture handles failed rerun correctly", {
  cap <- capture({ stop("fail") })
  result <- verify_capture(cap, details = TRUE)
  expect_true("missing_in_capture_result" %in% names(result))
})

