test_that("diagnose_capture identifies stochastic elements", {
  cap <- capture({
    set.seed(123)
    x <- rnorm(5)
  })

  diag <- diagnose_capture(cap)
  expect_true(diag$stochastic)
  expect_true(diag$seed_used)
})

test_that("diagnose_capture detects global state usage", {
  global_var <- 10
  cap <- capture({
    y <- global_var + 5
  })

  diag <- diagnose_capture(cap)
  expect_true('global_var' %in% diag$external_vars)
})

test_that("diagnose_capture detects all conditions", {
  # 1. stochastic
  cap <- capture({ x <- runif(1) })
  diag <- diagnose_capture(cap)
  expect_true(diag$stochastic)

  # 2. seed_used
  cap <- capture({ set.seed(123) })
  diag <- diagnose_capture(cap)
  expect_true(diag$seed_used)

  # 3. external_vars
  y <- 5
  cap <- capture({ z <- y + 1 })
  diag <- diagnose_capture(cap)
  expect_true("y" %in% diag$external_vars)

  # 4. global_state
  cap <- capture({ x <- get("y", envir = globalenv()) })
  diag <- diagnose_capture(cap)
  expect_true(diag$global_state)

  # 5. nse
  cap <- capture({ x <- eval(quote(1 + 1)) })
  diag <- diagnose_capture(cap)
  expect_true(diag$nse)

  # 6. direct_assign
  cap <- capture({ x <- 1 })
  diag <- diagnose_capture(cap)
  expect_true(diag$direct_assign)

  # 7. indirect_mutate
  cap <- capture({ assign("x", 5) })
  diag <- diagnose_capture(cap)
  expect_true(diag$indirect_mutate)

  # 8. orphan_meta
  cap <- format_capture(list(
    new_callobj(expr = call("meta", ._label = "test")),
    new_callobj(expr = quote(x <- 1))
  ))
  diag <- diagnose_capture(cap)
  expect_true(diag$orphan_meta)

  # 9. invalid_expr
  fake_capture <- new_capture(list(list(expr = 5, meta = list())))
  diag <- diagnose_capture(fake_capture)
  expect_true(diag$invalid_expr)

  # 10. duplicate_vars
  cap <- capture({
    x <- 1
    x <- 2
  })
  diag <- diagnose_capture(cap)
  expect_true(diag$duplicate_vars)

  # 11. side_effects
  cap <- capture({ print("Hello") })
  diag <- diagnose_capture(cap)
  expect_true(diag$side_effects)
})

test_that("diagnose_capture detects orphan constants", {
  cap <- format_capture(
    expr = list(
      call("<-", as.name("a"), 1),     # orphan
      call("<-", as.name("b"), 2),     # used
      call("print", as.name("b"))
    ),
    capture_type = "test"
  )

  diag <- diagnose_capture(cap)
  expect_true("a" %in% diag$orphan_constants)
  expect_false("b" %in% diag$orphan_constants)
})


test_that("diagnose_capture helper functions flag expected features", {
  cap <- capture({ set.seed(1); x <- runif(5); y <- x + 1 })
  struct <- analyze_capture_structure(cap)
  expect_true(detect_stochastic(struct))
  expect_true(detect_seed_used(struct))
  expect_false(detect_global_state(struct))
  expect_false(detect_nse(struct))
})
