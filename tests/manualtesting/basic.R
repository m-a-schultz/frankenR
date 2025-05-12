major_ops <- function(cap){
  atomized <- atomize_capture(cap)
  realized <- realize_capture(cap)
  normalized <- normalize_capture(cap)
  filtered <- filter_by_function(cap, "mean")
  list(original = cap, atom = atomized, real = realized, normal = normalized, filt = filtered)
}

cap1a <- capture(script="tests/examples/example1.R")
cap2a <- capture_script("tests/examples/example2.R")
cap3a <- capture_script("tests/examples/example3.R")
cap4a <- capture_script("tests/examples/example4.R")
cap5a <- capture_script("tests/examples/example5.R")

major_ops(cap1a)
major_ops(cap2a)
major_ops(cap3a)
major_ops(cap4a)
major_ops(cap5a)

# --- Test Block 1: Simple function calls ---
cap1b <- capture({
  mean(c(1, 2, 3))
  sum(4, 5, 6)
})

# --- Test Block 2: Nested function calls ---
cap2b <- capture({
  sum(mean(c(1, 2, 3)), sd(c(4, 5, 6)))
})

# --- Test Block 3: Pseudo metadata ---
cap3b <- capture({
  mean(c(1, 2, 3))
  meta(._label = "Mean function", ._category = "Statistics")
  sum(4, 5, 6)
})

# --- Test Block 4: Assignments and filtering ---
cap4b <- capture({
  x <- 1
  y <- sum(1, 2, 3)
  mean(x, y)
})

# --- Test Block 5: Unnamed arguments (normalization candidates) ---
cap5b <- capture({
  mean(c(1, 2, 3), 0.1)
  sd(c(4, 5, 6))
})

major_ops(cap1b)
major_ops(cap2b)
major_ops(cap3b)
major_ops(cap4b)
major_ops(cap5b)

# --- Test Block 1: Simple function calls ---
capture()
mean(c(1, 2, 3))
sum(4, 5, 6)
cap1c <- end_capture()

# --- Test Block 2: Nested function calls ---
capture()
sum(mean(c(1, 2, 3)), sd(c(4, 5, 6)))
cap2c <- end_capture()


# --- Test Block 3: Pseudo metadata ---
capture()
mean(c(1, 2, 3))
meta(._label = "Mean function", ._category = "Statistics")
sum(4, 5, 6)
cap3c <- end_capture()

# --- Test Block 4: Assignments and filtering ---
capture()
x <- 1
y <- sum(1, 2, 3)
mean(x, y)
cap4c <- end_capture()

# --- Test Block 5: Unnamed arguments (normalization candidates) ---
start_capture()
print("hi")
mean(c(1, 2, 3), 0.1)
sd(c(4, 5, 6))
cap5c <- end_capture()


major_ops(cap1c)
major_ops(cap2c)
major_ops(cap3c)
major_ops(cap4c)
major_ops(cap5c)


cap1a
cap1b
cap1c
