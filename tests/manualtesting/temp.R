# Example lines
lines <- c(
  "x <- 1  #@ ._label = 'one'",
  "y <- 2",
  "z <- c(1, 2, 3)  #@ ._label = 'vector', ._note = 'no split on commas'",
  "a <- 5  #@ ._category = 'math', ._tag = 'simple'"
)

pseudo_map <- extract_pseudo_comments(lines)

# Expected
print(pseudo_map)

# Should look like:
# $`1`
# $`1`$label
# [1] "one"
#
# $`3`
# $`3`$label
# [1] "vector"
#
# $`3`$note
# [1] "no split on commas"
#
# $`4`
# $`4`$category
# [1] "math"
#
# $`4`$tag
# [1] "simple"


tmp_script <- tempfile(fileext = ".R")
writeLines(c(
  "x <- 1  #@ ._label = 'first'",
  "y <- x + 1",
  "z <- c(1, 2, 3)  #@ ._note = 'vector'",
  "result <- sum(x, y, z)  #@ ._summary = TRUE"
), tmp_script)

cap <- capture_script(tmp_script)

# Inspect
print(cap$expressions)
print(cap$pseudo)
