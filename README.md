# frankenR

`frankenR` is a flexible R package for capturing, modifying, and rerunning structured blocks of code. It is designed for programmatic workflows that need to parse, analyze, mutate, re-execute, or export R code while retaining its logical structure. `frankenR` treats R code as manipulable data, making it ideal for building DSLs, transforming code, attaching metadata, and managing reproducible execution pipelines.

---

## Overview

### 1. **Capture**
Capture R expressions in three ways:
- **Session**: Live capture during interactive use with `capture()` and `capture_end()`.
- **Block**: Capture code from a `{}` block.
- **Script**: Capture from external `.R` files.

The result is a `code_capture` object:
```r
cap <- capture({
  x <- rnorm(100)
  hist(x)
})
```

### 2. **Modification**
Captured expressions can be mutated via functions or overloaded operators:
```r
cap[[2]] <- cap[[2]] + list(main = "Histogram")
cap[[2]] <- set_arg(cap[[2]], "breaks", 20)
```

### 3. **Rerun and Export**
Captured expressions can be rerun or exported to scripts:
```r
rerun_capture(cap, verbose = TRUE)
export_capture(cap, "my_script.R", meta = "comments")
```

---

## Use Cases

- **Plot customization pipelines** (e.g. `plob` or `ggplot` builders)
- **Code transformation for reproducibility or instrumentation**
- **Attaching semantic metadata to code blocks** (e.g., categories, tags)
- **Extracting and modifying argument structure programmatically**
- **Teaching tools for step-by-step code deconstruction**

---

## Accessors and Utilities

### Extract elements:
```r
get_function_name(cap[[2]])
get_arguments(cap[[2]])
get_expr_text(cap)
```

### Subsetting:
```r
cap[1:2]  # Returns a new code_capture with subset
cap[[1]]  # Returns a specific expression (as a callobj)
cap[[1]]["col"]  # Access argument by name
cap[[1]][1]       # Access first unnamed argument
```

### Operators:
```r
cap[[2]] + list(col = "blue")  # Add/modify arguments
cap[[2]] - "col"                # Remove argument by name
cap[[2]]$col                   # Extract argument by name
cap + capture({...})           # Concatenate captures
```

---

## Advanced Modifications

### Normalization
Adds names to unnamed arguments where possible based on function formals:
```r
cap_norm <- normalize_capture(cap)
```

### Realization
Evaluates all arguments inside captured expressions (without running the function):
```r
cap_realized <- realize_capture(cap)
```

### Atomization
Breaks complex expressions into linearized intermediate steps:
```r
cap_atom <- atomize_capture(cap, prefix = "tmp")
cap_selective <- atomize_selective_capture(cap, fn_names = c("mean", "sum"))
```

---

## Capture Methods Under the Hood

### 1. **Session Capture**
```r
capture()
x <- 1
plot(x)
meta(._label = "intro")
capture_end()
```
- Uses `addTaskCallback()` to monitor top-level evaluated expressions.
- Captures runtime calls.
- Detects `meta()` and attaches metadata to the previous expression.

### 2. **Block Capture**
```r
cap <- capture({
  x <- 5
  y <- x + 2
  meta(._desc = "Simple math")
})
```
- Uses `substitute()` to parse blocks without evaluation.
- `meta()` calls are parsed and metadata is attached internally.

### 3. **Script Capture**
```r
cap <- capture(script = "myscript.R")
```
- Parses file with `parse()` and reads `#@` metadata from comments.
- Attaches inline `meta(...)` calls and comment tags to previous expressions.

Each method returns a structured object:
```r
str(cap)
# List of 3
#  $ capture_type: chr "block" | "session" | "script"
#  $ expressions : list of call objects
#  $ meta        : list of metadata (or empty list)
```

---

## Metadata

Attach metadata using inline `meta()`:
```r
hist(x)
meta(._label = "Initial plot", ._class = "diagnostic")
```
Or as tagged comments in scripts:
```r
x <- rnorm(100)  #@ ._label = "Random draw"
```

### Metadata Uses
- Annotating expressions for categorization.
- Recording user intent, tags, or transformation info.
- Exporting annotations as:
```r
export_capture(cap, "out.R", meta = "comments")  # # key: value
export_capture(cap, "out.R", meta = "code")      # meta(...) code blocks
```

### Access
```r
cap$meta[[2]]       # Raw metadata list
cap[[2]]$label       # Access from callobj (if stored)
```

---

## Summary
`frankenR` lets you treat code like data. Capture blocks of R code as structured objects, modify them programmatically, and re-execute or export them with optional metadata. This package provides powerful low-level tools for R metaprogramming, dynamic scripting, and traceable computation.

---

## Example: End-to-End
```r
cap <- capture({
  x <- rnorm(100)
  hist(x)
  meta(._label = "Initial histogram")
})

cap[[2]] <- cap[[2]] + list(main = "Updated")
cap <- normalize_capture(cap)
cap <- atomize_capture(cap, depth = 2)
export_capture(cap, "plot.R", meta = "comments")
rerun_capture(cap, verbose = TRUE)
```

---
