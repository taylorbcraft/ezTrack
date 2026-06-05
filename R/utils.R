# Internal null-coalescing helper for compatibility with R < 4.4.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

utils::globalVariables(c(
  "bin",
  "first",
  "id",
  "timestamp",
  "y",
  "y_labels"
))
