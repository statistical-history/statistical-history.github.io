# HW 07: input checks for Problem 1. Base R.
# This helper validates the supplied data only. Students write the regressions,
# predictions, comparisons, and interpretations in their own submission.

check_galton_pairs <- function(d) {
  required <- c("family", "brother1", "brother2", "father", "mother", "weight")
  if (!is.data.frame(d) || anyDuplicated(names(d)) ||
      !all(required %in% names(d))) {
    stop("The Galton brother-pair file is missing required columns.")
  }
  if (nrow(d) == 0L || anyNA(d[required])) {
    stop("The Galton brother-pair file is empty or has missing values.")
  }
  numeric_columns <- c("brother1", "brother2", "father", "mother", "weight")
  if (!all(vapply(d[numeric_columns], is.numeric, logical(1))) ||
      !all(vapply(d[numeric_columns], function(x) all(is.finite(x)), logical(1)))) {
    stop("Heights and weights must be finite numbers.")
  }
  family <- as.character(d$family)
  if (any(!nzchar(trimws(family))) || any(d$weight <= 0)) {
    stop("Each row needs a family identifier and a positive weight.")
  }
  if (nrow(d) != 1210L || length(unique(family)) != 137L) {
    stop("Use the complete supplied file: 1,210 rows from 137 families.")
  }

  tolerance <- 1e-10
  for (id in unique(family)) {
    s <- d[family == id, , drop = FALSE]
    if (length(unique(s$father)) != 1L || length(unique(s$mother)) != 1L) {
      stop("Parent measurements must be constant within each family.")
    }
    # A family with k recorded sons contributes k*(k-1) ordered pairs.
    k <- (1 + sqrt(1 + 4 * nrow(s))) / 2
    if (k < 2 || abs(k - round(k)) > tolerance) {
      stop("A family has an unexpected number of brother-pair rows.")
    }
    if (abs(sum(s$weight) - 1) > tolerance ||
        any(abs(s$weight - 1 / nrow(s)) > tolerance)) {
      stop("Each family must have equal row weights that sum to one.")
    }
    if (any(abs(sort(s$brother1) - sort(s$brother2)) > tolerance)) {
      stop("Both brother columns must contain the same measurements within a family.")
    }
    # Equal heights are allowed: two distinct brothers can have the same
    # recorded height. Ties can also make several pair rows identical.
  }
  invisible(d)
}
