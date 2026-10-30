# HW 06: input checks, exact frequency expansion, and a design check
# for Problem 2. Base R.
# Students write all fits, allocations, simulations, estimators, and plots.

# Problem 1: Scottish militia measurements (same file as HW 05)

check_militia_counts <- function(d) {
  required <- c("regiment", "height_band_in", "height_lower_in",
                "height_upper_in", "chest_in", "count")
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    stop("The militia frequency file is missing required columns.")
  }
  if (nrow(d) == 0L || anyNA(d[required])) {
    stop("The militia frequency file is empty or has missing values.")
  }
  numeric_columns <- c("height_lower_in", "height_upper_in", "chest_in",
                       "count")
  if (!all(vapply(d[numeric_columns], is.numeric, logical(1)))) {
    stop("Heights, chest categories, and counts must be numeric.")
  }
  if (!all(vapply(d[numeric_columns], function(x) all(is.finite(x)),
                  logical(1))) || any(d$count < 0) ||
      any(d$count != floor(d$count)) || sum(d$count) == 0) {
    stop("Counts must be nonnegative integers with a positive total.")
  }
  if (anyDuplicated(d[c("regiment", "height_band_in", "chest_in")])) {
    stop("A regiment-by-height-by-chest frequency cell appears twice.")
  }
  if (any(d$height_lower_in > d$height_upper_in)) {
    stop("A height band's lower endpoint exceeds its upper endpoint.")
  }
  invisible(d)
}

expand_militia_counts <- function(d) {
  check_militia_counts(d)
  # Repeat each recorded category combination exactly count times.
  # This changes the table's format; it creates no new measurements.
  index <- rep(seq_len(nrow(d)), times = d$count)
  columns <- c("regiment", "height_band_in", "height_lower_in",
               "height_upper_in", "chest_in")
  expanded <- d[index, columns, drop = FALSE]
  rownames(expanded) <- NULL
  expanded
}

# Problem 2: the field experiment

design_counts <- function(plots) {
  # Number of plots in each soil class for each treatment combination.
  stopifnot(is.data.frame(plots), all(c("soil", "A", "B") %in% names(plots)))
  soil <- factor(plots$soil, levels = c(0, 1), labels = c("poor", "rich"))
  combination <- factor(paste0("A=", plots$A, ", B=", plots$B),
                        levels = c("A=0, B=0", "A=0, B=1",
                                   "A=1, B=0", "A=1, B=1"))
  counts <- table(soil, combination)
  addmargins(counts, 1)
}

# Problem 3: automobile emissions

check_emissions <- function(d) {
  required <- c("x1", "x2", "run", "y")
  stopifnot(is.data.frame(d), identical(names(d), required),
            nrow(d) == 18L, !anyNA(d),
            all(d$x1 %in% c(-1, 0, 1)), all(d$x2 %in% c(-1, 0, 1)),
            all(d$run %in% c(1, 2)), all(is.finite(d$y)))
  for (r in c(1, 2)) {
    settings <- d[d$run == r, c("x1", "x2")]
    stopifnot(nrow(settings) == 9L, !anyDuplicated(settings))
  }
  invisible(d)
}
