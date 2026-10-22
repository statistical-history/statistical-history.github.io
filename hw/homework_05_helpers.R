# HW 05: input checks and exact frequency expansion only. Base R.
# Students write all summaries, plots, fits, and forecasts.

# Problem 1: Birth counts

check_annual_births <- function(d) {
  required <- c("year", "births", "days")
  stopifnot(is.data.frame(d), identical(names(d), required),
            nrow(d) == 10L, !anyNA(d),
            identical(as.integer(d$year), 1994:2003),
            all(is.finite(d$births)), all(d$births > 0),
            all(d$births == as.integer(d$births)),
            all(d$days %in% c(365L, 366L)))
  expected_days <- ifelse(d$year %in% c(1996L, 2000L), 366L, 365L)
  stopifnot(all(d$days == expected_days))
  invisible(d)
}

check_birth_days <- function(d) {
  required <- c("year", "month", "date_of_month", "day_of_week", "births")
  stopifnot(is.data.frame(d), identical(names(d), required),
            nrow(d) == 3652L, !anyNA(d),
            all(is.finite(as.matrix(d))),
            all(as.matrix(d) == floor(as.matrix(d))),
            all(d$births > 0), all(d$day_of_week %in% 1:7))
  dates <- as.Date(sprintf("%04d-%02d-%02d", d$year, d$month, d$date_of_month))
  expected <- seq(as.Date("1994-01-01"), as.Date("2003-12-31"), by = "day")
  stopifnot(!anyNA(dates), length(dates) == length(expected),
            all(dates == expected),
            all(as.integer(format(dates, "%u")) == d$day_of_week))
  invisible(d)
}

# Problem 2: Scottish militia measurements

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
