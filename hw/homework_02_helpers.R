# ============================================================================
# Helper functions for Homework 2: probability, simulation, and certainty
# ============================================================================
#
# Base R only: you do not need to install any packages.
#
# Load this file once, at the top of your script or Quarto document:
#
#     source("homework_02_helpers.R")
#
# These functions do not simulate your chosen example for you. They help you
# summarize many independent trials once you have decided what an "experiment"
# is and what counts as landing close enough to the theoretical probability.
#
# In Bernoulli's example, the target probability is p0 = 0.6 and the allowed
# gap is epsilon = 1 / 50 = 0.02. An experiment of size N is judged successful
# if the observed proportion falls between 0.58 and 0.62, inclusive.
#
# A certainty level of 1000:1 means that the long-run chance of success should
# be at least 1000 / 1001.
#
# ============================================================================
# QUICK INDEX
# ============================================================================
#
#   summarize_experiments(counts, N, p0, epsilon, certainty_odds = 1000)
#       Takes a vector of success counts from many independent experiments.
#       Reports how often the observed proportion was close enough to p0.
#
#   simulate_grid(N_values, p0, epsilon, certainty_odds = 1000,
#                 B = 10000, model_p = p0)
#       Runs independent binomial simulation studies for several choices of N.
#       The random seed is yours to set before calling this function.
#
#   plot_certainty(results, certainty_odds = 1000)
#       Plots the estimated chance of landing close enough, with a horizontal
#       line at the requested certainty level.
#
#   check_indicators(x, N)
#       Checks that a vector contains exactly N zeros and ones. Useful if your
#       experiment creates individual 0/1 outcomes before you add them up.
#
# ============================================================================


check_probability <- function(value, name, allow_zero_one = TRUE) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
      !is.finite(value)) {
    stop(name, " must be one finite number.", call. = FALSE)
  }

  lower_ok <- if (allow_zero_one) value >= 0 else value > 0
  upper_ok <- if (allow_zero_one) value <= 1 else value < 1
  if (!lower_ok || !upper_ok) {
    endpoint_text <- if (allow_zero_one) {
      "between 0 and 1"
    } else {
      "between 0 and 1, not including the endpoints"
    }
    stop(name, " must be ", endpoint_text, ".", call. = FALSE)
  }

  invisible(TRUE)
}


check_positive_whole_number <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
      !is.finite(value) || value < 1 || abs(value - round(value)) > 1e-10) {
    stop(name, " must be one positive whole number.", call. = FALSE)
  }

  invisible(TRUE)
}


check_nonnegative_whole_numbers <- function(values, name) {
  if (!is.numeric(values) || length(values) < 1 || anyNA(values) ||
      any(!is.finite(values)) || any(values < 0) ||
      any(abs(values - round(values)) > 1e-10)) {
    stop(name, " must contain non-negative whole numbers.", call. = FALSE)
  }

  invisible(TRUE)
}


inside_count_bounds <- function(N, p0, epsilon) {
  lower_raw <- N * max(0, p0 - epsilon)
  upper_raw <- N * min(1, p0 + epsilon)
  tolerance <- 1e-10

  c(
    lower = ceiling(lower_raw - tolerance),
    upper = floor(upper_raw + tolerance)
  )
}


clopper_pearson_95 <- function(inside, total) {
  if (inside == 0) {
    lower <- 0
  } else {
    lower <- qbeta(0.025, inside, total - inside + 1)
  }

  if (inside == total) {
    upper <- 1
  } else {
    upper <- qbeta(0.975, inside + 1, total - inside)
  }

  c(lower = lower, upper = upper)
}


summarize_experiments <- function(counts, N, p0, epsilon,
                                  certainty_odds = 1000) {
  check_positive_whole_number(N, "N")
  check_nonnegative_whole_numbers(counts, "counts")
  check_probability(p0, "p0")
  if (!is.numeric(epsilon) || length(epsilon) != 1 || is.na(epsilon) ||
      !is.finite(epsilon) || epsilon < 0) {
    stop("epsilon must be one non-negative number.", call. = FALSE)
  }
  if (!is.numeric(certainty_odds) || length(certainty_odds) != 1 ||
      is.na(certainty_odds) || !is.finite(certainty_odds) ||
      certainty_odds <= 0) {
    stop("certainty_odds must be one positive number.", call. = FALSE)
  }
  if (any(counts > N + 1e-10)) {
    stop("No count can be larger than N.", call. = FALSE)
  }

  N <- round(N)
  counts <- round(counts)
  bounds <- inside_count_bounds(N, p0, epsilon)
  inside_each <- counts >= bounds[["lower"]] & counts <= bounds[["upper"]]

  B <- length(counts)
  inside <- sum(inside_each)
  outside <- B - inside
  q_hat <- inside / B
  certainty_target <- certainty_odds / (certainty_odds + 1)
  interval <- clopper_pearson_95(inside, B)

  if (inside == 0) {
    odds_hat <- 0
  } else if (outside == 0) {
    odds_hat <- NA_real_
  } else {
    odds_hat <- inside / outside
  }

  assessment <- if (interval[["lower"]] >= certainty_target) {
    "meets"
  } else if (interval[["upper"]] < certainty_target) {
    "below"
  } else {
    "unresolved"
  }

  data.frame(
    N = N,
    B = B,
    inside = inside,
    outside = outside,
    q_hat = q_hat,
    odds_hat = odds_hat,
    lower95 = interval[["lower"]],
    upper95 = interval[["upper"]],
    assessment = assessment,
    stringsAsFactors = FALSE
  )
}


simulate_grid <- function(N_values, p0, epsilon, certainty_odds = 1000,
                          B = 10000, model_p = p0) {
  check_nonnegative_whole_numbers(N_values, "N_values")
  if (any(N_values < 1)) {
    stop("Every value in N_values must be at least 1.", call. = FALSE)
  }
  check_probability(p0, "p0")
  check_probability(model_p, "model_p")
  check_positive_whole_number(B, "B")

  if (!is.numeric(epsilon) || length(epsilon) != 1 || is.na(epsilon) ||
      !is.finite(epsilon) || epsilon < 0) {
    stop("epsilon must be one non-negative number.", call. = FALSE)
  }

  N_values <- round(N_values)
  B <- round(B)

  pieces <- vector("list", length(N_values))
  for (i in seq_along(N_values)) {
    N <- N_values[[i]]
    counts <- rbinom(n = B, size = N, prob = model_p)
    pieces[[i]] <- summarize_experiments(
      counts = counts,
      N = N,
      p0 = p0,
      epsilon = epsilon,
      certainty_odds = certainty_odds
    )
  }

  do.call(rbind, pieces)
}


plot_certainty <- function(results, certainty_odds = 1000) {
  required <- c("N", "q_hat", "lower95", "upper95")
  missing <- setdiff(required, names(results))
  if (length(missing) > 0) {
    stop("results is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!is.numeric(certainty_odds) || length(certainty_odds) != 1 ||
      is.na(certainty_odds) || !is.finite(certainty_odds) ||
      certainty_odds <= 0) {
    stop("certainty_odds must be one positive number.", call. = FALSE)
  }
  if (nrow(results) < 1) {
    stop("results must contain at least one row.", call. = FALSE)
  }

  x <- results$N
  y <- results$q_hat
  lower <- pmax(0, results$lower95)
  upper <- pmin(1, results$upper95)
  certainty_target <- certainty_odds / (certainty_odds + 1)

  if (any(!is.finite(x)) || any(!is.finite(y)) ||
      any(!is.finite(lower)) || any(!is.finite(upper)) ||
      any(x <= 0)) {
    stop("N, q_hat, lower95, and upper95 must be finite; N must be positive.",
         call. = FALSE)
  }

  use_log_x <- length(unique(x)) > 2 && max(x) / min(x) >= 10
  x_log <- if (use_log_x) "x" else ""

  y_values <- c(lower, upper, certainty_target)
  y_limits <- range(y_values, na.rm = TRUE)
  y_padding <- max(0.01, diff(y_limits) * 0.08)
  y_limits <- c(max(0, y_limits[1] - y_padding),
                min(1, y_limits[2] + y_padding))

  plot(
    x, y,
    log = x_log,
    ylim = y_limits,
    xlab = "Number of trials in each experiment (N)",
    ylab = "Chance of landing close enough",
    main = "Simulated certainty",
    pch = 19
  )
  segments(x0 = x, y0 = lower, x1 = x, y1 = upper)
  lines(x[order(x)], y[order(x)], lty = 1)
  abline(h = certainty_target, lty = 2, col = "gray40")

  invisible(NULL)
}


check_indicators <- function(x, N) {
  check_positive_whole_number(N, "N")

  if (length(x) != N) {
    stop("x must contain exactly N values.", call. = FALSE)
  }
  if (anyNA(x)) {
    stop("x cannot contain missing values.", call. = FALSE)
  }
  if (!all(x %in% c(0, 1))) {
    stop("x must contain only 0s and 1s.", call. = FALSE)
  }

  invisible(TRUE)
}
