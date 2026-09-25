# Homework 3: base R helpers. Seeds belong in the caller's script.
# No packages or network access are needed once the supplied data are present.

make_prior <- function(name, mean, weight) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || name == "") {
    stop("name must be one non-empty character value.")
  }
  if (!is.numeric(mean) || length(mean) != 1 || !is.finite(mean) ||
      mean <= 0 || mean >= 1) {
    stop("mean must be one finite number between 0 and 1.")
  }
  if (!is.numeric(weight) || length(weight) != 1 || !is.finite(weight) ||
      weight <= 0) {
    stop("weight must be one finite positive number.")
  }
  data.frame(prior = name, prior_mean = mean, prior_weight = weight,
             stringsAsFactors = FALSE)
}

hw3_priors <- function() {
  do.call(rbind, list(
    make_prior("Uniform", 0.5, 2),
    make_prior("Jeffreys", 0.5, 1),
    make_prior("Centered", 0.5, 100),
    make_prior("Low, weak", 0.1, 100),
    make_prior("Low, strong", 0.1, 10000),
    make_prior("Low, extreme", 0.1, 1000000),
    make_prior("High, extreme", 0.9, 1000000)
  ))
}

check_priors <- function(priors) {
  if (!is.data.frame(priors) || nrow(priors) < 1 ||
      !all(c("prior", "prior_mean", "prior_weight") %in% names(priors))) {
    stop("priors must have prior, prior_mean, and prior_weight columns.")
  }
  if (!is.character(priors$prior) || any(is.na(priors$prior)) ||
      any(priors$prior == "")) {
    stop("prior names must be non-empty character values.")
  }
  for (field in c("prior_mean", "prior_weight")) {
    v <- priors[[field]]
    if (!is.numeric(v) || any(!is.finite(v))) {
      stop("prior_mean and prior_weight must be finite numbers.")
    }
  }
  if (any(priors$prior_mean <= 0 | priors$prior_mean >= 1)) {
    stop("prior_mean values must be between 0 and 1.")
  }
  if (any(priors$prior_weight <= 0)) {
    stop("prior_weight values must be positive.")
  }
  invisible(TRUE)
}

check_one_prior <- function(prior) {
  check_priors(prior)
  if (nrow(prior) != 1) stop("Choose exactly one prior row.")
  invisible(TRUE)
}

check_count <- function(x, name, minimum = 0) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) ||
      x < minimum || x != floor(x)) {
    stop(name, " must be a whole number at least ", minimum, ".")
  }
  invisible(TRUE)
}

.prior_shapes <- function(priors) {
  check_priors(priors)
  data.frame(alpha = priors$prior_mean * priors$prior_weight,
             beta = (1 - priors$prior_mean) * priors$prior_weight)
}

.posterior_shapes <- function(prior, boys, girls) {
  check_one_prior(prior)
  check_count(boys, "boys")
  check_count(girls, "girls")
  shapes <- .prior_shapes(prior)
  c(alpha = shapes$alpha + boys, beta = shapes$beta + girls)
}

posterior_mean <- function(prior, boys, girls) {
  shapes <- .posterior_shapes(prior, boys, girls)
  shapes[["alpha"]] / sum(shapes)
}

posterior_interval <- function(prior, boys, girls, level = 0.95) {
  if (!is.numeric(level) || length(level) != 1 || !is.finite(level) ||
      level <= 0 || level >= 1) {
    stop("level must be one finite number between 0 and 1.")
  }
  shapes <- .posterior_shapes(prior, boys, girls)
  tail <- (1 - level) / 2
  stats::qbeta(c(lower = tail, upper = 1 - tail), shapes[["alpha"]],
               shapes[["beta"]])
}

posterior_probability <- function(prior, boys, girls, threshold = 0.5,
                                  above = FALSE, log10 = FALSE) {
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      !is.finite(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be one finite number between 0 and 1.")
  }
  if (!is.logical(above) || length(above) != 1 || is.na(above)) {
    stop("above must be TRUE or FALSE.")
  }
  if (!is.logical(log10) || length(log10) != 1 || is.na(log10)) {
    stop("log10 must be TRUE or FALSE.")
  }
  shapes <- .posterior_shapes(prior, boys, girls)
  p <- stats::pbeta(threshold, shapes[["alpha"]], shapes[["beta"]],
                    lower.tail = !above, log.p = log10)
  if (log10) p <- p / log(10)
  p
}

# One row per prior. Interval bounds enclose 95% of posterior probability.
# Log probabilities are computed directly, preserving very small tails.
posterior_summary <- function(priors, boys, girls) {
  check_priors(priors)
  check_count(boys, "boys")
  check_count(girls, "girls")
  rows <- vector("list", nrow(priors))
  for (i in seq_len(nrow(priors))) {
    prior <- priors[i, , drop = FALSE]
    interval <- posterior_interval(prior, boys, girls)
    rows[[i]] <- data.frame(
      prior,
      posterior_mean = posterior_mean(prior, boys, girls),
      lower95 = interval[1],
      upper95 = interval[2],
      p_le_half = posterior_probability(prior, boys, girls, threshold = 0.5),
      p_gt_half = posterior_probability(prior, boys, girls, threshold = 0.5,
                                        above = TRUE),
      log10_p_le_half = posterior_probability(prior, boys, girls,
                                              threshold = 0.5, log10 = TRUE),
      log10_p_gt_half = posterior_probability(prior, boys, girls,
                                              threshold = 0.5, above = TRUE,
                                              log10 = TRUE),
      row.names = NULL
    )
  }
  do.call(rbind, rows)
}

prior_label <- function(prior) {
  paste0(prior$prior, "\nmean = ", format(prior$prior_mean, trim = TRUE),
         ", weight = ", format(prior$prior_weight, trim = TRUE))
}

# Each panel has its own vertical scale. zoom=TRUE displays the central
# 99.8% of each distribution on its own horizontal scale.
# The endpoint density of Jeffreys' prior is infinite. In the full view,
# draw only p in [0.001, 0.999] so that these spikes do not hide the U shape.
plot_priors <- function(priors, zoom = FALSE) {
  check_priors(priors)
  shapes <- .prior_shapes(priors)
  panel_cols <- min(3, nrow(priors))
  old <- par(mfrow = c(ceiling(nrow(priors) / panel_cols), panel_cols),
             mar = c(3.3, 4.5, 2.8, 0.8))
  on.exit(par(old))
  for (i in seq_len(nrow(priors))) {
    a <- shapes$alpha[i]
    b <- shapes$beta[i]
    limits <- if (zoom) stats::qbeta(c(0.001, 0.999), a, b) else c(0, 1)
    # Add a fine grid where the mass lies so narrow priors are not missed.
    local <- stats::qbeta(c(0.0001, 0.9999), a, b)
    x <- sort(unique(c(seq(max(limits[1], 0.001),
                          min(limits[2], 0.999), length.out = 2001),
                       seq(local[1], local[2], length.out = 2001))))
    x <- x[x > 0 & x < 1 & x >= limits[1] & x <= limits[2]]
    if (!zoom && (a < 1 || b < 1)) x <- x[x >= 0.001 & x <= 0.999]
    plot(x, stats::dbeta(x, a, b), type = "l", xlim = limits,
         xlab = "p", ylab = "", col = "#246A73", lwd = 1.5, cex.axis = 0.75,
         main = prior_label(priors[i, , drop = FALSE]), cex.main = 0.8)
    mtext("Density", side = 2, line = 3.1, cex = 0.7)
  }
  invisible(NULL)
}

# Compare posterior centers and intervals on one probability scale.
plot_posteriors <- function(results, xlim = c(0, 1)) {
  needed <- c("prior", "prior_mean", "prior_weight", "posterior_mean",
              "lower95", "upper95")
  if (!is.data.frame(results) || !all(needed %in% names(results))) {
    stop("results must come from posterior_summary().")
  }
  old <- par(mar = c(4, 8, 2, 1))
  on.exit(par(old))
  y <- rev(seq_len(nrow(results)))
  plot(results$posterior_mean, y, xlim = xlim, yaxt = "n", pch = 19,
       xlab = "p: posterior mean and central 95% interval", ylab = "",
       ylim = c(0.5, nrow(results) + 0.5), col = "#246A73")
  axis(2, at = y, labels = results$prior, las = 1, cex.axis = 0.8)
  segments(results$lower95, y, results$upper95, y, col = "#246A73", lwd = 2)
  abline(v = 0.5, lty = 2, col = "gray40")
  invisible(NULL)
}

check_population <- function(x) {
  if (!is.numeric(x) || length(x) < 2 || any(!is.finite(x)) ||
      length(unique(x)) < 2) {
    stop("Use a numeric population with at least two distinct, finite values.")
  }
  invisible(TRUE)
}

# Treat the observed list as the entire finite population, each row equally
# likely. Thus use the population SD (denominator length(x)), not sd(x).
population_summary <- function(x) {
  check_population(x)
  c(rows = length(x), mean = mean(x),
    sd = sqrt(mean((x - mean(x))^2)))
}

# n = independent draws in ONE mean; B = independently repeated means.
# mode="copied" draws one value and copies it n times: its average is itself.
average_experiment <- function(x, sizes = c(1, 5, 30, 100), B = 4000,
                               mode = c("independent", "copied")) {
  check_population(x)
  mode <- match.arg(mode)
  check_count(B, "B", 2)
  if (!is.numeric(sizes) || !length(sizes)) stop("Supply sample sizes.")
  for (n in sizes) check_count(n, "Each sample size", 1)
  out <- vector("list", length(sizes))
  for (i in seq_along(sizes)) {
    n <- sizes[i]
    means <- if (mode == "copied") {
      sample(x, B, replace = TRUE)
    } else {
      replicate(B, mean(sample(x, size = n, replace = TRUE)))
    }
    out[[i]] <- data.frame(n = n, repetition = seq_len(B), mean = means)
  }
  do.call(rbind, out)
}

# Histograms show Z=(sample mean - population mean)/(population SD/sqrt(n)).
# This common scale separates changes in shape from narrowing raw averages.
# The normal reference is a model prediction, not fitted to each histogram.
plot_average_experiment <- function(results, x, label) {
  pop <- population_summary(x)
  sizes <- unique(results$n)
  z <- (results$mean - pop[["mean"]]) / (pop[["sd"]] / sqrt(results$n))
  old <- par(mfrow = c(ceiling(length(sizes) / 2), 2),
             mar = c(4, 4, 2.5, 1))
  on.exit(par(old))
  limits <- range(c(-4, 4, z))
  for (n in sizes) {
    current <- z[results$n == n]
    h <- hist(current, breaks = 40, plot = FALSE)
    plot(h, freq = FALSE, xlim = limits,
         ylim = c(0, max(h$density, dnorm(0)) * 1.06),
         main = paste(label, "n =", n),
         xlab = "Standardized sample mean", col = "gray85", border = "white")
    curve(dnorm(x), add = TRUE, col = "#A33B20", lwd = 2)
  }
  invisible(NULL)
}

summarize_averages <- function(results, x) {
  pop <- population_summary(x)
  do.call(rbind, lapply(unique(results$n), function(n) {
    m <- results$mean[results$n == n]
    data.frame(n = n, B = length(m), center = mean(m), observed_sd = sd(m),
               independent_sd = pop[["sd"]] / sqrt(n))
  }))
}
