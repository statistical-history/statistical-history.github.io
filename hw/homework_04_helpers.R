# HW 04 helpers. Base R only; seeds belong in the calling document.

check_counties <- function(d) {
  stopifnot(is.data.frame(d), nrow(d) > 1L,
            all(c("county", "population", "births") %in% names(d)),
            !anyDuplicated(d$county), all(is.finite(d$population)),
            all(is.finite(d$births)), all(d$population > 0),
            all(d$births > 0))
  invisible(d)
}

ratio_estimate <- function(d, selected) {
  check_counties(d)
  stopifnot(length(selected) > 0L, !anyDuplicated(selected),
            all(is.finite(selected)), all(selected == as.integer(selected)),
            all(selected >= 1 & selected <= nrow(d)))
  sum(d$births) * (sum(d$population[selected]) / sum(d$births[selected]))
}

county_pools <- function(d) {
  check_counties(d)
  stopifnot(nrow(d) %% 2L == 0L)
  ordered <- order(d$population / d$births, d$county)
  half <- nrow(d) / 2L
  list("All counties (SRS)" = seq_len(nrow(d)),
       "Low-ratio half only" = ordered[seq_len(half)],
       "High-ratio half only" = ordered[half + seq_len(half)])
}

sample_estimates <- function(d, sizes = c(10, 25, 50), B = 2000) {
  pools <- county_pools(d)
  stopifnot(length(sizes) > 0L, !anyDuplicated(sizes),
            all(sizes == as.integer(sizes)), all(sizes > 0),
            all(sizes <= min(lengths(pools))), B >= 2, B == as.integer(B))
  truth <- sum(d$population)
  pieces <- list()
  for (name in names(pools)) {
    pool <- pools[[name]]
    for (n in sizes) {
      estimates <- replicate(B, {
        selected <- pool[sample.int(length(pool), n, replace = FALSE)]
        ratio_estimate(d, selected)
      })
      pieces[[length(pieces) + 1L]] <- data.frame(
        design = name, n = n, repetition = seq_len(B), estimate = estimates,
        error_percent = 100 * (estimates / truth - 1))
    }
  }
  do.call(rbind, pieces)
}

summarize_samples <- function(results) {
  pieces <- lapply(unique(results$design), function(design) {
    do.call(rbind, lapply(sort(unique(results$n)), function(n) {
      x <- results$error_percent[results$design == design & results$n == n]
      data.frame(design = design, n = n, mean_error_percent = mean(x),
                 lower_90 = unname(quantile(x, .05)),
                 upper_90 = unname(quantile(x, .95)),
                 within_10_percent = 100 * mean(abs(x) <= 10))
    }))
  })
  do.call(rbind, pieces)
}

plot_samples <- function(results) {
  old <- par(mfrow = c(1, 3), mar = c(4, 4.2, 3, 1))
  on.exit(par(old))
  limits <- range(c(0, results$error_percent))
  for (design in unique(results$design)) {
    d <- results[results$design == design, ]
    boxplot(error_percent ~ factor(n), data = d, ylim = limits,
            col = "#b9d7e8", outline = TRUE, cex = .35,
            xlab = "Counties sampled", ylab = "Estimation error (%)",
            main = design, cex.main = .85)
    abline(h = 0, lty = 2, col = "#ad3e2b", lwd = 2)
  }
}

check_convictions <- function(d) {
  stopifnot(is.data.frame(d), nrow(d) > 0L,
            all(c("year", "accused", "convicted") %in% names(d)),
            !anyDuplicated(d$year), all(is.finite(d$accused)),
            all(is.finite(d$convicted)), all(d$accused > 0),
            all(d$convicted >= 0 & d$convicted <= d$accused),
            all(d$accused == as.integer(d$accused)),
            all(d$convicted == as.integer(d$convicted)))
  invisible(d)
}

combine_rates <- function(d, method = c("pooled", "equal_year")) {
  check_convictions(d)
  method <- match.arg(method)
  if (method == "pooled") sum(d$convicted) / sum(d$accused)
  else mean(d$convicted / d$accused)
}

change_summary <- function(d, method = c("pooled", "equal_year")) {
  method <- match.arg(method)
  check_convictions(d)
  groups <- list("1825-1830" = 1825:1830, "1831-1833" = 1831:1833,
                 "1831 only" = 1831, "1832-1833" = 1832:1833)
  stopifnot(all(1825:1833 %in% d$year))
  rates <- vapply(groups, function(years)
    combine_rates(d[d$year %in% years, ], method), numeric(1))
  data.frame(period = names(rates), rate = unname(rates),
             change_percentage_points = 100 * unname(rates - rates[1]))
}

plot_convictions <- function(d) {
  check_convictions(d)
  plot(d$year, d$convicted / d$accused, type = "b", pch = 19,
       xlab = "Year", ylab = "Convictions / accused", ylim = c(.50, .66),
       xaxt = "n", col = "#1f5577")
  axis(1, at = d$year)
  abline(v = 1830.5, lty = 2, col = "#ad3e2b")
  abline(h = combine_rates(d[d$year <= 1830, ]), lty = 3)
  legend("bottomleft", c("Recorded annual rate", "Pooled 1825-1830 rate"),
         col = c("#1f5577", "black"), lty = c(1, 3), pch = c(19, NA),
         bty = "n", cex = .8)
}

simulate_no_change <- function(d, B = 4000,
                               method = c("pooled", "equal_year")) {
  check_convictions(d)
  method <- match.arg(method)
  stopifnot(all(1825:1833 %in% d$year), B >= 2, B == as.integer(B))
  before <- d$year <= 1830
  groups <- list("1831-1833" = d$year %in% 1831:1833,
                 "1831 only" = d$year == 1831,
                 "1832-1833" = d$year %in% 1832:1833)
  p0 <- combine_rates(d[before, ], "pooled")
  counts <- matrix(rbinom(nrow(d) * B, size = rep(d$accused, B), prob = p0),
                   nrow = nrow(d), ncol = B)
  rate <- function(keep) {
    if (method == "pooled") colSums(counts[keep, , drop = FALSE]) / sum(d$accused[keep])
    else colMeans(sweep(counts[keep, , drop = FALSE], 1, d$accused[keep], "/"))
  }
  before_rate <- rate(before)
  simulations <- do.call(rbind, lapply(names(groups), function(name) {
    data.frame(comparison = name, repetition = seq_len(B),
               difference_pp = 100 * (rate(groups[[name]]) - before_rate))
  }))
  observed <- change_summary(d, method)[-1, ]
  names(observed)[c(1, 3)] <- c("comparison", "difference_pp")
  list(p0 = p0, method = method, simulations = simulations,
       observed = observed[, c("comparison", "difference_pp")])
}

summarize_change <- function(experiment) {
  s <- experiment$simulations
  do.call(rbind, lapply(seq_len(nrow(experiment$observed)), function(i) {
    name <- experiment$observed$comparison[i]
    x <- s$difference_pp[s$comparison == name]
    data.frame(comparison = name,
               observed_pp = experiment$observed$difference_pp[i],
               simulated_mean_pp = mean(x),
               simulated_lower_90 = unname(quantile(x, .05)),
               simulated_upper_90 = unname(quantile(x, .95)))
  }))
}

plot_change <- function(experiment) {
  old <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  on.exit(par(old))
  limits <- range(c(experiment$simulations$difference_pp,
                    experiment$observed$difference_pp, 0))
  breaks <- pretty(limits, n = 35)
  for (i in seq_len(nrow(experiment$observed))) {
    name <- experiment$observed$comparison[i]
    x <- experiment$simulations$difference_pp[
      experiment$simulations$comparison == name]
    hist(x, breaks = breaks, xlim = range(breaks), col = "#b9d7e8",
         border = "white", main = paste(name, "vs. before"),
         cex.main = .85, xlab = "After - before (percentage points)")
    abline(v = experiment$observed$difference_pp[i], col = "#ad3e2b", lwd = 2)
    abline(v = 0, lty = 2)
    legend("topleft", "Observed difference", col = "#ad3e2b", lwd = 2,
           bty = "n", cex = .65)
  }
}
