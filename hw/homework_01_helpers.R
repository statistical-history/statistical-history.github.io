# ============================================================================
# Helper functions for Homework 1
# ============================================================================
#
# Base R only: you do not need to install any packages.
#
# Load this file once, at the top of your document:
#
#     source("homework_01_helpers.R")
#
# It covers BOTH problems. The two halves are completely independent -- nothing
# in the Problem 1 section is used by Problem 2, or the other way round -- so
# find your problem's section below and ignore the other one.
#
# ============================================================================
# WHAT IS IN HERE
# ============================================================================
#
# NONE OF THESE ARE R FUNCTIONS. They were written for this course. `?best_line`
# will not work and searching for them online will not help -- they exist only
# in this file. The assignment text lists, part by part, which ones that part
# uses and what each does for you; this is the quick index.
#
# ---- PROBLEM 1 -- Mayer's 27 equations ------------------------------------
#      data: data/mayer_observations.csv          unknowns: beta, alpha, gamma
#
#   check_mayer_data(mayer)
#       Checks the CSV loaded correctly. Stops with a readable message if
#       something is wrong; silent if all is well. Returns nothing you use.
#
#   format_angle(arcminutes)
#       872.83 -> "14 degrees 32.83 minutes". Returns text.
#
#   sum_mayer_equations(rows, label)
#       Adds up one chosen set of equations. Returns a one-row data frame.
#       (Used internally by the functions below; you will not need to call it.)
#
#   solve_mayer_sums(sums)
#       Solves three summed equations for the three unknowns. Returns a one-row
#       data frame with beta, alpha, gamma, theta, and whether it solved.
#
#   solve_mayer_rows(mayer, equation_ids)
#       Same, but you name three equation numbers and it does the subsetting
#       and solving in one step. Returns the same one-row shape.
#
#   repeat_random_partitions(mayer, repetitions, seed)
#       Shuffles all 27 into three sets of nine, sums, and solves -- repeated
#       as many times as you ask. Returns one row per repetition.
#
#   repeat_random_triples(mayer, repetitions, seed, one_from_each_group)
#       Draws three equations at random and solves them, repeatedly. The last
#       argument switches between the two selection rules. One row per rep.
#
#   mayer_error_minutes(full_alpha, comparison_alpha)
#       Mayer's own accuracy comparison between a 27-equation answer and a
#       3-equation one. Returns a number, or a whole column of numbers.
#
#   summarize_alpha_results(results, reference_alpha, method_name)
#       Boils many repetitions down to one row: smallest, middle, largest, and
#       how far they moved from Mayer's own answer.
#
#   plot_alpha_runs(results, reference_alpha, main)
#       Plots those repetitions in order, with Mayer's answer marked. Draws a
#       plot; returns nothing.
#
# ---- PROBLEM 2 -- Boscovich's five meridian arcs --------------------------
#      data: data/boscovich_arcs.csv           unknowns: A, B
#      model: arc_toises = A + B * sin^2(latitude)
#
#   check_boscovich_data(arcs)
#       As check_mayer_data(), for the five arcs.
#
#   add_sin2_latitude(arcs)
#       Adds the sin^2(latitude) column the model needs. Returns the data with
#       two new columns. Safe to run more than once.
#
#   score_line(arcs, intercept, slope, label)
#       THE SCORECARD. Give it any line; it reports how that line does against
#       all five arcs under both criteria -- total absolute error and total
#       squared error -- whether its errors cancel to zero, and the
#       earth-shape it implies. Returns one scored row.
#
#   boscovich_candidate_lines(arcs)
#       Builds AND scores Boscovich's five candidate lines. Returns five rows.
#
#   boscovich_residual_table(arcs)
#       All 25 residuals: each candidate measured against each arc.
#
#   best_line(lines, criterion)
#       Picks the winner from any table of scored lines. criterion is
#       "absolute" (Boscovich's rule) or "squared" (Legendre's). One row.
#
#   all_pair_lines(arcs)
#       Builds and scores all ten lines through two arcs. Returns ten rows.
#
#   mayer_split_line(arcs, low_group_size)
#       Mayer's method applied to the five arcs: sorts them, cuts them into a
#       low and a high group at the size you name, averages each group, solves
#       the 2x2, and scores the answer. Returns one scored row.
#
#   compare_methods(...)
#       Stacks scored rows from different methods into one comparison table.
#
#   plot_arc_lines(arcs, lines, main)
#       Plots the five arcs with any set of lines drawn on top. Pass a whole
#       table of candidates, or a single winning row. Draws; returns nothing.
#
# ============================================================================


check_mayer_data <- function(mayer) {
  required <- c(
    "equation", "mayer_group", "constant_degrees", "constant_minutes",
    "constant_arcminutes", "coef_alpha", "coef_alpha_sin_theta"
  )

  missing <- setdiff(required, names(mayer))
  if (length(missing) > 0) {
    stop(
      paste("The Mayer data are missing:", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  numeric_columns <- setdiff(required, "mayer_group")
  numeric_check <- vapply(mayer[numeric_columns], is.numeric, logical(1))
  if (!all(numeric_check)) {
    stop("The equation, angle, and coefficient columns must be numeric.",
         call. = FALSE)
  }
  if (anyNA(mayer[required]) ||
      any(!is.finite(as.matrix(mayer[numeric_columns])))) {
    stop("The Mayer data contain a missing or non-finite value.", call. = FALSE)
  }

  if (nrow(mayer) != 27 ||
      !identical(sort(as.integer(mayer$equation)), 1:27) ||
      any(mayer$equation != as.integer(mayer$equation))) {
    stop("The Mayer data should contain equation numbers 1 through 27 once each.",
         call. = FALSE)
  }

  if (any(mayer$constant_degrees != as.integer(mayer$constant_degrees)) ||
      any(mayer$constant_minutes != as.integer(mayer$constant_minutes)) ||
      any(mayer$constant_minutes < 0 | mayer$constant_minutes >= 60)) {
    stop("Angle degrees must be whole numbers and minutes must run from 0 to 59.",
         call. = FALSE)
  }

  expected_arcminutes <- 60 * mayer$constant_degrees + mayer$constant_minutes
  if (!all(mayer$constant_arcminutes == expected_arcminutes)) {
    stop("The degree/minute and total-arcminute columns do not agree.", call. = FALSE)
  }

  group_counts <- table(mayer$mayer_group)
  if (!all(c("I", "II", "III") %in% names(group_counts)) ||
      !all(group_counts[c("I", "II", "III")] == 9)) {
    stop("Mayer's groups I, II, and III should each contain nine equations.",
         call. = FALSE)
  }

  invisible(TRUE)
}

format_angle <- function(arcminutes, digits = 2) {
  vapply(arcminutes, function(value) {
    if (!is.finite(value)) return("not available")
    sign_text <- if (value < 0) "-" else ""
    absolute_value <- abs(value)
    degrees <- floor(absolute_value / 60)
    minutes <- absolute_value - 60 * degrees
    paste0(sign_text, degrees, " degrees ", round(minutes, digits), " minutes")
  }, character(1))
}

# Add the left-side constants and the two right-side coefficients for any
# chosen set of equations. This is the operation Mayer performed by hand.
sum_mayer_equations <- function(rows, label = "") {
  if (nrow(rows) < 1) stop("Choose at least one equation to sum.", call. = FALSE)

  data.frame(
    label = label,
    n = nrow(rows),
    constant_arcminutes = sum(rows$constant_arcminutes),
    coef_alpha = sum(rows$coef_alpha),
    coef_alpha_sin_theta = sum(rows$coef_alpha_sin_theta),
    stringsAsFactors = FALSE
  )
}

# Solve three summed equations for beta, alpha, and gamma, where
# gamma = alpha * sin(theta). Students do not need matrix algebra for HW 1.
solve_mayer_sums <- function(sums) {
  required <- c(
    "n", "constant_arcminutes", "coef_alpha", "coef_alpha_sin_theta"
  )
  missing <- setdiff(required, names(sums))
  if (length(missing) > 0) {
    stop(paste("The sums are missing:", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if (nrow(sums) != 3) {
    stop("Exactly three summed equations are needed for the three unknowns.",
         call. = FALSE)
  }
  if (!all(vapply(sums[required], is.numeric, logical(1))) ||
      anyNA(sums[required]) ||
      any(!is.finite(as.matrix(sums[required])))) {
    stop("Replace every NA in the three sums with a finite numeric value.",
         call. = FALSE)
  }

  equation_matrix <- cbind(
    beta = sums$n,
    alpha = -sums$coef_alpha,
    gamma = -sums$coef_alpha_sin_theta
  )

  separation <- abs(det(equation_matrix))
  answer <- tryCatch(
    solve(equation_matrix, sums$constant_arcminutes),
    error = function(e) NULL
  )

  if (is.null(answer) || any(!is.finite(answer))) {
    return(data.frame(
      solved = FALSE,
      beta_arcminutes = NA_real_,
      alpha_arcminutes = NA_real_,
      gamma_arcminutes = NA_real_,
      theta_degrees = NA_real_,
      theta_available = FALSE,
      equation_separation = separation
    ))
  }

  ratio <- answer[["gamma"]] / answer[["alpha"]]
  theta_available <- is.finite(ratio) && abs(ratio) <= 1 + 1e-12
  theta_degrees <- NA_real_
  if (theta_available) {
    ratio <- max(-1, min(1, ratio))
    theta_degrees <- asin(ratio) * 180 / pi
  }

  data.frame(
    solved = TRUE,
    beta_arcminutes = unname(answer[["beta"]]),
    alpha_arcminutes = unname(answer[["alpha"]]),
    gamma_arcminutes = unname(answer[["gamma"]]),
    theta_degrees = theta_degrees,
    theta_available = theta_available,
    equation_separation = separation
  )
}

solve_mayer_rows <- function(mayer, equation_ids) {
  check_mayer_data(mayer)
  if (length(equation_ids) != 3 || length(unique(equation_ids)) != 3) {
    stop("Choose three distinct equation numbers.", call. = FALSE)
  }
  if (!all(equation_ids %in% mayer$equation)) {
    stop("At least one chosen equation number is not in the data.", call. = FALSE)
  }

  rows <- mayer[match(sort(equation_ids), mayer$equation), , drop = FALSE]
  sums <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    sum_mayer_equations(rows[i, , drop = FALSE], as.character(rows$equation[i]))
  }))
  answer <- solve_mayer_sums(sums)

  cbind(
    data.frame(
      equations = paste(rows$equation, collapse = ","),
      source_groups = paste(rows$mayer_group, collapse = ","),
      stringsAsFactors = FALSE
    ),
    answer
  )
}

# Each repetition shuffles all 27 equations and splits them into three
# non-overlapping sets of nine. Every equation is used exactly once.
repeat_random_partitions <- function(mayer, repetitions = 1000, seed = 1750) {
  check_mayer_data(mayer)
  if (!is.numeric(repetitions) || length(repetitions) != 1 ||
      is.na(repetitions) || !is.finite(repetitions) ||
      repetitions < 1 || repetitions != round(repetitions)) {
    stop("repetitions must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != round(seed)) {
    stop("seed must be one non-negative whole number.", call. = FALSE)
  }

  set.seed(seed)
  output <- vector("list", repetitions)

  for (i in seq_len(repetitions)) {
    shuffled <- sample(mayer$equation, size = 27, replace = FALSE)
    sets <- split(shuffled, rep(1:3, each = 9))
    sums <- do.call(rbind, lapply(seq_along(sets), function(j) {
      rows <- mayer[mayer$equation %in% sets[[j]], , drop = FALSE]
      sum_mayer_equations(rows, paste0("random_set_", j))
    }))
    answer <- solve_mayer_sums(sums)

    output[[i]] <- cbind(
      data.frame(
        repetition = i,
        set_1 = paste(sort(sets[[1]]), collapse = ","),
        set_2 = paste(sort(sets[[2]]), collapse = ","),
        set_3 = paste(sort(sets[[3]]), collapse = ","),
        stringsAsFactors = FALSE
      ),
      answer
    )
  }

  do.call(rbind, output)
}

# Draw either any three distinct equations, or exactly one equation from
# each of Mayer's original groups. Each draw is solved as a three-row system.
repeat_random_triples <- function(mayer, repetitions = 1000, seed = 1751,
                                  one_from_each_group = FALSE) {
  check_mayer_data(mayer)
  if (!is.numeric(repetitions) || length(repetitions) != 1 ||
      is.na(repetitions) || !is.finite(repetitions) ||
      repetitions < 1 || repetitions != round(repetitions)) {
    stop("repetitions must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
      seed != round(seed)) {
    stop("seed must be one non-negative whole number.", call. = FALSE)
  }
  if (length(one_from_each_group) != 1 || !is.logical(one_from_each_group) ||
      is.na(one_from_each_group)) {
    stop("one_from_each_group must be TRUE or FALSE.", call. = FALSE)
  }

  set.seed(seed)
  output <- vector("list", repetitions)
  group_order <- c("I", "II", "III")

  for (i in seq_len(repetitions)) {
    if (one_from_each_group) {
      chosen <- vapply(group_order, function(group) {
        sample(mayer$equation[mayer$mayer_group == group], size = 1)
      }, numeric(1))
      selection_rule <- "one from each Mayer group"
    } else {
      chosen <- sample(mayer$equation, size = 3, replace = FALSE)
      selection_rule <- "any three equations"
    }

    answer <- solve_mayer_rows(mayer, chosen)
    output[[i]] <- cbind(
      data.frame(
        repetition = i,
        selection_rule = selection_rule,
        stringsAsFactors = FALSE
      ),
      answer
    )
  }

  do.call(rbind, output)
}

# Mayer treated accuracy as improving in direct proportion to the number of
# equations. This returns his larger, conservative error calculation.
mayer_error_minutes <- function(full_alpha, comparison_alpha,
                                full_equations = 27, comparison_equations = 3) {
  size_ratio <- full_equations / comparison_equations
  if (!is.finite(size_ratio) || size_ratio <= 1) {
    stop("The full result must use more equations than the comparison.",
         call. = FALSE)
  }
  abs(comparison_alpha - full_alpha) / (size_ratio - 1)
}

summarize_alpha_results <- function(results, reference_alpha, method_name) {
  usable <- results$solved & is.finite(results$alpha_arcminutes)
  values <- results$alpha_arcminutes[usable]
  movement <- abs(values - reference_alpha)

  if (length(values) == 0) {
    stop("There are no solved alpha values to summarize.", call. = FALSE)
  }

  data.frame(
    method = method_name,
    repetitions = nrow(results),
    solved = sum(usable),
    theta_not_available = sum(results$solved & !results$theta_available),
    smallest_alpha = min(values),
    middle_alpha = median(values),
    largest_alpha = max(values),
    middle_absolute_move = median(movement),
    largest_absolute_move = max(movement),
    within_5_minutes = sum(movement <= 5),
    stringsAsFactors = FALSE
  )
}

plot_alpha_runs <- function(results, reference_alpha, main = "Alpha across selections") {
  usable <- results$solved & is.finite(results$alpha_arcminutes)
  values <- sort(results$alpha_arcminutes[usable])
  if (length(values) == 0) stop("There are no alpha values to plot.", call. = FALSE)

  plot(
    seq_along(values), values,
    pch = 16, cex = 0.45,
    xlab = "Repeated selections, ordered by the alpha result",
    ylab = "alpha (arcminutes)",
    main = main
  )
  abline(h = reference_alpha, col = "firebrick", lwd = 2)
}


# ============================================================================
# ============================================================================
##
##   PROBLEM 2 -- BOSCOVICH'S FIVE MERIDIAN ARCS
##
##   Each row of data/boscovich_arcs.csv is one measured arc, and the model is
##
##       arc_toises = A + B * sin^2(latitude)
##
##   with two unknowns, A and B. Five measurements, two unknowns, and no line
##   that satisfies all five -- the same wall Mayer hit, on a smaller problem.
##
# ============================================================================
# ============================================================================


check_boscovich_data <- function(arcs) {
  required <- c("place", "lat_deg", "lat_min", "arc_toises")

  missing <- setdiff(required, names(arcs))
  if (length(missing) > 0) {
    stop(
      paste("The meridian-arc data are missing:", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  numeric_columns <- c("lat_deg", "lat_min", "arc_toises")
  if (!all(vapply(arcs[numeric_columns], is.numeric, logical(1)))) {
    stop("The latitude and arc-length columns must be numeric.", call. = FALSE)
  }
  if (anyNA(arcs[required]) ||
      any(!is.finite(as.matrix(arcs[numeric_columns])))) {
    stop("The meridian-arc data contain a missing or non-finite value.",
         call. = FALSE)
  }

  if (nrow(arcs) != 5) {
    stop("The meridian-arc data should contain exactly five measured arcs.",
         call. = FALSE)
  }
  if (anyDuplicated(arcs$place) > 0) {
    stop("Each measured arc should be listed once, under its own place name.",
         call. = FALSE)
  }

  if (any(arcs$lat_deg != as.integer(arcs$lat_deg)) ||
      any(arcs$lat_min != as.integer(arcs$lat_min)) ||
      any(arcs$lat_min < 0 | arcs$lat_min >= 60) ||
      any(arcs$lat_deg < 0 | arcs$lat_deg > 90)) {
    stop(paste("Latitude degrees must be whole numbers from 0 to 90 and",
               "latitude minutes must run from 0 to 59."), call. = FALSE)
  }

  if (any(arcs$arc_toises <= 0)) {
    stop("Arc lengths must be positive numbers of toises.", call. = FALSE)
  }

  invisible(TRUE)
}

# Boscovich's predictor. One degree of the meridian grows with sin^2(latitude)
# if the earth is flattened at the poles, so this is the column every method
# below fits against. Safe to call more than once.
add_sin2_latitude <- function(arcs) {
  check_boscovich_data(arcs)
  latitude_degrees <- arcs$lat_deg + arcs$lat_min / 60
  arcs$latitude_degrees <- latitude_degrees
  arcs$sin2_latitude <- sin(latitude_degrees * pi / 180)^2
  arcs
}

# The scorecard every method in Problem 2 is judged by. Give it any intercept
# and slope and it reports how that one line does against all five arcs.
#
#   sum_abs_residuals     Boscovich's own criterion (total absolute error)
#   sum_squared_residuals Legendre's criterion (least squares)
#   residual_total        Boscovich's second requirement: errors cancel to zero
#   one_over_ellipticity  the historical answer, 3A/B: 1/230 means the polar
#                         radius is about 1/230 shorter than the equatorial one
score_line <- function(arcs, intercept, slope, label = "") {
  arcs <- add_sin2_latitude(arcs)

  intercept <- as.numeric(intercept)
  slope <- as.numeric(slope)
  if (length(intercept) != 1 || length(slope) != 1 ||
      !is.finite(intercept) || !is.finite(slope)) {
    stop("Give score_line() one finite intercept and one finite slope.",
         call. = FALSE)
  }

  fitted <- intercept + slope * arcs$sin2_latitude
  residuals <- arcs$arc_toises - fitted

  data.frame(
    method = label,
    intercept_A = intercept,
    slope_B = slope,
    sum_abs_residuals = sum(abs(residuals)),
    sum_squared_residuals = sum(residuals^2),
    residual_total = round(sum(residuals), 6),
    one_over_ellipticity = 3 * intercept / slope,
    stringsAsFactors = FALSE
  )
}

# Boscovich's method of situation. Requiring the residuals to cancel to zero
# forces the line through the centroid of the five points, so only the slope
# is still free -- and the best slope always turns out to be the one joining
# the centroid to one of the five arcs. That leaves exactly five lines to check.
boscovich_candidate_lines <- function(arcs) {
  arcs <- add_sin2_latitude(arcs)
  x <- arcs$sin2_latitude
  y <- arcs$arc_toises
  x_centre <- mean(x)
  y_centre <- mean(y)

  if (any(x == x_centre)) {
    stop(paste("One arc sits exactly at the centre of the data, so no single",
               "line joins it to the centre."), call. = FALSE)
  }

  rows <- lapply(seq_len(nrow(arcs)), function(i) {
    slope <- (y[i] - y_centre) / (x[i] - x_centre)
    intercept <- y_centre - slope * x_centre
    cbind(
      data.frame(anchor = arcs$place[i], stringsAsFactors = FALSE),
      score_line(arcs, intercept, slope,
                 label = paste("centroid +", arcs$place[i]))
    )
  })

  do.call(rbind, rows)
}

# Every candidate line is judged against every arc, not just the one that
# defined it. This shows all 25 residuals at once. A candidate's residual at
# its own anchor is exactly zero, by construction.
boscovich_residual_table <- function(arcs) {
  arcs <- add_sin2_latitude(arcs)
  lines <- boscovich_candidate_lines(arcs)

  table_values <- vapply(seq_len(nrow(lines)), function(i) {
    arcs$arc_toises -
      (lines$intercept_A[i] + lines$slope_B[i] * arcs$sin2_latitude)
  }, numeric(nrow(arcs)))

  out <- as.data.frame(t(table_values), stringsAsFactors = FALSE)
  names(out) <- arcs$place
  cbind(
    data.frame(candidate_anchored_at = lines$anchor, stringsAsFactors = FALSE),
    out
  )
}

# Pick the winner from any table of lines, under whichever criterion you name.
# "absolute" is Boscovich's rule; "squared" is Legendre's.
best_line <- function(lines, criterion = c("absolute", "squared")) {
  criterion <- match.arg(criterion)
  column <- if (criterion == "absolute") {
    "sum_abs_residuals"
  } else {
    "sum_squared_residuals"
  }
  if (!column %in% names(lines)) {
    stop("That table has no scores in it -- build it with a helper first.",
         call. = FALSE)
  }
  lines[which.min(lines[[column]]), , drop = FALSE]
}

# The pre-1750 habit: trust two measurements and ignore the rest. Any two arcs
# fix A and B exactly, and with five arcs there are choose(5, 2) = 10 such
# pairs, few enough to compute every one.
all_pair_lines <- function(arcs) {
  arcs <- add_sin2_latitude(arcs)
  x <- arcs$sin2_latitude
  y <- arcs$arc_toises
  pairs <- utils::combn(nrow(arcs), 2)

  rows <- lapply(seq_len(ncol(pairs)), function(k) {
    i <- pairs[1, k]
    j <- pairs[2, k]
    slope <- (y[j] - y[i]) / (x[j] - x[i])
    intercept <- y[i] - slope * x[i]
    cbind(
      data.frame(
        pair = paste(arcs$place[i], arcs$place[j], sep = " + "),
        stringsAsFactors = FALSE
      ),
      score_line(arcs, intercept, slope,
                 label = paste(arcs$place[i], "+", arcs$place[j]))
    )
  })

  do.call(rbind, rows)
}

# Mayer's method, transplanted onto Boscovich's problem. Two unknowns need two
# groups, not three. Sort the arcs by sin^2(latitude), put the lowest
# low_group_size of them in the first group and the rest in the second, then
# average each group and solve the resulting 2 x 2 system exactly.
#
# Averages, not sums: the groups here have different sizes, and dividing by the
# group size keeps both equations in the same A + B*x = y shape so neither group
# is weighted more just for being bigger. (Mayer's own groups all had nine
# equations, so the question never came up for him.)
mayer_split_line <- function(arcs, low_group_size) {
  arcs <- add_sin2_latitude(arcs)
  n <- nrow(arcs)

  if (!is.numeric(low_group_size) || length(low_group_size) != 1 ||
      is.na(low_group_size) || low_group_size != round(low_group_size) ||
      low_group_size < 1 || low_group_size > n - 1) {
    stop(paste0("low_group_size must be one whole number from 1 to ", n - 1,
                ", so that both groups get at least one arc."), call. = FALSE)
  }

  sorted <- order(arcs$sin2_latitude)
  group_1 <- sorted[seq_len(low_group_size)]
  group_2 <- sorted[(low_group_size + 1):n]

  mean_1_x <- mean(arcs$sin2_latitude[group_1])
  mean_2_x <- mean(arcs$sin2_latitude[group_2])
  mean_1_y <- mean(arcs$arc_toises[group_1])
  mean_2_y <- mean(arcs$arc_toises[group_2])

  coefficient_matrix <- matrix(c(1, 1, mean_1_x, mean_2_x), nrow = 2)
  answer <- tryCatch(
    solve(coefficient_matrix, c(mean_1_y, mean_2_y)),
    error = function(e) NULL
  )
  if (is.null(answer) || any(!is.finite(answer))) {
    stop("The two group averages do not separate enough to solve for A and B.",
         call. = FALSE)
  }

  label <- paste0("Mayer split ", low_group_size, "/", n - low_group_size)
  cbind(
    data.frame(
      split = label,
      group_1 = paste(arcs$place[group_1], collapse = " + "),
      group_2 = paste(arcs$place[group_2], collapse = " + "),
      group_1_mean_sin2 = mean_1_x,
      group_2_mean_sin2 = mean_2_x,
      coefficient_spread = mean_2_x - mean_1_x,
      stringsAsFactors = FALSE
    ),
    score_line(arcs, answer[1], answer[2], label = label)
  )
}

# Stack any collection of scored lines into one comparison table, keeping only
# the columns they all share. Feed it whole rows from the helpers above.
compare_methods <- function(...) {
  rows <- list(...)
  if (length(rows) == 0) stop("Give compare_methods() at least one line.",
                              call. = FALSE)

  shared <- c("method", "intercept_A", "slope_B", "sum_abs_residuals",
              "sum_squared_residuals", "residual_total", "one_over_ellipticity")

  trimmed <- lapply(rows, function(row) {
    if (!all(shared %in% names(row))) {
      stop("Every row must be a scored line from one of the Problem 2 helpers.",
           call. = FALSE)
    }
    row[, shared, drop = FALSE]
  })

  out <- do.call(rbind, trimmed)
  rownames(out) <- NULL
  out
}

# The five arcs, plus one line for every row of `lines`. Pass a whole table of
# candidates to see them all at once, or a single row to see just the winner.
plot_arc_lines <- function(arcs, lines = NULL,
                           main = "Five meridian arcs") {
  arcs <- add_sin2_latitude(arcs)

  plot(
    arcs$sin2_latitude, arcs$arc_toises,
    pch = 16, cex = 1.3,
    xlab = "sin^2(latitude)",
    ylab = "length of one degree of the meridian (toises)",
    main = main
  )
  text(arcs$sin2_latitude, arcs$arc_toises, labels = arcs$place,
       pos = 4, cex = 0.7, xpd = NA)

  if (!is.null(lines) && nrow(lines) > 0) {
    line_colours <- grDevices::hcl.colors(nrow(lines), palette = "Dark 3")
    for (i in seq_len(nrow(lines))) {
      abline(a = lines$intercept_A[i], b = lines$slope_B[i],
             col = line_colours[i], lwd = 2)
    }
    legend("topleft", legend = lines$method, col = line_colours,
           lwd = 2, cex = 0.7, bty = "n")
  }

  invisible(NULL)
}


# ============================================================================
# Two-decimal printing
# ============================================================================
#
# Every table below is printed rounded to two decimals, so that a wide table
# fits on one row instead of spilling its last few columns onto extra lines.
#
# ONLY THE PRINTING IS ROUNDED. The values inside keep their full precision,
# so anything you compute from a column -- an error, a difference, a plot --
# is unaffected by this.

# R wraps printed output at 80 characters by default, which is what pushes the
# last few columns of a wide table onto extra rows. An HTML code block scrolls
# sideways, so there we can afford a wide setting and every table prints as one
# row per record. A PDF page cannot scroll, so when rendering to PDF we fall
# back to a width that fits the paper and accept some wrapping.
options(width = if (isTRUE(try(knitr::is_latex_output(), silent = TRUE))) 85 else 250)

hw_table <- function(d) {
  if (!inherits(d, "hw_table")) class(d) <- c("hw_table", class(d))
  d
}

print.hw_table <- function(x, ..., digits = 2) {
  plain <- as.data.frame(x)
  numeric_columns <- vapply(plain, is.numeric, logical(1))
  plain[numeric_columns] <- lapply(plain[numeric_columns], round, digits)
  print.data.frame(plain, ...)
  invisible(x)
}

# Give every table-returning helper above that printing behaviour, without
# changing what any of them computes.
local({
  tabular <- c(
    "sum_mayer_equations", "solve_mayer_sums", "solve_mayer_rows",
    "repeat_random_partitions", "repeat_random_triples",
    "summarize_alpha_results", "score_line", "boscovich_candidate_lines",
    "boscovich_residual_table", "best_line", "all_pair_lines",
    "mayer_split_line", "compare_methods"
  )
  where <- environment(sum_mayer_equations)
  for (name in tabular) {
    original <- get(name, envir = where)
    assign(name, local({
      f <- original
      function(...) hw_table(f(...))
    }), envir = where)
  }
})
