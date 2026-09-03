# ============================================================================
# Helper functions for Lab 01
# ============================================================================
#
# Base R only: you do not need to install anything.
#
# Load this file once, near the top of your document:
#
#     source("lab-01-helpers.R")
#
# NONE OF THESE ARE R FUNCTIONS. They were written for this course, so `?`
# will not find them and neither will searching online. They exist only after
# the source() line above has run.
#
# WHAT ONE ROW OF THE DATA IS. Each row is one equation:
#
#     a * x1  +  b * x2  +  c * x3  =  rhs
#
# `a`, `b`, `c` and `rhs` are the four columns, and they are MEASURED NUMBERS --
# known from the start, different in every row. `x1`, `x2`, `x3` are the three
# UNKNOWNS, the same three in all 27 rows, and they are what solve() hands back.
# The measured numbers are not the answer and never get estimated; they are the
# question.
#
# There are only four of them, and they do nothing you could not do by hand:
#
#   check_equations(practice_df)        Check the CSV loaded correctly
#   show_equation(practice_df, row)     Print one row as readable algebra
#   solve_rows(practice_df, rows)       Build a 3x3 from three rows and solve it
#   sum_rows(practice_df, rows)         Add several rows into one summed equation
#
# Inside the definitions below that first argument is called `equations`, not
# `practice_df`. That is on purpose and it is not a mistake: a function names
# its own inputs, and these three would work on any table of equations shaped
# like ours. What you pass in is `practice_df`; what the function calls it
# while it works is its own business.
#
# Open this file and read solve_rows() once you have done Exercise 2 by hand.
# You will recognize every line of it.
#
# ============================================================================


# Checks that the file you loaded is the one this lab expects. Stops with a
# readable message if something is off, and says nothing at all if all is well.
# "No output" is the good outcome here.
check_equations <- function(equations) {
  required <- c("obs", "a", "b", "c", "rhs")

  missing <- setdiff(required, names(equations))
  if (length(missing) > 0) {
    stop(paste("These columns are missing:", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if (!all(vapply(equations[required], is.numeric, logical(1)))) {
    stop("Every column should be numeric.", call. = FALSE)
  }
  if (anyNA(equations[required])) {
    stop("The data contain a missing value.", call. = FALSE)
  }
  if (nrow(equations) != 27) {
    stop(paste("Expected 27 equations, but this file has", nrow(equations),
               "rows."), call. = FALSE)
  }

  message("Looks right: 27 equations, 3 unknowns, no missing values.")
  invisible(TRUE)
}


# Writes one of the 27 equations out as algebra, so you can see what a row of
# the CSV actually says. Give it the data and a row number.
show_equation <- function(equations, row) {
  if (length(row) != 1 || row < 1 || row > nrow(equations)) {
    stop(paste0("Pick one row number between 1 and ", nrow(equations), "."),
         call. = FALSE)
  }

  paste0(
    "Equation ", equations$obs[row], ":  ",
    equations$a[row], " * x1  ",
    ifelse(equations$b[row] < 0, "- ", "+ "), abs(equations$b[row]), " * x2  ",
    ifelse(equations$c[row] < 0, "- ", "+ "), abs(equations$c[row]), " * x3  ",
    "=  ", equations$rhs[row]
  )
}


# Picks three equations out of the 27 and solves them exactly.
#
# This is the whole of Exercise 2, packaged: it pulls the three rows you name,
# turns their measured a, b and c values into a 3x3 matrix, and hands that to
# solve() along with the right-hand sides. Compare it against your own by-hand version --
# it should give the same three numbers, because it IS the same three lines.
solve_rows <- function(equations, rows) {
  if (length(rows) != 3 || length(unique(rows)) != 3) {
    stop("Choose exactly three different row numbers.", call. = FALSE)
  }
  if (!all(rows %in% seq_len(nrow(equations)))) {
    stop(paste0("Row numbers must be between 1 and ", nrow(equations), "."),
         call. = FALSE)
  }

  A <- as.matrix(equations[rows, c("a", "b", "c")])
  b <- equations$rhs[rows]

  answer <- solve(A, b)
  names(answer) <- c("x1", "x2", "x3")
  answer
}


# Adds several equations together to make one equation.
#
# Give it the data and the row numbers you want added. It adds those rows up
# column by column -- all the a's, all the b's, all the c's, and all the
# right-hand sides -- and hands back one equation's worth of numbers.
#
# Note what it does NOT touch: x1, x2 and x3. Adding equations together changes
# the measured numbers in front of the unknowns; the unknowns themselves are the
# same three quantities they always were.
#
# This is Mayer's whole idea in one function. The hard part was never the
# addition; it was believing you were allowed to do it.
sum_rows <- function(equations, rows) {
  if (length(rows) < 2) {
    stop("Give at least two row numbers to add together.", call. = FALSE)
  }
  if (!all(rows %in% seq_len(nrow(equations)))) {
    stop(paste0("Row numbers must be between 1 and ", nrow(equations), "."),
         call. = FALSE)
  }

  c(colSums(equations[rows, c("a", "b", "c")]),
    rhs = sum(equations$rhs[rows]))
}
