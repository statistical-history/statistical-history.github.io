draw_regression_study <- function() {
  train_x <- matrix(rnorm(28 * 50), nrow = 28)
  test_x <- matrix(rnorm(160 * 50), nrow = 160)
  predictor_names <- paste0("X", seq_len(50))
  colnames(train_x) <- predictor_names
  colnames(test_x) <- predictor_names

  train_y <- 2 * train_x[, "X1"] - 1.5 * train_x[, "X2"] + rnorm(28)
  test_y <- 2 * test_x[, "X1"] - 1.5 * test_x[, "X2"] + rnorm(160)

  list(train = data.frame(Y = train_y, train_x),
       test = data.frame(Y = test_y, test_x))
}
