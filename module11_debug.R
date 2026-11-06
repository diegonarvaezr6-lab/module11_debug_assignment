
# Assignment 11 - Debugging in R
# Module 11: Logical Operators and Defensive Programming

# Tukey outlier helper function
tukey.outlier <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- IQR(x)

  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

# Step 1: Original buggy function
tukey_multiple <- function(x) {
  outliers <- array(TRUE, dim = dim(x))

  for (j in 1:ncol(x)) {
    outliers[, j] <- outliers[, j] && tukey.outlier(x[, j])
  }

  outlier.vec <- vector("logical", length = nrow(x))

  for (i in 1:nrow(x)) {
    outlier.vec[i] <- all(outliers[i, ])
  }

  return(outlier.vec)
}

# Step 2: Reproduce the bug
set.seed(123)
test_mat <- matrix(rnorm(50), nrow = 10)

cat("Original buggy function:\n")
try(tukey_multiple(test_mat))

# Step 3: Corrected function
corrected_tukey <- function(x) {

  # Defensive checks
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("Input must be a numeric matrix.")
  }

  if (nrow(x) == 0 || ncol(x) == 0) {
    stop("Matrix must not be empty.")
  }

  if (anyNA(x) || any(!is.finite(x))) {
    stop("Matrix must contain only finite numeric values.")
  }

  outliers <- array(TRUE, dim = dim(x))

  for (j in seq_len(ncol(x))) {
    outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])
  }

  outlier.vec <- logical(nrow(x))

  for (i in seq_len(nrow(x))) {
    outlier.vec[i] <- all(outliers[i, ])
  }

  outlier.vec
}

# Step 4: Test corrected function
cat("\nCorrected function output:\n")
result <- corrected_tukey(test_mat)
print(result)

cat("\nOutput length:", length(result), "\n")
cat("Is logical:", is.logical(result), "\n")

# Step 5: Test defensive programming
cat("\nTesting invalid input:\n")
try(corrected_tukey("invalid input"))

cat("\nAssignment 11 completed.\n")
