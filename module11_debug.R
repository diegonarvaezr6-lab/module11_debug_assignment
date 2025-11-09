
R version 4.5.1 (2025-06-13) -- "Great Square Root"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.82 (8536) aarch64-apple-darwin20]

[Workspace restored from /Users/diegonarvaez/.RData]
[History restored from /Users/diegonarvaez/.Rapp.history]

> tukey.outlier <- function(x) {
+   stopifnot(is.numeric(x))
+   q1  <- quantile(x, 0.25, na.rm = TRUE)
+   q3  <- quantile(x, 0.75, na.rm = TRUE)
+   iqr <- q3 - q1
+   (x < q1 - 1.5 * iqr) | (x > q3 + 1.5 * iqr)
+ }
> 
> tukey_multiple <- function(x) {
+   outliers <- array(TRUE, dim = dim(x))
+   for (j in 1:ncol(x)) {
+     outliers[, j] <- outliers[, j] && tukey.outlier(x[, j])  # <-- BUG: usa &&
+   }
+   outlier.vec <- vector("logical", length = nrow(x))
+   for (i in 1:nrow(x)) {
+     outlier.vec[i] <- all(outliers[i, ])
+   }
+   return(outlier.vec)
+ }
> 
> set.seed(123)
> test_mat <- matrix(rnorm(50), nrow = 10)
> tukey_multiple(test_mat)
Error in outliers[, j] && tukey.outlier(x[, j]) : 
  'length = 10' in coercion to 'logical(1)'
> 
> corrected_tukey <- function(x) {
+   outliers <- array(TRUE, dim = dim(x))
+   for (j in seq_len(ncol(x))) {
+     outliers[, j] <- outliers[, j] & tukey.outlier(x[, j])  # FIX: usa &
+   }
+   outlier.vec <- logical(nrow(x))
+   for (i in seq_len(nrow(x))) {
+     outlier.vec[i] <- all(outliers[i, ])
+   }
+   outlier.vec
+ }
> 
> corrected_tukey(test_mat)
 [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
> 
> 