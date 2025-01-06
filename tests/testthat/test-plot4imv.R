
# Test the plot function
test_that("plot creates a valid ggplot object", {
  # Load necessary libraries
  library(ggplot2)

  # Test Case 1: Input as a matrix
  imvval_matrix <- matrix(
    c(
      0.5515242, 0.11274523, 0.1258842, 0.0620904700, 0.06808110, 0.1731388,
      0.6584795, 0.12968836, 0.1474620, 0.0009655212, 0.12936124, 0.1669361,
      0.6552690, 0.08138187, 0.1284782, 0.1388661603, 0.17258212, 0.2009063,
      0.5482656, 0.07689373, 0.1205227, 0.0777095088, 0.07991321, 0.1960324,
      0.3345910, 0.14111976, 0.1464525, 0.1287349535, 0.12528330, 0.1594424
    ),
    ncol = 6,
    byrow = TRUE,
    dimnames = list(NULL, c("ad1", "ad2", "ad3", "ad4", "ad5", "all"))
  )
  # Run the function
  p1 <- plot4imv(imvval_matrix, threshold_item = 0.01, threshold_all = 0.001)
  # Check if the output is a ggplot object
  expect_s3_class(p1, "ggplot")

  # Test Case 2: Input as a vector
  imvval_vector <- as.matrix(t(c(ad1 = 0.02, ad2 = 0.03, ad3 = 0.05, ad4 = 0.01, all = 0.002)))
  # Run the function
  p2 <- plot4imv(imvval_vector, threshold_item = 0.01, threshold_all = 0.001)
  # Check if the output is a ggplot object
  expect_s3_class(p2, "ggplot")

})

