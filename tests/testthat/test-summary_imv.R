
# Test the summary.imv and print.summary_imv functions
test_that("summary.imv correctly summarizes IMV results", {
  # Example IMV matrix
  imvres <- as.matrix(t(c(ad1 = 0.02, ad2 = 0.03, ad3 = 0.05, ad4 = 0.01, all = 0.03)))

  # Call summary.imv
  summary_result <- summary.imvsem(imvres)

  # Test scale-level IMV
  expect_equal(summary_result$scale_imv, 0.03, info = "Scale-level IMV should match the 'all' column average.")

  # Test item-level IMV
  expect_equal(
    summary_result$item_level_imv,
    data.frame(Item = c("ad1", "ad2", "ad3", "ad4"), IMV = c(0.02, 0.03, 0.05, 0.01)),
    info = "Item-level IMV summary should include correct variable names and averages."
  )

  # Test number of items
  expect_equal(summary_result$n_items, 4, info = "The number of items should be correctly calculated.")
})
