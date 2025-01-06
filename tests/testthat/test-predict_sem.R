

# Test the predict_sem function
test_that("predict_sem correctly predicts probabilities", {

  # Load dataset
  data_path <- system.file("extdata", "BPI.dat", package = "imv4sem")
  expect_true(file.exists(data_path), info = "Dataset file exists in extdata")
  data <- read.table(data_path, header = TRUE)

  # Define observed variables
  vary <- colnames(data)

  # Define the model
  model <- '
  AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
  Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
  Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
  Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
  PeerProb =~ NA*pp1 + pp2 + pp3
  Depend =~ NA*de1 + de2 + de3 + de4
  AnxDep ~~ 1*AnxDep
  Headstr ~~ 1*Headstr
  Antisoc ~~ 1*Antisoc
  Hyperac ~~ 1*Hyperac
  PeerProb ~~ 1*PeerProb
  Depend ~~ 1*Depend
  '

  # Split data into training and testing datasets
  set.seed(123)
  train_indices <- sample(1:nrow(data), size = round(0.7 * nrow(data)))
  train <- data[train_indices, ]
  test <- data[-train_indices, ]

  # Call predict_sem
  yprob <- predict_sem(train = train, test = test, model = model, vary = vary)

  # Test if output is a matrix
  expect_true(is.matrix(yprob), info = "Output should be a matrix.")

  # Test if column names match 'vary'
  expect_equal(colnames(yprob), vary, info = "Column names should match the 'vary' argument.")

  # Test if matrix has the correct number of rows
  expect_equal(nrow(yprob), nrow(test), info = "Number of rows in the output should match the number of rows in the 'test' dataset.")

  # Test if probabilities are in range [0, 1]
  expect_true(all(yprob >= 0 & yprob <= 1), info = "All probabilities should be between 0 and 1.")

})
