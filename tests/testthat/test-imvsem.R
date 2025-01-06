# Test the imvsem function
test_that("imvsem calculates IMV correctly", {

  # Load a sample dataset from inst/extdata
  data_path <- system.file("extdata", "BPI.dat", package = "imv4sem")
  expect_true(file.exists(data_path), info = "Dataset file exists in extdata")

  data <- read.table(data_path, header = TRUE)

  # Define the observed variables
  vary <- colnames(data)

  # Define the first model (6 factors)
  model_6f <- '
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

  # Define the second model (5 factors)
  model_5f <- '
  AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
  Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
  Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
  Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
  PeerProb =~ NA*pp1 + pp2 + pp3 + de1 + de2 + de3 + de4
  AnxDep ~~ 1*AnxDep
  Headstr ~~ 1*Headstr
  Antisoc ~~ 1*Antisoc
  Hyperac ~~ 1*Hyperac
  PeerProb ~~ 1*PeerProb
  '

  # Test cross-validation IMV calculation
  set.seed(1)
  imv_result <- imvsem(model1 = model_5f, model2 = model_6f, vary = vary, data = data, nfold = 5)

  # Check the output structure
  expect_true(is.matrix(imv_result), info = "Output is a matrix")
  expect_true(all(vary %in% colnames(imv_result)), info = "All variables in `vary` are present in column names of the result")

  # Check the values (e.g., ensure no NA or NaN values)
  expect_false(any(is.na(imv_result)), info = "No NA values in the result")
})


# Test the imvsem function for baseline prediction
test_that("imvsem calculates IMV correctly", {

  # Load a sample dataset from inst/extdata
  data_path <- system.file("extdata", "BPI.dat", package = "imv4sem")
  expect_true(file.exists(data_path), info = "Dataset file exists in extdata")

  data <- read.table(data_path, header = TRUE)

  # Define the observed variables
  vary <- colnames(data)

  # Define the first model (6 factors)
  model_6f <- '
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


  # Test cross-validation IMV calculation
  set.seed(1)
  imv_result <- imvsem(model1 = model_6f, vary = vary, data = data, nfold = 5)

  # Check the output structure
  expect_true(is.matrix(imv_result), info = "Output is a matrix")
  expect_true(all(vary %in% colnames(imv_result)), info = "All variables in `vary` are present in column names of the result")

  # Check the values (e.g., ensure no NA or NaN values)
  expect_false(any(is.na(imv_result)), info = "No NA values in the result")
})
