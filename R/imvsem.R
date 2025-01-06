#' Calculate IMV Using Two Models
#'
#' Computes the Information Measure for Validation (IMV) by comparing two models
#' across specified variables using cross-validation or in-sample prediction.
#'
#' @param model1 Character. The first model (lavaan syntax).
#' @param model2 Character. The second model (lavaan syntax).
#' when model2 = NULL, the comparison will be conducted between model1 and a baaseline model
#' @param vary Character vector. Names of observed variables.
#' @param data Data frame. The dataset used for training/testing.
#' @param seed Integer. Seed for random sampling (default = `1234`).
#' @param nfold Integer. Number of folds for cross-validation (default = `5`,
#' when nfold = 1, the IMV will be based on insample prediction instead of out of sample prediction).
#'
#' @examples
#' # Load necessary libraries
#' library(lavaan)
#' library(stats)
#' library(imv)
#'
#' # Load the dataset from the inst/extdata folder
#' data_path <- system.file("extdata", "BPI.dat", package = "imv4sem")
#' data <- read.table(data_path, header = TRUE)
#'
#' # Define the variables to be analyzed
#' vary <- colnames(data)
#'
#' # Define the first model (6 factors)
#' model_6f <- '
#' AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
#' Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
#' Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
#' Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
#' PeerProb =~ NA*pp1 + pp2 + pp3
#' Depend =~ NA*de1 + de2 + de3 + de4
#' AnxDep ~~ 1*AnxDep
#' Headstr ~~ 1*Headstr
#' Antisoc ~~ 1*Antisoc
#' Hyperac ~~ 1*Hyperac
#' PeerProb ~~ 1*PeerProb
#' Depend ~~ 1*Depend
#' '
#'
#' # Define the second model (5 factors)
#' model_5f <- '
#' AnxDep =~ NA*ad1 + ad2 + ad3 + ad4 + ad5
#' Headstr =~ NA*hs1 + hs2 + hs3 + hs4 + hs5
#' Antisoc =~ NA*as1 + as2 + as3 + as4 + as5 + as6
#' Hyperac =~ NA*hy1 + hy2 + hy3 + hy4 + hy5
#' PeerProb =~ NA*pp1 + pp2 + pp3 + de1 + de2 + de3 + de4
#' AnxDep ~~ 1*AnxDep
#' Headstr ~~ 1*Headstr
#' Antisoc ~~ 1*Antisoc
#' Hyperac ~~ 1*Hyperac
#' PeerProb ~~ 1*PeerProb
#' '
#'
#' # Calculate IMV between 5-factor model and 6-factor model using out-of-sample prediction
#' imv56f <- imvsem(model1 = model_5f, model2 = model_6f, vary = vary, data = data, nfold = 5)
#' # Calculate IMV between 5-factor model and 6-factor model using in-sample prediction
#' imv56f_in <- imvsem(model1 = model_5f, model2 = model_6f, vary = vary, data = data, nfold = 1)
#' # Calculate IMV between 6-factor model and baseline model using out-of-sample prediction
#' imv6f_base <- imvsem(model1 = model_6f, vary = vary, data = data, nfold = 5)
#'
#' @return A matrix of baseline IMV values for each variable and scale-level imv values ("all").
#' @importFrom lavaan cfa lavPredict inspect
#' @importFrom stats pnorm
#' @importFrom imv imv.binary
#' @export
imvsem <- function(model1, model2 = NULL, vary, data, seed = 1234, nfold = 5) {
  if (!all(vary %in% colnames(data))) {
    stop("All variables in 'vary' must exist in 'data'.")
  }
  if (nfold <= 0 || nfold > nrow(data)) {
    stop("'nfold' must be between 1 and the number of rows in 'data'.")
  }

  NY <- length(vary) #  Number of observed variables
  if (is.null(model2)) {
    # Baseline IMV calculation logic (formerly imv_baseline)
    if (nfold > 1) {
      # Cross-validation logic
      set.seed(seed) # Ensure reproducibility
      data$group <- sample(1:nfold, nrow(data), replace = TRUE) # Assign fold indices
      data_lst <- split(data, data$group) # Split data into folds
      imv <- NULL
      for (foldi in 1:nfold) {
        # Prepare training and testing datasets for the fold
        test <- data_lst[[foldi]][, vary, drop = FALSE]
        train <- data.frame(do.call("rbind", data_lst[-foldi]))[, vary, drop = FALSE]
        imv_foldi <- NULL
        # Predict probabilities for the model
        yprob <- predict_sem(train, test, model1, vary)
        if (sum(is.na(yprob)) == 0) {
          # Calculate IMV for each variable
          for (varyi in vary) {
            imv.temp <- imv.binary(as.vector(test[, varyi]),
                                   rep(mean(test[, varyi]), length(test[, varyi])),
                                   yprob[, varyi])
            imv_foldi <- c(imv_foldi, imv.temp)
          }
          # Calculate overall IMV across variables
          imv.allpar <- mean(imv_foldi)
          imv_foldi <- c(imv_foldi, imv.allpar)
        } else {
          # Handle NA predictions
          imv_foldi <- rep(NA, length(vary) + 1)
        }
        names(imv_foldi) <- c(vary, 'all') # Add variable names and overall label
        imv <- rbind(imv, c(imv_foldi, fold = foldi)) # Append results for this fold
      }
    } else {
      # In-sample prediction logic
      yprob <- predict_insample(data, model1, vary)
      imv <- NULL
      for (varyi in vary) {
        imv.temp <- imv.binary(as.vector(data[, varyi]),
                               rep(mean(data[, varyi]), length(data[, varyi])),
                               yprob[, varyi])
        imv <- c(imv, imv.temp)
      }
      imv_all <- mean(imv) # Scale-level IMV
      imv <- c(imv, imv_all, 1) # Append scale-level IMV and fold
      names(imv) <- c(vary, 'all', "fold")
      imv <- as.matrix(t(imv)) # Convert to single-row matrix
    }
  }else {
    # IMV calculation logic for comparing two models
    if (nfold > 1) {
      # Cross-validation logic
      set.seed(seed) # Ensures reproducibility
      data$group <- sample(1:nfold, nrow(data), replace = TRUE) # Assign fold indices
      data_lst <- split(data, data$group) # Split data into folds
      imv <- NULL
      for (foldi in 1:nfold) {
        # Prepare training and testing datasets for the fold
        test <- data_lst[[foldi]][, vary, drop = FALSE]
        train <- data.frame(do.call("rbind", data_lst[-foldi]))[, vary, drop = FALSE]
        # Calculate IMV for the fold
        imv_foldi <- fold(train, test, model1, model2, vary)
        # Append results for this fold
        imv <- rbind(imv, c(imv_foldi, "fold" = foldi))
      }
    } else {
      # In-sample prediction logic
      imv <- NULL
      # Predict probabilities for both models
      yprob1 <- predict_insample(data, model1, vary)
      yprob2 <- predict_insample(data, model2, vary)
      for (varyi in vary) {
        # Calculate IMV for each variable
        imv.temp <- imv.binary(as.vector(data[, varyi]), yprob1[, varyi], yprob2[, varyi])
        imv <- c(imv, imv.temp)
      }
      # Calculate overall IMV across variables
      imv.allpar <- mean(imv)
      imv <- c(imv, imv.allpar, 1)
      names(imv) <- c(vary, 'all', "fold") # Add variable names and overall label
      imv <- as.matrix(t(imv))
      return(imv)
    }
  }
  return(imv)
}
