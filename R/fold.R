#' Supporting function - fold()
#'
#' Performs cross-validation to compute IMV for two models.
#'
#' @param train Data frame. Training dataset.
#' @param test Data frame. Testing dataset.
#' @param model1 Character. The first model (lavaan syntax).
#' @param model2 Character. The second model (lavaan syntax).
#' @param vary Character vector. Names of observed variables.
#' @param varx Character. Optional. Names of covariates (default = `NA`).
#'
#' @return A named vector of IMV values for each variable and a summary value ("all").
#' @examples
#' # Example usage is covered by higher-level functions imvsem and imv_baseline.
#' @importFrom lavaan cfa lavPredict inspect
#' @importFrom stats pnorm
#' @export
fold <- function(train, test, model1, model2, vary, varx = NA) {
  imv <- NULL
  # Predict probabilities for both models
  yprob1 <- predict_sem(train, test, model1, vary)
  yprob2 <- predict_sem(train, test, model2, vary)
  if (sum(is.na(yprob1)) == 0 & sum(is.na(yprob2)) == 0) {
    for (varyi in vary) {
      # Compute IMV for each variable
      imv.temp <- imv.binary(as.vector(test[, varyi]), yprob1[, varyi], yprob2[, varyi])
      imv <- c(imv, imv.temp)
    }
    # Compute overall IMV
    imv.allpar <- mean(imv)
    imv <- c(imv, imv.allpar)
  } else {
    # Handle NA cases
    imv <- rep(NA, length(vary) + 1)
  }
  names(imv) <- c(vary, 'all')
  return(imv)
}
