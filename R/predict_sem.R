#' Supporting function - predict_sem()
#'
#' Predicts probabilities for a test dataset using a CFA model fitted on the training dataset.
#'
#' @param train Data frame. Training dataset.
#' @param test Data frame. Testing dataset.
#' @param model Character. CFA model (lavaan syntax).
#' @param vary Character vector. Names of observed variables.
#'
#' @return A matrix of predicted probabilities.
#' @examples
#' # Example usage is covered by higher-level functions imvsem and imv_baseline.
#' @importFrom lavaan cfa lavPredict inspect
#' @importFrom stats pnorm
#' @export
predict_sem <- function(train, test, model, vary) {
  fit_train <- cfa(model, data = train, ordered = colnames(train), parameterization = "delta")
  ov_pred <- lavPredict(fit_train, newdata = test, type = 'ov')
  y_threshold <- inspect(fit_train, what = "est")$tau
  y_residual <- inspect(fit_train, what = "est")$theta
  yprob <- NULL
  for (varyi in vary) {
    i <- which(colnames(train) == varyi)
    ov_pred[, i] <- ov_pred[, i] - y_threshold[i]
    temp <- pnorm(ov_pred[, i] / sqrt(y_residual[i, i]))
    yprob <- cbind(yprob, temp)
  }
  colnames(yprob) <- vary
  return(yprob)
}
