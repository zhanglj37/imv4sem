#' Supporting function - predict_insample()
#'
#' Predicts probabilities for the dataset used for fitting the CFA model.
#'
#' @param data Data frame. Dataset used for fitting.
#' @param model Character. CFA model (lavaan syntax).
#' @param vary Character vector. Names of observed variables.
#'
#' @return A matrix of predicted probabilities.
#' @examples
#' # Example usage is covered by higher-level functions imvsem and imv_baseline.
#' @importFrom lavaan cfa lavPredict inspect
#' @importFrom stats pnorm
#' @export
predict_insample <- function(data, model, vary) {
  fit_train <- cfa(model, data = data, ordered = colnames(data), parameterization = "delta")
  ov_pred <- lavPredict(fit_train, type = 'ov')
  y_threshold <- inspect(fit_train, what = "est")$tau
  y_residual <- inspect(fit_train, what = "est")$theta
  yprob <- NULL
  for (varyi in vary) {
    i <- which(colnames(data) == varyi)
    ov_pred[, i] <- ov_pred[, i] - y_threshold[i]
    temp <- pnorm(ov_pred[, i] / sqrt(y_residual[i, i]))
    yprob <- cbind(yprob, temp)
  }
  colnames(yprob) <- vary
  return(yprob)
}
