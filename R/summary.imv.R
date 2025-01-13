#' Summarize IMV Results
#'
#' This function provides a summary of IMV values for a given object.
#'
#' @param object Matrix or data frame. The output from the `imvsem` or `imv_baseline` function.
#' @param ... Additional arguments (not used).
#'
#' @return A list of class `summary_imvsem` containing the following components:
#' \item{scale_imv}{Scale-level IMV (average of the `all` column).}
#' \item{item_level_imv}{A data frame with item-level IMVs and their names.}
#' \item{n_items}{The number of item-level IMVs.}
#' @examples
#' # Example IMV result
#' imvres <- as.matrix(t(c(ad1 = 0.02, ad2 = 0.03, ad3 = 0.05, ad4 = 0.01, all = 0.03)))
#' summary_imvsem <- summary(imvres)
#' print(summary_imvsem)
#'
#' @export
summary_imvsem <- function(object, ...) {
  # Ensure input is a matrix or data frame
  if (!is.matrix(object) && !is.data.frame(object)) {
    stop("Input must be a matrix or data frame.")
  }

  # Convert to data frame if needed
  object <- as.data.frame(object)

  # Scale-level IMV
  scale_imv <- mean(object[["all"]], na.rm = TRUE)

  # Item-level IMVs
  item_level_imv <- object[, !colnames(object) %in% c("all", "fold"), drop = FALSE]
  item_avg <- colMeans(item_level_imv, na.rm = TRUE)
  item_level_summary <- data.frame(Item = names(item_avg), IMV = round(item_avg,4))
  rownames(item_level_summary) <- NULL

  # Number of items
  n_items <- ncol(item_level_imv)

  # Return summary as a list
  result <- list(
    scale_imv = scale_imv,
    item_level_imv = item_level_summary,
    n_items = n_items
  )
  class(result) <- "summary_imvsem"
  return(result)
}

#' Print Summary for IMV
#'
#' Custom print method for objects of class `summary_imvsem`.
#'
#' @param x An object of class `summary_imvsem` obtained from summary.imv().
#' @param ... Additional arguments (not used).
#' @return printed results for IMV values
#'
#' @export
print.summary_imvsem <- function(x, ...) {
  if (!inherits(x, "summary_imvsem")) {
    stop("Input must be of class 'summary_imvsem'.")
  }

  cat("# IMV values\n")
  cat("# Number of items =", x$n_items, "\n")
  cat("# Scale-level IMV =", round(x$scale_imv, 4), "\n")
  cat("# Item-level IMV:\n")
  print(x$item_level_imv)
}
