#' Plot Item-Level IMV with Thresholds and Scale-Level Note
#'
#' This function calculates the average IMV across all folds and creates a bar chart
#' for the item-level IMVs. The scale-level IMV is included as a note in the plot.
#'
#' @param imvres Matrix or vector. Output from the `imvsem` or `imv_baseline` function.
#' @return A ggplot object showing the item-level IMV bar chart with thresholds.
#'
#' @examples
#' # In-sample prediction IMV values
#' imvres <- as.matrix(t(c(ad1 = 0.02, ad2 = 0.03, ad3 = 0.05, ad4 = 0.01, all = 0.03)))
#' plot(imvres)
#'
#' @import ggplot2
#' @export
plot4imv <- function(imvres) {

  imvres_df <- as.data.frame(imvres)
  colnames(imvres_df)[ncol(imvres_df)] <- "fold" # Rename the last column as "fold"
  avg_imv <- colMeans(imvres_df[, -ncol(imvres_df)]) # Exclude "fold" column for averaging

  # Convert to a data frame for plotting
  avg_imv_df <- data.frame("Variable" = names(avg_imv), "IMV" = avg_imv)

  # Separate item-level and scale-level IMVs
  item_level <- avg_imv_df[avg_imv_df$Variable != "all", , drop = FALSE] # Exclude "all"
  scale_level <- avg_imv_df[avg_imv_df$Variable == "all", , drop = FALSE] # Only "all"

  # Extract scale-level IMV value for the note
  scale_imv_value <- round(scale_level$IMV, 3)
  scale_note <- paste0("Scale-Level IMV = ", scale_imv_value)

  # Create bar chart for item-level IMVs
  p_item <- ggplot(item_level, aes(x = .data$Variable, y = .data$IMV)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  #  geom_hline(yintercept = threshold_item, linetype = "dashed", color = "red") +
  #  annotate("text", x = 1, y = threshold_item + 0.005,
  #           label = paste("Threshold =", threshold_item), color = "red", hjust = 0) +
    labs(title = "Average Item-Level IMV with Threshold",
         subtitle = scale_note,
         x = "Item",
         y = "Average IMV") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1),
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 12, face = "italic"))

  return(p_item)
}
