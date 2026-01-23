


#'Plot Read Lengths
#'
#' @param my_data
#'
#' @returns plot
#' @import dplyr, ggplot2
#' @export
#'
#' @examples
#' \dontrun {
#'  plot_length <- lapply(seq_sum_database, plot_read_lengths)
#' }
plot_read_lengths <- function(my_data){
  mean_length = mean(my_data$sequence_length_template)
  max_y_mean <- max(count_seq$n, na.rm = TRUE)
  n50_SR <- Biostrings::N50(my_data$sequence_length_template)
  max_y_n50 <- max(count_seq$n, na.rm = TRUE)
  count_seq <- my_data %>% dplyr::count(sequence_length_template)
  sample_name <-  dplyr::first(my_data$sample_id)

  data <- ggplot2::ggplot(data = count_seq) + ggplot2::aes(x = sequence_length_template, y = n)
  bar_color <- ggplot2::geom_col(color = "skyblue")
  mean_lengt <- ggplot2::geom_vline(ggplot2::aes(xintercept = mean_length), color = "purple")
  n50_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = n50_SR), color = "darkgreen")
  axis_limit <- ggplot2::coord_cartesian(xlim = c(0, 4000), ylim = c(0, 10000))
  plot_lable <-  ggplot2::labs(title =paste0("Length plot  ", sample_name), x = "Sequence length [nt]", y = "No of bases sequenced")

  mean_label <- ggplot2::annotate(geom = "text", x = mean_length* 2.5, y = max_y_mean * 0.65, label = paste("Mean Length"), color = "purple", fontface = "bold", size = 4)
  n50_label <- ggplot2::annotate(geom = "text", x = n50_SR* 2.5, y = max_y_n50 * 0.65, label = paste("N50"), color = "darkgreen", fontface = "bold", size = 4)

  length_plot <- data + bar_color + mean_lengt + n50_line + axis_limit + plot_lable + mean_label + n50_label
  length_plot
}
