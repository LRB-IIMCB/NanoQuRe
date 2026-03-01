
#' Plot Quality Distribution
#' Generates a plot with number of reads of each length and their average quality score Q score sorted by pass/fail filtering status. Vertical line represents Q score cut-off which is by default equal to 7.
#'
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import ggplot2
#' @import dplyr
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_quality_distribution <- function(my_data){

  assertthat::assert_that(nrow(my_data) > 0, msg = "The input data frame is empty.")

  assertthat::assert_that(assertthat::has_name(my_data, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(assertthat::has_name(my_data, "sample_id"), msg = "Missing 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(my_data, "mean_qscore_template"), msg = "The data frame is missing the 'mean_qscore_template' column")
  assertthat::assert_that(assertthat::has_name(my_data, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")

  assertthat::assert_that(is.numeric(my_data$mean_qscore_template), msg = "Q-score column must be numeric")


  sample_name <-  dplyr::first(my_data$sample_id)
  length_qscore <- my_data %>% dplyr::select(c(sequence_length_template, mean_qscore_template, passes_filtering))
  sorted_lenq <- length_qscore %>% dplyr::arrange(length_qscore$mean_qscore_template)

  data <- ggplot2::ggplot(data = sorted_lenq) + ggplot2::aes(x = mean_qscore_template, fill = passes_filtering)
  pass_fail <- ggplot2::geom_histogram(binwidth = 0.09, color = "black", position = "stack")
  qscore_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = 7), color = "orange")
  y_axis <- nrow(my_data)*0.05
  axis_limit <- ggplot2::coord_cartesian(xlim = c(0, 15), ylim = c(0, y_axis))
  plot_lable <-  ggplot2::labs(title = paste0("Quality distribution ", sample_name), x = "Mean Q score of read", y = "Number of reads")

  qscore_label <- ggplot2::annotate(geom = "text", x = 8.2, y = y_axis*0.95, label = paste("Q score cut-off"), color = "orange", fontface = "bold", size = 4)

  length_plot <- data + pass_fail + qscore_line + axis_limit + plot_lable + n50_label
  return(length_plot)
}


#plot1 <- plot_quality_distribution(ligase_ela)
#plot1

#plot2 <- plot_quality_distribution(sample_data)
#plot2
