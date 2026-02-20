
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
quality_distr <- function(my_data){

  assertthat::assert_that(my_data %has_name% "sequence_length_template", msg = "The data frame is missing the 'sequence_length_template' column")
  assertthat::assert_that(my_data %has_name% "mean_qscore_template", msg = "The data frame is missing the 'mean_qscore_template' column")
  assertthat::assert_that(my_data %has_name% "passes_filtering", msg = "The data frame is missing the 'passes_filtering' column")


  sample_name <-  dplyr::first(my_data$sample_id)
  length_qscore <- my_data %>% dplyr::select(c(sequence_length_template, mean_qscore_template, passes_filtering))
  sorted_lenq <- length_qscore %>% dplyr::arrange(length_qscore$mean_qscore_template)

  data <- ggplot2::ggplot(data = sorted_lenq) + ggplot2::aes(x = mean_qscore_template, fill = passes_filtering)
  pass_fail <- ggplot2::geom_histogram(binwidth = 0.09, color = "black", position = "stack")
  qscore_line <- ggplot2::geom_vline(ggplot2::aes(xintercept = 7), color = "orange")
  axis_limit <- ggplot2::coord_cartesian(xlim = c(0, 15), ylim = c(0, 40000))
  plot_lable <-  ggplot2::labs(title = paste0("Quality distribuion ", sample_name), x = "Mean Q score of read", y = "No of reads")

  n50_label <- ggplot2::annotate(geom = "text", x = 7, y = 39999, label = paste("Q score cut-off"), color = "orange", fontface = "bold", size = 4)

  length_plot <- data + pass_fail + qscore_line + axis_limit + plot_lable + n50_label
  return(length_plot)
}
