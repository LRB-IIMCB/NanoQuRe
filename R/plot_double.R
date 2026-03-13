#' Plot Double
#' 
#' Generates a plot containing minimum, maximum and average read length over sequencing time and a plot containing minimum, maximum and average value of qscore through sequencing.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import dplyr
#' @import ggplot2
#' @importFrom assertthat assert_that
#'
#' @export
#'
#' @examples
#' NULL
plot_double <- function(seq_summary){
  
  
  
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")
  assertthat::assert_that(is.logical(seq_summary$passes_filtering), msg = "Column 'passes_filtering' must be logical")
  assertthat::assert_that(is.numeric(seq_summary$start_time), msg = "Column 'start_time' must be numeric")
  
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  
  
  # length - mean, max, average
  # qscore - mean, max, average
  
  sample_name <-  dplyr::first(seq_summary$sample_id)
  double_data <- seq_summary %>% dplyr::select(c(start_time, duration, sequence_length_template, mean_qscore_template))
  
  
  double_data <- double_data %>% dplyr::arrange(start_time)
  double_data <- double_data %>% dplyr::mutate(hour = start_time/3600)
  double_data <- double_data %>% dplyr::mutate(hour = floor(start_time / 3600))
  double_data <- double_data %>% group_by(hour)
  
summary_length <- double_data %>%
  summarise(max_length = max(sequence_length_template),
  min_length = min(sequence_length_template),
  av_length = mean(sequence_length_template))
  

summary_qscore <- double_data %>%
  summarise(max_qscore = max(mean_qscore_template),
            min_qscore = min(mean_qscore_template),
            av_qscore = mean(mean_qscore_template))



double_1_plot <- ggplot(summary_length, aes(x = hour)) +
  geom_line(aes(y = max_length, color = "Max"), linewidth = 1) +
  geom_line(aes(y = av_length, color = "Average"), linewidth = 1) +
  geom_line(aes(y = min_length, color = "Min"), linewidth = 1) +
  scale_color_manual(name = NULL, values = c("Max" = "#E69F00", "Average" = "#009E73", "Min" = "#0072B2"), breaks = c("Max", "Average", "Min")) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(title = paste0("Length: ", sample_name), x = "Time [h]", y = "Length [bp]") +
  nanoqure_theme()

double_2_plot <- ggplot(summary_qscore, aes(x = hour)) +
  geom_line(aes(y = max_qscore, color = "Max"), linewidth = 1) +
  geom_line(aes(y = av_qscore, color = "Average"), linewidth = 1) +
  geom_line(aes(y = min_qscore, color = "Min"), linewidth = 1) +
  scale_color_manual(name = NULL, values = c("Max" = "#E69F00", "Average" = "#009E73", "Min" = "#0072B2"), breaks = c("Max", "Average", "Min")) +
  labs(title = paste0("Q-score: ", sample_name), x = "Time [h]", y = "Q-score") +
  nanoqure_theme()


return(list(
  length_plot = double_1_plot,
  qscore_plot = double_2_plot
))

}


