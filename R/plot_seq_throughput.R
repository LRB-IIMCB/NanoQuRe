#' Plot Seq Throughput
#'
#'Generates a plot with sequence throughput during time filtered by pass status
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' NULL
plot_seq_throughput <- function(seq_summary){
  
  
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  
  
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")
  assertthat::assert_that(is.logical(seq_summary$passes_filtering), msg = "Column 'passes_filtering' must be logical")
  assertthat::assert_that(is.numeric(seq_summary$start_time), msg = "Column 'start_time' must be numeric")
  
  
  
  sample_name <-  dplyr::first(seq_summary$sample_id)
  cum_data <- seq_summary %>% dplyr::select(c(start_time,duration, passes_filtering, sequence_length_template))
  
  pass_cum <- cum_data %>% dplyr::filter(passes_filtering==TRUE) %>% dplyr::arrange(start_time)
  fail_cum <- cum_data %>% dplyr::filter(passes_filtering==FALSE) %>% dplyr::arrange(start_time)
  
  
  pass_cum <- pass_cum %>% mutate(hour = floor(start_time / 3600))
  
  fail_cum <- fail_cum %>% mutate(hour = floor(start_time / 3600))
  
  
  pass_throughput <- pass_cum %>% group_by(hour) %>%
    summarise(yield_per_hour = sum(sequence_length_template) / 1e9) %>%
    mutate(pass_status = "pass")
  
  fail_throughput <- fail_cum %>%
    group_by(hour) %>%
    summarise(yield_per_hour = sum(sequence_length_template) / 1e9) %>%
    mutate(pass_status = "fail")
  
  data_by_hour <- bind_rows(pass_throughput, fail_throughput)
  
  throughput <- ggplot(data_by_hour, aes(x= hour, y=yield_per_hour, color=pass_status))
  channel_lines <- geom_line(linewidth = 1)
  channel_labels <- labs(title = (sample_name), x = "Time [h]", y = "Yield per hour")
  
  
  throughput_plot <- throughput + channel_lines + channel_labels + nanoqure_theme()
  return(throughput_plot)
  
}

