#' Plot Seq Throughput
#'
#'Generates a plot with sequence throughput during time filtered by pass status
#'
#' @param my_data A dataframe containing the sequencing summary
#'
#' @returns ggplot2 object
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' NULL
plot_seq_throughput <- function(my_data){
  
  
  assertthat::assert_that(assertthat::has_name(my_data, "sample_name"), msg = "The data frame is missing the 'sample_name' column")
  
  assertthat::assert_that(assertthat::has_name(my_data, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(my_data, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(my_data, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(my_data, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  
  
  assertthat::assert_that(nrow(my_data) > 0, msg = "The input data frame is empty")
  assertthat::assert_that(is.logical(my_data$passes_filtering), msg = "Column 'passes_filtering' must be logical")
  assertthat::assert_that(is.numeric(my_data$start_time), msg = "Column 'start_time' must be numeric")
  
  
  
  sample_name <-  dplyr::first(my_data$sample_id)
  cum_data <- my_data %>% dplyr::select(c(start_time,duration, passes_filtering, sequence_length_template))
  
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
  channel_labels <- labs(title = paste0("Throughput ", sample_name), x = "Time [h]", y = "Yield per hour")
  
  
  throughput_plot <- throughput + channel_lines + channel_labels
  return(throughput_plot)
  
}

#plot5 <- plot_seq_throughput(sample_data)
#plot5
