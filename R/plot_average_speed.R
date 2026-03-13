#' Plot Average Speed
#' 
#' Generates a plot with average speed of bases going through the pore, filtered by pass status.
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
plot_average_speed <- function(seq_summary){
  
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")

  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing 'sample_id'column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  
  
  
  
assertthat::assert_that(is.logical(seq_summary$passes_filtering), msg = "Column 'passes_filtering' must be logical")
assertthat::assert_that(is.numeric(seq_summary$start_time), msg = "Column 'start_time' must be numeric")
assertthat::assert_that(is.numeric(seq_summary$duration), msg = "Column 'duration' must be numeric")
assertthat::assert_that(is.numeric(seq_summary$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")
  
sample_name <-  dplyr::first(seq_summary$sample_id)

  
cum_data <- seq_summary %>% dplyr::select(c(start_time,duration, passes_filtering, sequence_length_template))

pass_h <- cum_data %>% dplyr::filter(passes_filtering==TRUE) %>% dplyr::arrange(start_time)
fail_h <- cum_data %>% dplyr::filter(passes_filtering==FALSE) %>% dplyr::arrange(start_time)

pass_h <- pass_h %>% mutate(hour = floor(start_time / 3600))
fail_h <- fail_h %>% mutate(hour = floor(start_time / 3600))


pass_speed <- pass_h %>% group_by(hour) %>%
  summarise(speed = sum(sequence_length_template) / sum(duration)) %>%
  mutate(pass_status = "pass")

fail_speed <- fail_h %>%
  group_by(hour) %>%
  summarise(speed = sum(sequence_length_template) / sum(duration)) %>%
  mutate(pass_status = "fail")

data_by_hour <- bind_rows(pass_speed, fail_speed)

av_speed <- ggplot(data_by_hour, aes(x= hour, y=speed, color=pass_status))
channel_lines <- geom_line(linewidth = 1)
channel_labels <- labs(title = paste0("Average speed [bp/s]", sample_name), x = "Time [h]", y = "Speed")


av_speed_plot <- av_speed + channel_lines + channel_labels + nanoqure_theme()
return(av_speed_plot)

}


