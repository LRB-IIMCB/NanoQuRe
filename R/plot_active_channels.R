#' Plot Active Channels
#' 
#' Generates a plot showing number of active channels during sequencing time. 
#' 
#' @param my_data A dataframe containing the sequencing summary
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
plot_active_channels <- function(my_data){
  
  
  assertthat::assert_that(assertthat::has_name(my_data, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(my_data, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(my_data, "channel"), msg = "The data frame is missing the 'channel' column")
  assertthat::assert_that(assertthat::has_name(my_data, "sample_id"), msg = "The data frame is missing 'sample_id' column")
  
  
  assertthat::assert_that(is.numeric(my_data$start_time), msg = "Column 'start_time' must be numeric")
  assertthat::assert_that(is.numeric(my_data$duration), msg = "Column 'duration' must be numeric")
  

  sample_name <- dplyr::first(my_data$sample_id)

  sorted_by_channel <- my_data %>% group_by(channel)

  sorted_by_channel <- sorted_by_channel %>% dplyr::summarise(
    last_activity = max(start_time + duration, na.rm = TRUE) / 3600)

  sorted_by_channel <- sorted_by_channel %>% arrange(last_activity)


  sorted_by_channel <- sorted_by_channel %>% dplyr::mutate(channel_no_start = dplyr::n(), event = 1, inactive_channels = cumsum(event), active_channels = channel_no_start - inactive_channels)

  channel_data <- ggplot(sorted_by_channel, aes(x= last_activity, y=active_channels))
  channel_lines <- geom_line(linewidth = 1)
  channel_labels <- labs(title = paste0("Number of active channels ", sample_name), x = "Time [h]", y = "Number of active channels")


  channel_plot <- channel_data + channel_lines + channel_labels
  return(channel_plot)

}

#plot4 <- plot_active_channels(ligase_induro)
#plot4
