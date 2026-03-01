
plot_active_channels <- function(my_data){

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
