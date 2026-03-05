#' Plot Average Speed
#' 
#' Generates a plot with average speed of bases going through the pore, filtered by pass status.
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
plot_average_speed <- function(my_data){
sample_name <-  dplyr::first(my_data$sample_id)

  
cum_data <- my_data %>% dplyr::select(c(start_time,duration, passes_filtering, sequence_length_template))

pass_h <- cum_data %>% dplyr::filter(passes_filtering==TRUE) %>% dplyr::arrange(start_time)
fail_h <- cum_data %>% dplyr::filter(passes_filtering==FALSE) %>% dplyr::arrange(start_time)

pass_h <- pass_h %>% mutate(hour = floor(start_time / 3600))
fail_h <- fail_h %>% mutate(hour = floor(start_time / 3600))

#pass_h <- pass_h %>% mutate(speed = sum(sequence_length_template) / sum(duration))
#fail_h <- fail_h %>% mutate(speed = sum(sequence_length_template) / sum(duration))


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
channel_labels <- labs(title = paste0("Average speed ", sample_name), x = "Time [h]", y = "Speed")


av_speed_plot <- av_speed + channel_lines + channel_labels
return(av_speed_plot)

}


#plot6 <- plot_average_speed(ligase_induro)
#plot6
