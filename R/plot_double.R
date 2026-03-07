

plot_double <- function(my_data){
  
  # length - mean, max, average
  # qscore - mean, max, average
  
  sample_name <-  dplyr::first(my_data$sample_id)
  double_data <- my_data %>% dplyr::select(c(start_time, duration, sequence_length_template, mean_qscore_template))
  
  
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




double_1 <- ggplot(summary_length, aes(x = hour))
#aes(x= hour, y=c(max_length, min_length, av_length))
line_1 <- geom_line(aes(y = max_length), color = "orange")
line_2 <- geom_line(aes(y = av_length), color = "green")
line_3 <- geom_line(aes(y = min_length), color = "blue")

labels_1 <- labs(title = paste0("Length ", sample_name), x = "Time [h]", y = "min, average, max")

double_1_plot <- double_1 + line_1 + line_2 + line_3 + labels_1 + scale_y_log10(labels = scales::label_comma()) + scale_x_continuous(breaks = seq(0, max(summary_length$hour), by = 5))
return(double_1_plot)






double_2 <- ggplot(summary_qscore, aes(x = hour))
#aes(x= hour, y=c(max_length, min_length, av_length))
line_4 <- geom_line(aes(y = max_qscore), color = "orange")
line_5 <- geom_line(aes(y = av_qscore), color = "green")
line_6 <- geom_line(aes(y = min_qscore), color = "blue")

labels_2 <- labs(title = paste0("Qscore ", sample_name), x = "Time [h]", y = "min, average, max")

double_2_plot <- double_2 + line_4 + line_5 + line_6 + labels_2 + scale_x_continuous(breaks = seq(0, max(summary_length$hour), by = 5))
return(double_2_plot)


}




plot7 <- plot_double(ligase_induro)
plot7




