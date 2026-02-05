

#'Plot Cumulative Yield
#'
#' @param my_data
#'
#' @returns plot
#' @export
#'
#' @examples
#' \dontrun {
#'  plot_yield <- lapply(seq_sum_database, plot_cumulative_yield)
#' }
plot_cumulative_yield() <- function(my_data){
  sample_name <-  first(my_data$sample_id)
  cum_data <- my_data %>% select(c(start_time,duration, passes_filtering, sequence_length_template))

  pass_cum <- cum_data %>% filter(passes_filtering==TRUE) %>% arrange(start_time)
  fail_cum <- cum_data %>% filter(passes_filtering==FALSE) %>% arrange(start_time)

  pass_cum <- pass_cum %>% mutate(h_start_time = start_time/3600,
                                  bases_gb = cumsum(as.numeric(sequence_length_template)) / 1e9,
                                  pass_status = "pass")

  fail_cum <- fail_cum %>% mutate(h_start_time = start_time/3600,
                                  bases_gb = cumsum(as.numeric(sequence_length_template)) / 1e9,
                                  pass_status = "fail")

  pass_fail_tab <- dplyr::bind_rows(pass_cum, fail_cum)


cum_data <- ggplot(pass_fail_tab, aes(x = h_start_time, y = bases_gb, color = pass_status))
cum_lines <- geom_line(linewidth = 1)
cum_labels <- labs(title = "Cumulative Yield", x = "Time (hours)", y = "Yield (Gb)")

cum_plot <- cum_data + cum_lines + cum_labels
cum_plot

}
