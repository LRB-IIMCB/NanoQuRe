#'Plot Cumulative Yield
#'
#'Generates a cumulative plot containing the number of sequenced bases in Gb over time in hours sorted by pass/fail filtering status.
#'
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
plot_cumulative_yield <- function(seq_summary){

  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"), msg = "The data frame is missing the 'sample_id' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"), msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"), msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"), msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"), msg = "The data frame is missing the 'sequence_length_template' column")
  
  assertthat::assert_that(is.numeric(seq_summary$sequence_length_template), msg = "Column 'sequence_length_template' must be numeric")
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")
  assertthat::assert_that(is.logical(seq_summary$passes_filtering), msg = "Column 'passes_filtering' must be logical")
  assertthat::assert_that(is.numeric(seq_summary$start_time), msg = "Column 'start_time' must be numeric")
  

  sample_name <-  dplyr::first(seq_summary$sample_id)
  cum_data <- seq_summary %>% dplyr::select(c(start_time,duration, passes_filtering, sequence_length_template))

  pass_cum <- cum_data %>% dplyr::filter(passes_filtering==TRUE) %>% dplyr::arrange(start_time)
  fail_cum <- cum_data %>% dplyr::filter(passes_filtering==FALSE) %>% dplyr::arrange(start_time)

  pass_cum <- pass_cum %>% dplyr::mutate(h_start_time = start_time/3600,
        bases_gb = cumsum(as.numeric(sequence_length_template)) / 1e9, pass_status = "pass")

  fail_cum <- fail_cum %>% dplyr::mutate(h_start_time = start_time/3600,
        bases_gb = cumsum(as.numeric(sequence_length_template)) / 1e9, pass_status = "fail")

  pass_fail_tab <- dplyr::bind_rows(pass_cum, fail_cum)


cum_data <- ggplot2::ggplot(pass_fail_tab, aes(x = h_start_time, y = bases_gb, color = pass_status))
cum_lines <- ggplot2::geom_line(linewidth = 1)
cum_labels <- ggplot2::labs(title = paste0("Cumulative Yield ", sample_name), x = "Time [h]", y = "Yield [Gb]")

cum_plot <- cum_data + cum_lines + cum_labels + nanoqure_theme()
return(cum_plot)

}


