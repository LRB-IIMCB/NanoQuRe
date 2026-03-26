#' Plot Average Speed
#'
#' Generates an interactive plot with average speed of bases going through
#' the pore per hour, filtered by pass/fail status.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_average_speed <- function(seq_summary) {
  
  # --- Validation ---
  assertthat::assert_that(nrow(seq_summary) > 0,
                          msg = "The input data frame is empty")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"),
                          msg = "The data frame is missing 'sample_id'column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"),
                          msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"),
                          msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "passes_filtering"),
                          msg = "The data frame is missing the 'passes_filtering' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sequence_length_template"),
                          msg = "The data frame is missing the 'sequence_length_template' column")
  
  assertthat::assert_that(is.logical(seq_summary$passes_filtering),
                          msg = "Column 'passes_filtering' must be logical")
  assertthat::assert_that(is.numeric(seq_summary$start_time),
                          msg = "Column 'start_time' must be numeric")
  assertthat::assert_that(is.numeric(seq_summary$duration),
                          msg = "Column 'duration' must be numeric")
  assertthat::assert_that(is.numeric(seq_summary$sequence_length_template),
                          msg = "Column 'sequence_length_template' must be numeric")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  
  cum_data <- seq_summary %>%
    dplyr::select(start_time, duration, passes_filtering, sequence_length_template)
  
  pass_speed <- cum_data %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(speed = sum(sequence_length_template) / sum(duration),
                     .groups = "drop")
  
  fail_speed <- cum_data %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(speed = sum(sequence_length_template) / sum(duration),
                     .groups = "drop")
  
  # --- Plot ---
  speed_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = pass_speed,
      x             = ~hour,
      y             = ~speed,
      name          = "Pass",
      line          = list(color = "#0072B2", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Speed: %{y:.1f} bp/s<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      data          = fail_speed,
      x             = ~hour,
      y             = ~speed,
      name          = "Fail",
      line          = list(color = "#D62728", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Speed: %{y:.1f} bp/s<extra>Fail</extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x    = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title     = list(text = "<b>Time [h]</b>",
                         font = list(size = 13, family = "Arial")),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title     = list(text = "<b>Speed [bp/s]</b>",
                         font = list(size = 13, family = "Arial")),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = list(
        bgcolor     = "#ffffff",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font        = list(size = 11, family = "Arial"),
        x           = 0.75,
        y           = 0.95
      )
    )
  
  return(speed_plot)
}
