#' Plot Active Channels
#'
#' Generates an interactive plot showing number of active channels during
#' sequencing time.
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
plot_active_channels <- function(seq_summary) {
  
  # --- Validation ---
  assertthat::assert_that(nrow(seq_summary) > 0, msg = "The input data frame is empty")
  
  assertthat::assert_that(assertthat::has_name(seq_summary, "start_time"),
                          msg = "The data frame is missing the 'start_time' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "duration"),
                          msg = "The data frame is missing the 'duration' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "channel"),
                          msg = "The data frame is missing the 'channel' column")
  assertthat::assert_that(assertthat::has_name(seq_summary, "sample_id"),
                          msg = "The data frame is missing 'sample_id' column")
  
  assertthat::assert_that(is.numeric(seq_summary$start_time),
                          msg = "Column 'start_time' must be numeric")
  assertthat::assert_that(is.numeric(seq_summary$duration),
                          msg = "Column 'duration' must be numeric")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  
  sorted_by_channel <- seq_summary %>%
    dplyr::group_by(channel) %>%
    dplyr::summarise(
      last_activity = max(start_time + duration, na.rm = TRUE) / 3600,
      .groups = "drop"
    ) %>%
    dplyr::arrange(last_activity) %>%
    dplyr::mutate(
      channel_no_start  = dplyr::n(),
      event             = 1,
      inactive_channels = cumsum(event),
      active_channels   = channel_no_start - inactive_channels
    )
  
  # --- Plot ---
  channel_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = sorted_by_channel,
      x             = ~last_activity,
      y             = ~active_channels,
      name          = "Active channels",
      line          = list(color = "#404040", width = 2.5),
      hovertemplate = "Time: %{x:.2f} h<br>Active channels: %{y}<extra></extra>"
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
        title     = list(text = "<b>Number of active channels</b>",
                         font = list(size = 13, family = "Arial")),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      showlegend    = FALSE        # single line, legend adds no value here
    )
  
  return(channel_plot)
}