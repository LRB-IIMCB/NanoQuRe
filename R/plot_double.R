#' Plot Length and Q-score Over Time
#'
#' Generates two interactive plots showing how read length and mean Q score
#' change over the course of a sequencing run. Returns both as a named list.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns A named list with two plotly objects:
#' \describe{
#'   \item{length_plot}{Read length (min, mean, max) per hour}
#'   \item{qscore_plot}{Mean Q score (min, mean, max) per hour}
#' }
#' @import dplyr
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom assertthat assert_that
#' @export
plot_double <- function(seq_summary) {
  
  # --- Validation ---
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("start_time" %in% names(seq_summary)))
    stop("The data frame is missing the 'start_time' column")
  if (!("duration" %in% names(seq_summary)))
    stop("The data frame is missing the 'duration' column")
  if (!("passes_filtering" %in% names(seq_summary)))
    stop("The data frame is missing the 'passes_filtering' column")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!is.logical(seq_summary$passes_filtering))
    stop("Column 'passes_filtering' must be logical")
  if (!is.numeric(seq_summary$start_time))
    stop("Column 'start_time' must be numeric")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  
  double_data <- seq_summary %>%
    dplyr::select(start_time, duration, sequence_length_template,
                  mean_qscore_template) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour)
  
  summary_length <- double_data %>%
    dplyr::summarise(
      max_length = max(sequence_length_template),
      min_length = min(sequence_length_template),
      av_length  = mean(sequence_length_template),
      .groups    = "drop"
    ) %>%
    dplyr::mutate(min_length = pmax(min_length, 50))
  
  summary_qscore <- double_data %>%
    dplyr::summarise(
      max_qscore = max(mean_qscore_template),
      min_qscore = min(mean_qscore_template),
      av_qscore  = mean(mean_qscore_template),
      .groups    = "drop"
    )
  
  # shared legend config — shown on length plot only to avoid duplication
  legend_outside <- list(
    x           = 1.02,
    y           = 1,
    xanchor     = "left",
    bgcolor     = "#ffffff",
    bordercolor = "#cccccc",
    borderwidth = 1,
    font        = list(size = 11, family = "Arial")
  )
  
  # --- Length plot ---
  length_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = summary_length,
      x             = ~hour,
      y             = ~max_length,
      name          = "Max",
      line          = list(color = "#E69F00", width = 2),
      hovertemplate = "Time: %{x} h<br>Max length: %{y} bp<extra></extra>"
    ) %>%
    plotly::add_lines(
      data          = summary_length,
      x             = ~hour,
      y             = ~av_length,
      name          = "Average",
      line          = list(color = "#009E73", width = 2),
      hovertemplate = "Time: %{x} h<br>Avg length: %{y:.0f} bp<extra></extra>"
    ) %>%
    plotly::add_lines(
      data          = summary_length,
      x             = ~hour,
      y             = ~min_length,
      name          = "Min",
      line          = list(color = "#56B4E9", width = 2),
      hovertemplate = "Time: %{x} h<br>Min length: %{y} bp<extra></extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>Length ", sample_name, "</b>"),
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
        title      = list(text = "<b>Length [bp]</b>",
                          font = list(size = 13, family = "Arial")),
        type       = "log",
        dtick      = "D2",
        tickformat = ",.0f",
        showgrid   = TRUE,
        gridcolor  = "#e0e0e0",
        tickfont   = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend        = legend_outside
    )
  
  # --- Q score plot (legend hidden — same colours as length plot) ---
  qscore_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = summary_qscore,
      x             = ~hour,
      y             = ~max_qscore,
      name          = "Max",
      line          = list(color = "#E69F00", width = 2),
      hovertemplate = "Time: %{x} h<br>Max Q: %{y:.2f}<extra></extra>",
      showlegend    = FALSE
    ) %>%
    plotly::add_lines(
      data          = summary_qscore,
      x             = ~hour,
      y             = ~av_qscore,
      name          = "Average",
      line          = list(color = "#009E73", width = 2),
      hovertemplate = "Time: %{x} h<br>Avg Q: %{y:.2f}<extra></extra>",
      showlegend    = FALSE
    ) %>%
    plotly::add_lines(
      data          = summary_qscore,
      x             = ~hour,
      y             = ~min_qscore,
      name          = "Min",
      line          = list(color = "#56B4E9", width = 2),
      hovertemplate = "Time: %{x} h<br>Min Q: %{y:.2f}<extra></extra>",
      showlegend    = FALSE
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>Q-score ", sample_name, "</b>"),
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
        title     = list(text = "<b>Q-score</b>",
                         font = list(size = 13, family = "Arial")),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )
  
  return(list(
    length_plot = length_plot,
    qscore_plot = qscore_plot
  ))
}