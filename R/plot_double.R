#' Plot Double
#'
#' Generates an interactive combined plot containing minimum, maximum and
#' average read length over sequencing time (log10 scale) and minimum, maximum
#' and average Q score over sequencing time, displayed side by side.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns plotly subplot object
#' @import dplyr
#' @importFrom plotly plot_ly add_lines subplot layout
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' NULL
plot_double <- function(seq_summary) {
  
  # --- Validation ---
  assertthat::assert_that(nrow(seq_summary) > 0,
                          msg = "The input data frame is empty")
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
    )
  
  summary_qscore <- double_data %>%
    dplyr::summarise(
      max_qscore = max(mean_qscore_template),
      min_qscore = min(mean_qscore_template),
      av_qscore  = mean(mean_qscore_template),
      .groups    = "drop"
    )
  
  # --- Length plot (log10 y axis) ---
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
        title     = list(text = "<b>Length [bp]</b>",
                         font = list(size = 13, family = "Arial")),
        type      = "log",         # plotly log10 axis
        tickformat = ",.0f",
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
        font        = list(size = 11, family = "Arial")
      )
    )
  
  # --- Q score plot (linear y axis) ---
  qscore_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = summary_qscore,
      x             = ~hour,
      y             = ~max_qscore,
      name          = "Max",
      line          = list(color = "#E69F00", width = 2),
      hovertemplate = "Time: %{x} h<br>Max Q: %{y:.2f}<extra></extra>",
      showlegend    = FALSE        # legend already shown in left plot
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
  
  # --- Combine side by side ---
  return(list(
    length_plot = length_plot,
    qscore_plot = qscore_plot
  ))
  
}