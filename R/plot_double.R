#' Plot Length and Q-score Over Time
#'
#' Generates two interactive plots showing how read length and mean Q-score
#' change over the course of a sequencing run. Returns both as a named list.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return A named list with two plotly objects:
#' \describe{
#'   \item{length_plot}{Read length (min, mean, max) per hour}
#'   \item{qscore_plot}{Mean Q-score (min, mean, max) per hour}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' result <- plot_double(sample_data)
#' result$length_plot
#' result$qscore_plot
#' }
plot_double <- function(seq_summary) {

  #assertions
  if (missing(seq_summary)) {
    stop("Sequencing summary is missing. Please provide a valid seq_summary argument.",
         call. = FALSE)
  }

  if (!is.data.frame(seq_summary) || nrow(seq_summary) == 0) {
    stop("Empty data frame provided as an input (seq_summary). Please provide a valid input.",
         call. = FALSE)
  }

  # required columns
  required_cols <- c("sample_id", "start_time", "sequence_length_template",
                     "mean_qscore_template")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.numeric(seq_summary$mean_qscore_template),
                   "Column mean_qscore_template must be numeric. Please provide a valid argument.")

  assert_condition(is.numeric(seq_summary$start_time),
                   "Column start_time must be numeric. Please provide a valid argument.")

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)

  # every hour of the run gets a row: hours without reads stay NA so the
  # lines break instead of being interpolated across the gap
  all_hours <- data.frame(
    hour = seq(floor(min(seq_summary$start_time, na.rm = TRUE) / 3600),
               floor(max(seq_summary$start_time, na.rm = TRUE) / 3600))
  )

  double_data <- seq_summary %>%
    dplyr::select(start_time, sequence_length_template, mean_qscore_template) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour)

  summary_length <- double_data %>%
    dplyr::summarise(
      max_length = suppressWarnings(max(sequence_length_template, na.rm = TRUE)),
      # zero-length reads are left out of the minimum: 0 cannot be drawn on the
      # log y axis
      min_length = suppressWarnings(
        min(sequence_length_template[sequence_length_template > 0], na.rm = TRUE)),
      av_length = mean(sequence_length_template, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # all-NA hours give +/-Inf and NaN, which are mapped to NA
    dplyr::mutate(dplyr::across(-hour, ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour)
  # true minimum (of reads with positive length) is kept; flooring it at 50
  # could plot min > max

  summary_qscore <- double_data %>%
    dplyr::summarise(
      max_qscore = suppressWarnings(max(mean_qscore_template, na.rm = TRUE)),
      min_qscore = suppressWarnings(min(mean_qscore_template, na.rm = TRUE)),
      av_qscore = mean(mean_qscore_template, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(-hour, ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour)

  # shared legend config - shown on length plot only to avoid duplication
  legend_outside <- list(
    x = 1.02,
    y = 1,
    xanchor = "left",
    bgcolor = "#ffffff",
    bordercolor = "#cccccc",
    borderwidth = 1,
    font = list(size = 11, family = "Arial")
  )

  # Length plot
  length_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = summary_length,
      x = ~hour,
      y = ~max_length,
      name = "Max",
      line = list(color = "#E69F00", width = 2),
      hovertemplate = "Time: %{x} h<br>Max length: %{y} nt<extra></extra>"
    ) %>%
    plotly::add_lines(
      data = summary_length,
      x = ~hour,
      y = ~av_length,
      name = "Average",
      line = list(color = "#009E73", width = 2),
      hovertemplate = "Time: %{x} h<br>Avg length: %{y:.0f} nt<extra></extra>"
    ) %>%
    plotly::add_lines(
      data = summary_length,
      x = ~hour,
      y = ~min_length,
      name = "Min",
      line = list(color = "#56B4E9", width = 2),
      hovertemplate = "Time: %{x} h<br>Min length: %{y} nt<extra></extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>Length ", sample_name, "</b>"),
        x = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title = list(text = "<b>Time [h]</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title = list(text = "<b>Length [nt]</b>",
                     font = list(size = 13, family = "Arial")),
        type = "log",
        dtick = "D2",
        tickformat = ",.0f",
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = legend_outside
    )

  # Q-score plot (legend hidden - same colours as length plot)
  qscore_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = summary_qscore,
      x = ~hour,
      y = ~max_qscore,
      name = "Max",
      line = list(color = "#E69F00", width = 2),
      hovertemplate = "Time: %{x} h<br>Max Q: %{y:.2f}<extra></extra>",
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      data = summary_qscore,
      x = ~hour,
      y = ~av_qscore,
      name = "Average",
      line = list(color = "#009E73", width = 2),
      hovertemplate = "Time: %{x} h<br>Avg Q: %{y:.2f}<extra></extra>",
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      data = summary_qscore,
      x = ~hour,
      y = ~min_qscore,
      name = "Min",
      line = list(color = "#56B4E9", width = 2),
      hovertemplate = "Time: %{x} h<br>Min Q: %{y:.2f}<extra></extra>",
      showlegend = FALSE
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>Q-score ", sample_name, "</b>"),
        x = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title = list(text = "<b>Time [h]</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title = list(text = "<b>Q-score</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )

  return(list(
    length_plot = length_plot,
    qscore_plot = qscore_plot
  ))
}
