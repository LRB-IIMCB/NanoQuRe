#' Plot Average Speed
#'
#' Generates an interactive plot with average speed of bases going through
#' the pore per hour, filtered by pass/fail status.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_average_speed(sample_data)
#' }
plot_average_speed <- function(seq_summary) {

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
  required_cols <- c("sample_id", "start_time", "duration", "passes_filtering", "sequence_length_template")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.logical(seq_summary$passes_filtering),
                   "Column passes_filtering must be logical. Please provide a valid argument.")
  assert_condition(is.numeric(seq_summary$start_time),
                   "Column start_time must be numeric. Please provide a valid argument.")
  assert_condition(is.numeric(seq_summary$duration),
                   "Column duration must be numeric. Please provide a valid argument.")
  assert_condition(is.numeric(seq_summary$sequence_length_template),
                   "Column sequence_length_template must be numeric. Please provide a valid argument.")

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)
  # every hour of the run gets a row: empty hours stay NA so the line breaks
  # instead of being interpolated across the gap
  all_hours <- data.frame(
    hour = seq(floor(min(seq_summary$start_time, na.rm = TRUE) / 3600),
               floor(max(seq_summary$start_time, na.rm = TRUE) / 3600))
  )

  cum_data <- seq_summary %>%
    dplyr::select(start_time, duration, passes_filtering, sequence_length_template)

  pass_speed <- cum_data %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(speed = sum(sequence_length_template, na.rm = TRUE) /
                       sum(duration, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour) %>%
    # zero total duration gives Inf/NaN, which cannot be drawn
    dplyr::mutate(speed = ifelse(is.finite(speed), speed, NA_real_))

  fail_speed <- cum_data %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(speed = sum(sequence_length_template, na.rm = TRUE) /
                       sum(duration, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour) %>%
    # zero total duration gives Inf/NaN, which cannot be drawn
    dplyr::mutate(speed = ifelse(is.finite(speed), speed, NA_real_))

  # Plot
  speed_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = pass_speed,
      x = ~hour,
      y = ~speed,
      name = "Pass",
      line = list(color = "#0072B2", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Speed: %{y:.1f} nt/s<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      data = fail_speed,
      x = ~hour,
      y = ~speed,
      name = "Fail",
      line = list(color = "#D62728", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Speed: %{y:.1f} nt/s<extra>Fail</extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
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
        title = list(text = "<b>Speed [nt/s]</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = list(
        x = 1.02,
        y = 1,
        xanchor = "left",
        bgcolor = "#ffffff",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font = list(size = 11, family = "Arial")
      )
    )

  return(speed_plot)
}
