#' Plot Seq Throughput
#'
#' Generates an interactive plot with sequence throughput during time
#' filtered by pass/fail status.
#'
#' @param seq_summary A data frame containing the sequencing summary
#'
#' @return plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_seq_throughput(sample_data)
#' }
plot_seq_throughput <- function(seq_summary) {

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

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)
  # every hour of the run gets a row: empty hours are zero yield instead of
  # a straight line interpolated across the gap
  all_hours <- data.frame(
    hour = seq(floor(min(seq_summary$start_time, na.rm = TRUE) / 3600),
               floor(max(seq_summary$start_time, na.rm = TRUE) / 3600))
  )

  cum_data <- seq_summary %>%
    dplyr::select(start_time, duration, passes_filtering, sequence_length_template)

  pass_throughput <- cum_data %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(yield_per_hour = sum(sequence_length_template, na.rm = TRUE) / 1e9,
                     .groups = "drop") %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour) %>%
    dplyr::mutate(yield_per_hour = ifelse(is.na(yield_per_hour), 0, yield_per_hour))

  fail_throughput <- cum_data %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(yield_per_hour = sum(sequence_length_template, na.rm = TRUE) / 1e9,
                     .groups = "drop") %>%
    dplyr::right_join(all_hours, by = "hour") %>%
    dplyr::arrange(hour) %>%
    dplyr::mutate(yield_per_hour = ifelse(is.na(yield_per_hour), 0, yield_per_hour))

  # Plot
  throughput_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = pass_throughput,
      x = ~hour,
      y = ~yield_per_hour,
      name = "Pass",
      line = list(color = "#0072B2", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Yield: %{y:.3f} Gb<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      data = fail_throughput,
      x = ~hour,
      y = ~yield_per_hour,
      name = "Fail",
      line = list(color = "#D62728", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Yield: %{y:.3f} Gb<extra>Fail</extra>"
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
        title = list(text = "<b>Yield per hour [Gb]</b>",
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

  return(throughput_plot)
}
