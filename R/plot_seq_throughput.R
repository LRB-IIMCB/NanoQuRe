#' Plot Seq Throughput
#'
#' Generates an interactive plot with sequence throughput during time
#' filtered by pass/fail status.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom plotly plot_ly add_lines layout
#' @export
#'
#' @examples
#' plot_seq_throughput(sample_data)
plot_seq_throughput <- function(seq_summary) {
  
  # --- Validation ---
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!("start_time" %in% names(seq_summary)))
    stop("The data frame is missing the 'start_time' column")
  if (!("duration" %in% names(seq_summary)))
    stop("The data frame is missing the 'duration' column")
  if (!("passes_filtering" %in% names(seq_summary)))
    stop("The data frame is missing the 'passes_filtering' column")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!is.logical(seq_summary$passes_filtering))
    stop("Column 'passes_filtering' must be logical")
  if (!is.numeric(seq_summary$start_time))
    stop("Column 'start_time' must be numeric")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  
  cum_data <- seq_summary %>%
    dplyr::select(start_time, duration, passes_filtering, sequence_length_template)
  
  pass_throughput <- cum_data %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(yield_per_hour = sum(sequence_length_template) / 1e9,
                     .groups = "drop")
  
  fail_throughput <- cum_data %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(hour = floor(start_time / 3600)) %>%
    dplyr::group_by(hour) %>%
    dplyr::summarise(yield_per_hour = sum(sequence_length_template) / 1e9,
                     .groups = "drop")
  
  # --- Plot ---
  throughput_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = pass_throughput,
      x             = ~hour,
      y             = ~yield_per_hour,
      name          = "Pass",
      line          = list(color = "#0072B2", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Yield: %{y:.3f} Gb<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      data          = fail_throughput,
      x             = ~hour,
      y             = ~yield_per_hour,
      name          = "Fail",
      line          = list(color = "#D62728", width = 2.5),
      hovertemplate = "Time: %{x} h<br>Yield: %{y:.3f} Gb<extra>Fail</extra>"
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
        title     = list(text = "<b>Yield per hour [Gb]</b>",
                         font = list(size = 13, family = "Arial")),
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9",
      legend = list(
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        bgcolor     = "#ffffff",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font        = list(size = 11, family = "Arial")
      )
    )
  
  return(throughput_plot)
}