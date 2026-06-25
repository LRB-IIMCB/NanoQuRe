#' Plot Cumulative Yield
#'
#' Generates an interactive cumulative plot containing the number of sequenced
#' bases in Gb over time in hours sorted by pass/fail filtering status.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param max_points Maximum number of points plotted per pass/fail trace.
#'   Reads are binned by time so very large runs stay performant in the
#'   browser. Defaults to 2000L.
#'
#' @returns plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_cumulative_yield(sample_data)
#' }
plot_cumulative_yield <- function(seq_summary, max_points = 2000L) {

  # Validation
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
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
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  if (!is.logical(seq_summary$passes_filtering))
    stop("Column 'passes_filtering' must be logical")
  if (!is.numeric(seq_summary$start_time))
    stop("Column 'start_time' must be numeric")

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)

  # Determine bin width so we never exceed max_points on the plot
  max_time_h <- max(seq_summary$start_time, na.rm = TRUE) / 3600
  bin_width_h <- max(max_time_h / max_points, 1 / 60) # minimum 1-minute bins

  cum_data <- seq_summary %>%
    dplyr::select(start_time, passes_filtering, sequence_length_template) %>%
    dplyr::arrange(start_time) %>%
    dplyr::mutate(
      h_start_time = start_time / 3600,
      # bin index for each read
      time_bin = floor(h_start_time / bin_width_h) * bin_width_h
    )

  build_cum_trace <- function(df) {
    df %>%
      dplyr::arrange(h_start_time) %>%
      # cumsum over ALL rows first, then take last value per bin
      dplyr::mutate(bases_gb = cumsum(as.numeric(sequence_length_template)) / 1e9) %>%
      dplyr::group_by(time_bin) %>%
      dplyr::summarise(bases_gb = dplyr::last(bases_gb), .groups = "drop") %>%
      dplyr::rename(h_start_time = time_bin)
  }

  pass_cum <- cum_data %>%
    dplyr::filter(passes_filtering == TRUE) %>%
    build_cum_trace()

  fail_cum <- cum_data %>%
    dplyr::filter(passes_filtering == FALSE) %>%
    build_cum_trace()

  # Plot
  cum_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = pass_cum,
      x = ~h_start_time,
      y = ~bases_gb,
      name = "Pass",
      line = list(color = "#0072B2", width = 2.5),
      hovertemplate = "Time: %{x:.2f} h<br>Yield: %{y:.3f} Gb<extra>Pass</extra>"
    ) %>%
    plotly::add_lines(
      data = fail_cum,
      x = ~h_start_time,
      y = ~bases_gb,
      name = "Fail",
      line = list(color = "#D62728", width = 2.5),
      hovertemplate = "Time: %{x:.2f} h<br>Yield: %{y:.3f} Gb<extra>Fail</extra>"
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
        title = list(text = "<b>Yield [Gb]</b>",
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

  return(cum_plot)
}
