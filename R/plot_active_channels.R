#' Plot Active Channels
#'
#' Generates an interactive plot of the number of channels producing reads
#' over the course of the run. The run is divided into time windows
#' (\code{bin_width}, 10 minutes by default) and a channel counts as active in
#' a window when at least one of its reads overlaps that window. Vertical
#' dashed lines mark the first window after the reference point in which the
#' number of active channels falls to 50\% and 25\% of the reference, where
#' the reference is the highest count within the first hour of the run. If a
#' threshold is not reached, a note is displayed on the plot.
#'
#' The sequencing summary does not record pore (mux) scans or pauses, so
#' short periodic dips or gaps in the curve can reflect run control rather
#' than pore loss.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param thresholds Numeric vector of fractions at which to draw threshold
#'   lines. Defaults to c(0.50, 0.25).
#' @param bin_width Width of each time window in hours. Defaults to 1/6
#'   (10 minutes).
#'
#' @returns plotly object
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' plot_active_channels(sample_data)
#' }
plot_active_channels <- function(seq_summary,
                                 thresholds = c(0.50, 0.25),
                                 bin_width = 1 / 6) {

  # Assertion
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("start_time" %in% names(seq_summary)))
    stop("The data frame is missing the 'start_time' column")
  if (!("duration" %in% names(seq_summary)))
    stop("The data frame is missing the 'duration' column")
  if (!("channel" %in% names(seq_summary)))
    stop("The data frame is missing the 'channel' column")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing 'sample_id' column")
  if (!is.numeric(seq_summary$start_time))
    stop("Column 'start_time' must be numeric")
  if (!is.numeric(seq_summary$duration))
    stop("Column 'duration' must be numeric")

  if (!is.numeric(bin_width) || length(bin_width) != 1 || !(bin_width > 0))
    stop("bin_width must be a positive number of hours")

  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)

  reads <- seq_summary[!is.na(seq_summary$start_time) &
                         !is.na(seq_summary$duration) &
                         !is.na(seq_summary$channel), ]
  if (nrow(reads) == 0)
    stop("No reads with start_time, duration and channel available")

  # every window a read overlaps, from its start window to its end window
  first_bin <- floor(reads$start_time / 3600 / bin_width)
  last_bin  <- floor((reads$start_time + reads$duration) / 3600 / bin_width)
  n_windows <- last_bin - first_bin + 1
  read_idx  <- rep(seq_len(nrow(reads)), n_windows)
  window_channel <- unique(data.frame(
    bin     = first_bin[read_idx] + sequence(n_windows) - 1,
    channel = reads$channel[read_idx]
  ))

  # windows without any read are kept as zero active channels
  all_bins <- seq(min(first_bin), max(last_bin))
  active_by_window <- data.frame(
    bin = all_bins,
    active_channels = tabulate(window_channel$bin - min(all_bins) + 1,
                               nbins = length(all_bins))
  )
  active_by_window$time_h <- active_by_window$bin * bin_width

  # reference: the highest count within the first hour of observed data
  first_hour <- active_by_window$time_h < min(active_by_window$time_h) + 1
  initial_channels <- max(active_by_window$active_channels[first_hour])
  reference_time <- active_by_window$time_h[first_hour][
    which.max(active_by_window$active_channels[first_hour])]
  y_max <- max(active_by_window$active_channels)

  # Base plot
  channel_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data = active_by_window,
      x = ~time_h,
      y = ~active_channels,
      name = "Active channels",
      line = list(color = "#612a78", width = 2.5),
      hovertemplate = "Time: %{x:.2f} h<br>Active channels: %{y}<extra></extra>"
    )

  # Threshold lines and annotations
  annotations <- list()
  missed_labels <- c()

  for (thr in sort(thresholds, decreasing = TRUE)) {

    thr_count <- round(initial_channels * thr)

    crossing_time <- active_by_window %>%
      dplyr::filter(time_h > reference_time, active_channels <= thr_count) %>%
      dplyr::slice(1) %>%
      dplyr::pull(time_h)

    if (length(crossing_time) == 0) {
      missed_labels <- c(missed_labels, paste0(thr * 100, "%"))
      next
    }

    thr_label <- paste0(
      thr * 100, "% of first-hour maximum"
    )

    channel_plot <- channel_plot %>%
      plotly::add_lines(
        x = c(crossing_time, crossing_time),
        y = c(0, y_max),
        name = thr_label,
        line = list(color = "#CC79A7", width = 1.8, dash = "dash"),
        hovertemplate = paste0(thr_label, "<extra></extra>")
      )
  }

  # Warning annotation if any threshold was not reached
  if (length(missed_labels) > 0) {

    missed_str <- paste(missed_labels, collapse = " and ")

    annotations <- list(list(
      x = 0.5,
      y = 0.15,
      xref = "paper",
      yref = "paper",
      text = paste0(
        "<b>\u26a0 ", missed_str, " threshold(s) not reached during this run.</b><br>",
        "Active channels stayed above this level for the rest of the run."
      ),
      showarrow = FALSE,
      font = list(size = 12, color = "#9e2a2b", family = "Arial"),
      align = "center",
      bgcolor = "#fff3cd",
      bordercolor = "#e69f00",
      borderwidth = 1.5,
      borderpad = 6,
      opacity = 0.92
    ))
  }

  # Layout
  channel_plot <- channel_plot %>%
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
        title = list(text = "<b>Number of active channels</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      annotations = annotations,
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

  return(channel_plot)
}
