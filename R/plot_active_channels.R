#' Plot Active Channels
#'
#' Generates an interactive plot showing number of active channels during
#' sequencing time. Vertical dashed lines mark the time point at which 50%
#' and 25% of initial channels remain active, annotated with the crossing
#' time in hours. If a threshold is not reached during the run, a warning
#' annotation is displayed on the plot prompting closer inspection.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param thresholds Numeric vector of fractions at which to draw threshold
#'   lines. Defaults to c(0.50, 0.25).
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom plotly plot_ly add_lines layout
#' @export
#'
#' @examples
#' plot_active_channels(sample_data)
plot_active_channels <- function(seq_summary, thresholds = c(0.50, 0.25)) {
  
  # --- Validation ---
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
  
  initial_channels <- dplyr::first(sorted_by_channel$channel_no_start)
  x_max            <- max(sorted_by_channel$last_activity)
  y_max            <- initial_channels
  
  # --- Base plot ---
  channel_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = sorted_by_channel,
      x             = ~last_activity,
      y             = ~active_channels,
      name          = "Active channels",
      line          = list(color = "#612a78", width = 2.5),
      hovertemplate = "Time: %{x:.2f} h<br>Active channels: %{y}<extra></extra>"
    )
  
  # --- Threshold lines and annotations ---
  annotations   <- list()
  missed_labels <- c()
  
  for (thr in sort(thresholds, decreasing = TRUE)) {
    
    thr_count <- round(initial_channels * thr)
    
    crossing_time <- sorted_by_channel %>%
      dplyr::filter(active_channels <= thr_count) %>%
      dplyr::slice(1) %>%
      dplyr::pull(last_activity)
    
    if (length(crossing_time) == 0) {
      missed_labels <- c(missed_labels, paste0(thr * 100, "%"))
      next
    }
    
    thr_label <- paste0(
      thr * 100, "% of initial active channels"
    )
    
    channel_plot <- channel_plot %>%
      plotly::add_lines(
        x             = c(crossing_time, crossing_time),
        y             = c(0, y_max),
        name          = thr_label,
        line          = list(color = "#CC79A7", width = 1.8, dash = "dash"),
        hovertemplate = paste0(thr_label, "<extra></extra>")
      )
  }
  
  # --- Warning annotation if any threshold was not reached ---
  if (length(missed_labels) > 0) {
    
    missed_str <- paste(missed_labels, collapse = " and ")
    
    annotations <- list(list(
      x           = x_max * 0.5,
      y           = y_max * 0.15,
      text        = paste0(
        "<b>\u26a0 ", missed_str, " threshold(s) not reached during this run.</b><br>",
        "Channel decay may be atypical \u2014 please inspect this sample more closely."
      ),
      showarrow   = FALSE,
      font        = list(size = 12, color = "#9e2a2b", family = "Arial"),
      align       = "center",
      bgcolor     = "#fff3cd",
      bordercolor = "#e69f00",
      borderwidth = 1.5,
      borderpad   = 6,
      opacity     = 0.92
    ))
  }
  
  # --- Layout ---
  channel_plot <- channel_plot %>%
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
      annotations   = annotations,
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
  
  return(channel_plot)
}