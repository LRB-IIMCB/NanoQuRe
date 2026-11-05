#' Plot Read Lengths
#'
#' Generates an interactive histogram of read lengths. Reads are counted in
#' fixed bins (50 bins per order of magnitude on the default log axis, 200
#' equal bins on a linear axis), so the plot size does not grow with the
#' number of reads. Two vertical lines represent the mean read length and the
#' N50 value calculated with \code{\link{calculate_n50}}.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param upper_limit Upper limit of the x axis in nucleotides. If NULL
#'   (default), the full length range is shown. Reads beyond the limit are
#'   counted in an on-plot note.
#' @param y_limit Upper limit of the y axis. If NULL (default), the axis is
#'   scaled automatically.
#' @param log_x Logical; plot lengths on a log10 axis. Defaults to TRUE.
#'   Reads of length 0 cannot be shown on a log axis and are counted in an
#'   on-plot note.
#' @param weight What the bar height counts: \code{"reads"} (default, number
#'   of reads per bin) or \code{"bases"} (sum of read lengths per bin, i.e.
#'   where the sequenced bases are).
#'
#' @returns plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_read_lengths(sample_data)
#' plot_read_lengths(sample_data, weight = "bases")
#' }
plot_read_lengths <- function(seq_summary,
                              upper_limit = NULL,
                              y_limit = NULL,
                              log_x = TRUE,
                              weight = "reads") {

  # Assertions
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!is.numeric(seq_summary$sequence_length_template))
    stop("Column 'sequence_length_template' must be numeric")
  if (!(weight %in% c("reads", "bases")))
    stop("weight must be either 'reads' or 'bases'")
  if (!is.logical(log_x) || is.na(log_x))
    stop("log_x must be TRUE or FALSE")

  if (!is.null(upper_limit)) {
    upper_limit <- suppressWarnings(as.numeric(upper_limit))
    if (is.na(upper_limit))
      stop("upper_limit must be a number")
  }

  # Data prep
  lengths     <- seq_summary$sequence_length_template[
    !is.na(seq_summary$sequence_length_template)]
  sample_name <- seq_summary$sample_id[[1]]
  mean_length <- mean(lengths)
  n50_SR      <- calculate_n50(seq_summary)

  # zero-length reads have no position on a log axis
  n_hidden_zero <- if (log_x) sum(lengths <= 0) else 0
  shown <- if (log_x) lengths[lengths > 0] else lengths
  if (length(shown) == 0)
    stop("No reads of positive length to plot; use log_x = FALSE")

  # bin edges; the last edge lies above the longest read so every read is counted
  if (log_x) {
    steps_per_decade <- 50
    low  <- floor(log10(min(shown)) * steps_per_decade)
    high <- floor(log10(max(shown)) * steps_per_decade) + 1
    breaks <- 10^(seq(low, high) / steps_per_decade)
  } else {
    bin_width <- max(1, ceiling(max(shown) / 200))
    breaks <- seq(0, (floor(max(shown) / bin_width) + 1) * bin_width, by = bin_width)
  }

  bin_idx <- findInterval(shown, breaks)
  bin_values <- if (weight == "reads") rep(1, length(shown)) else shown
  heights <- as.vector(tapply(bin_values,
                              factor(bin_idx, levels = seq_len(length(breaks) - 1)),
                              sum, default = 0))

  # step line: one point per left edge, plus the right edge of the last bin
  count_seq <- data.frame(
    edge  = breaks,
    upper = c(breaks[-1], breaks[length(breaks)]),
    n     = c(heights, heights[length(heights)])
  )
  max_y <- max(heights) * 1.05

  # note reads that are not visible
  n_hidden_limit <- if (is.null(upper_limit)) 0 else sum(lengths > upper_limit)
  hidden_note <- c(
    if (n_hidden_limit > 0)
      paste0(n_hidden_limit, if (n_hidden_limit == 1) " read" else " reads",
             " longer than ", upper_limit, " nt outside the axis"),
    if (n_hidden_zero > 0)
      paste0(n_hidden_zero, if (n_hidden_zero == 1) " read" else " reads",
             " of length 0 not shown on the log axis")
  )
  annotations <- if (length(hidden_note) == 0) list() else list(list(
    x = 1, y = 1, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste(hidden_note, collapse = "<br>"),
    font = list(size = 11, family = "Arial", color = "#9e2a2b")
  ))

  if (weight == "reads") {
    y_title <- "<b>Number of reads</b>"
    trace_name <- "Read counts"
  } else {
    y_title <- "<b>Number of bases</b>"
    trace_name <- "Bases"
  }

  x_range <- if (is.null(upper_limit)) {
    NULL
  } else if (log_x) {
    log10(c(min(breaks), upper_limit))
  } else {
    c(0, upper_limit)
  }

  # Plot
  length_plot <- plotly::plot_ly() %>%
    plotly::add_lines(
      data          = count_seq,
      x             = ~edge,
      y             = ~n,
      customdata    = ~upper,
      name          = trace_name,
      line          = list(color = "#404040", width = 1, shape = "hv"),
      fill          = "tozeroy",
      fillcolor     = "rgba(64, 64, 64, 0.6)",
      hovertemplate = "Length: %{x:.0f}-%{customdata:.0f} nt<br>Count: %{y}<extra></extra>"
    ) %>%
    plotly::add_lines(
      x             = c(mean_length, mean_length),
      y             = c(0, max_y),
      name          = paste0("Mean: ", round(mean_length, 0), " nt"),
      line          = list(color = "#CC79A7", width = 2.5),
      hovertemplate = paste0("Mean: ", round(mean_length, 0), " nt<extra></extra>")
    ) %>%
    plotly::add_lines(
      x             = c(n50_SR, n50_SR),
      y             = c(0, max_y),
      name          = paste0("N50: ", round(n50_SR, 0), " nt"),
      line          = list(color = "#009E73", width = 2.5),
      hovertemplate = paste0("N50: ", round(n50_SR, 0), " nt<extra></extra>")
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x    = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      xaxis = list(
        title     = list(text = "<b>Sequence length [nt]</b>",
                         font = list(size = 13, family = "Arial")),
        type      = if (log_x) "log" else "linear",
        range     = x_range,
        showgrid  = TRUE,
        gridcolor = "#e0e0e0",
        tickfont  = list(size = 11, family = "Arial", color = "#333333")
      ),
      yaxis = list(
        title     = list(text = y_title,
                         font = list(size = 13, family = "Arial")),
        range     = if (is.null(y_limit)) NULL else c(0, y_limit),
        rangemode = "tozero",
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

  return(length_plot)
}
