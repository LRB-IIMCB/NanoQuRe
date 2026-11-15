#' Pore Activity Heatmap
#'
#' Generates an interactive heatmap showing channel activity over time and
#' channel activity with respect to their disposition on the flow cell.
#'
#' The input must contain a single \code{run_id}. Only the MinION (512
#' channels) and PromethION (3000 channels) layouts are supported. Flongle
#' (126 channels) is not: Flongle data passed with \code{platform = "minion"}
#' is drawn on the MinION layout, so the spatial panel does not reflect the
#' physical positions of the channels.
#'
#' Function contains helpers originally provided by Barbara Ottolini,
#' from Oxford Nanopore Technologies.
#' For reference see: https://github.com/sagrudd/nanopoRe/blob/master/R/FlowcellLayout.R
#'
#' @param seq_summary A data frame containing the sequencing summary
#' @param platform Flow cell used for sequencing: \code{"minion"} or
#'   \code{"promethion"}. Defaults to \code{"minion"}.
#' @param bin_width Width of each time bin in hours (a single positive number)
#'   for the channel-by-time heatmap. Defaults to 1. Increase for very long runs to limit matrix size.
#'
#' @return plotly object
#' @export
#'
#' @examples
#' \dontrun{
#' pore_activity_heatmap(sample_data, platform = "minion")
#' }
pore_activity_heatmap <- function(seq_summary,
                                  platform = "minion",
                                  bin_width = 1) {

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
  required_cols <- c("run_id", "channel", "sample_id", "start_time",
                     "sequence_length_template", "duration")
  missing_cols <- setdiff(required_cols, colnames(seq_summary))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  assert_condition(is.numeric(seq_summary$duration),
                   "Column duration must be numeric. Please provide a valid argument.")
  assert_condition(is.numeric(bin_width) && length(bin_width) == 1 &&
                     !is.na(bin_width) && bin_width > 0,
                   "Bin_width must be a single positive number of hours. Please provide a valid argument.")
  assert_condition(!anyNA(seq_summary$start_time) && !anyNA(seq_summary$duration),
                   "Columns start_time and duration must not contain NA values. Please provide a valid input.")

  if (!(is_string(platform) && platform %in% c("minion", "promethion"))) {
    stop("Wrong platform specified. Choose either 'minion' or 'promethion'.",
         call. = FALSE)
  }

  assert_condition(dplyr::n_distinct(seq_summary$run_id) == 1,
                   "Sequencing summary must contain only one run_id. Please provide a valid input.")

  ##############################################################################
  # Channel map helpers
  # the following helper functions were kindly provided by Barbara Ottolini,
  # from Oxford Nanopore Technologies,
  # pointing at https://github.com/sagrudd/nanopoRe/blob/master/R/FlowcellLayout.R
  # these should provide the channels layout in MinION and PromethION flow cells

  getPromethIONChannelMap <- function() {
    chunk <- function(i) {
      m <- matrix(seq_len(250), ncol = 10, byrow = TRUE)
      m + i
    }
    layout <- do.call(cbind, lapply(seq(from = 0, to = 2750, by = 250), chunk))
    # channel ids and their (row, col) must be enumerated in the same
    # (column-major) order; pairing as.vector(t(layout)) with which(arr.ind)
    # scrambled the spatial map (only 2/512 MinION channels were in place)
    channelMap <- data.frame(
      channel = as.vector(layout),
      which(layout > 0, arr.ind = TRUE)
    )
    return(channelMap)
  }

  getMinIONChannelMap <- function() {
    blockCalc <- function(i) {
      m <- matrix(seq(i, i + 63, by = 1), ncol = 8, byrow = TRUE)
      cbind(m[seq(5, 8, by = 1), ], m[seq(4), rev(seq(8))])
    }
    layout <- do.call(rbind, lapply(c(1, 449, 385, 321, 257, 193, 129, 65), blockCalc))
    # channel ids and their (row, col) must be enumerated in the same
    # (column-major) order; pairing as.vector(t(layout)) with which(arr.ind)
    # scrambled the spatial map (only 2/512 MinION channels were in place)
    channelMap <- data.frame(
      channel = as.vector(layout),
      which(layout > 0, arr.ind = TRUE)
    )
    return(channelMap)
  }
  ##############################################################################


  # Data prep
  sample_name <- dplyr::first(seq_summary$sample_id)

  # only the columns used below are kept
  tab <- data.frame(channel = seq_summary$channel,
                    start_time = seq_summary$start_time,
                    duration = seq_summary$duration,
                    sequence_length_template = seq_summary$sequence_length_template)
  # real read-end time in hours; the previous rescale() to [0, round(max)]
  # shifted the time axis and, with left-open bins, dropped the earliest read
  # (and the whole temporal panel for runs shorter than 30 min)
  tab$template_unix <- (tab$start_time + tab$duration) / 3600
  first_bin <- floor(min(tab$template_unix) / bin_width)
  last_bin <- floor(max(tab$template_unix) / bin_width)
  bins <- seq(from = first_bin, to = last_bin + 1) * bin_width
  n_bins <- length(bins) - 1

  if (platform == "minion") {
    n_channels <- 512
    layout <- getMinIONChannelMap()
  } else {
    n_channels <- 3000
    layout <- getPromethIONChannelMap()
  }

  # out-of-range channels used to be dropped silently (e.g. PromethION data
  # with the default platform = "minion" lost ~80% of bases from the map)
  if (any(is.na(tab$channel)) || any(tab$channel < 1 | tab$channel > n_channels)) {
    stop("Channel ids must be between 1 and ", n_channels, " for platform '",
         platform, "'. Check the 'platform' argument.", call. = FALSE)
  }

  # Channel activity matrices
  # channels_activity_overtime <- matrix(0, ncol = n_channels, nrow = length(bins) - 1)
  # channels_activity          <- rep(0, n_channels)
  #
  # for (i in seq_len(n_channels)) {
  #   subtab <- tab[channel == i]
  #   channels_activity[i] <- sum(subtab$sequence_length_template)
  #   for (l in seq_len(length(bins) - 1)) {
  #     from      <- bins[l]
  #     to        <- bins[l + 1]
  #     subsubtab <- subtab[template_unix > from & template_unix <= to]
  #     channels_activity_overtime[l, i] <- sum(subsubtab$sequence_length_template)
  #   }
  # }

  ##############################################################################
  # channel activity matrices - grouped sums with rowsum() instead of a
  # per-channel x per-bin nested loop, so the cost follows the number of reads
  # and not n_channels * n_bins
  # left-closed bins [from, to): every read falls into exactly one bin
  tab$channel <- as.integer(tab$channel)
  tab$bin_idx <- as.integer(floor(tab$template_unix / bin_width) - first_bin + 1)

  channels_activity <- rep(0, n_channels)
  channel_totals <- rowsum(tab$sequence_length_template, tab$channel)
  channels_activity[as.integer(rownames(channel_totals))] <- channel_totals[, 1]

  # one sum per (bin, channel) cell, addressed by its column-major position
  # in the n_bins x n_channels matrix
  channels_activity_overtime <- matrix(0, ncol = n_channels, nrow = n_bins)
  cell_idx <- (tab$channel - 1L) * n_bins + tab$bin_idx
  cell_totals <- rowsum(tab$sequence_length_template, cell_idx)
  channels_activity_overtime[as.integer(rownames(cell_totals))] <- cell_totals[, 1]
  ##############################################################################

  channels_activity_labels <- matrix("0", nrow = max(layout$row), ncol = max(layout$col))
  channels_activity_map <- matrix(0, nrow = max(layout$row), ncol = max(layout$col))

  for (m in seq_len(nrow(layout))) {
    r <- layout$row[m]
    c <- layout$col[m]
    label <- layout$channel[m]
    channels_activity_labels[r, c] <- as.character(label)
    channels_activity_map[r, c] <- channels_activity[label]
  }

  # Plots
  p1 <- plotly::plot_ly() %>%
    plotly::add_trace(
      x = as.character(seq_len(n_channels)),
      y = as.character(bins[-length(bins)]),
      z = channels_activity_overtime,
      type = "heatmap",
      xgap = 0.4,
      ygap = 0.4,
      colors = c("#f9f9f9", "#B03060"),
      colorbar = list(x = 1.02, y = 1),
      hovertemplate = "Channel: %{x}<br>Time: %{y} h<br>Bases: %{z}<extra></extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      yaxis = list(
        title = list(text = "<b>Sequencing run-time [h]</b>",
                     font = list(size = 13, family = "Arial")),
        showgrid = FALSE,
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      xaxis = list(
        title = list(text = "",
                     font = list(size = 13, family = "Arial")),
        showgrid = FALSE,
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )

  p2 <- plotly::plot_ly() %>%
    plotly::add_trace(
      z = channels_activity_map,
      type = "heatmap",
      colors = c("#f9f9f9", "#B03060"),
      customdata = apply(channels_activity_labels, 1, as.list),
      hovertemplate = "Channel: %{customdata}<br>Bases: %{z}<extra></extra>",
      xgap = 0.8,
      ygap = 0.8,
      colorbar = list(x = 1.02, y = 0.45)
    ) %>%
    plotly::layout(
      yaxis = list(
        zeroline = FALSE, showline = FALSE,
        showticklabels = FALSE, showgrid = FALSE, ticks = ""
      ),
      xaxis = list(
        zeroline = FALSE, showline = FALSE,
        showticklabels = FALSE, showgrid = FALSE, ticks = ""
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )

  fig <- plotly::subplot(p1, p2, nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.05)

  return(fig)
}
