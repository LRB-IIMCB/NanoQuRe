#' Pore Activity Heatmap
#'
#' Generates an interactive heatmap showing channel activity over time and
#' channel activity with respect to their disposition on the flowcell.
#'
#' @param seq_summary A dataframe containing the sequencing summary
#' @param platform Flowcell used for sequencing: \code{"minion"} or
#'   \code{"promethion"}. Defaults to \code{"minion"}.
#'
#' @returns plotly object
#' @import dplyr
#' @importFrom data.table data.table
#' @importFrom plotly plot_ly add_trace layout subplot
#' @export
#'
#' @examples
#' pore_activity_heatmap(sample_data, platform = "minion")
pore_activity_heatmap <- function(seq_summary, platform = "minion") {
  
  time <- 1
  
  # --- Channel map helpers ---
  getPromethIONChannelMap <- function() {
    chunk <- function(i) {
      m <- matrix(seq_len(250), ncol = 10, byrow = TRUE)
      m + i
    }
    layout <- do.call(cbind, lapply(seq(from = 0, to = 2750, by = 250), chunk))
    channelMap <- as.data.frame(cbind(
      channel = as.vector(t(layout)),
      which(layout == as.vector(layout), arr.ind = TRUE)
    ))
    return(channelMap)
  }
  
  getMinIONChannelMap <- function() {
    blockCalc <- function(i) {
      m <- matrix(seq(i, i + 63, by = 1), ncol = 8, byrow = TRUE)
      cbind(m[seq(5, 8, by = 1), ], m[seq(4), rev(seq(8))])
    }
    layout <- do.call(rbind, lapply(c(1, 449, 385, 321, 257, 193, 129, 65), blockCalc))
    channelMap <- as.data.frame(cbind(
      channel = as.vector(t(layout)),
      which(layout == as.vector(layout), arr.ind = TRUE)
    ))
    return(channelMap)
  }
  
  # --- Validation ---
  if (nrow(seq_summary) == 0)
    stop("The input data frame is empty")
  if (!("run_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'run_id' column")
  if (!("channel" %in% names(seq_summary)))
    stop("The data frame is missing the 'channel' column")
  if (!("sample_id" %in% names(seq_summary)))
    stop("The data frame is missing the 'sample_id' column")
  if (!("sequence_length_template" %in% names(seq_summary)))
    stop("The data frame is missing the 'sequence_length_template' column")
  if (!(platform %in% c("minion", "promethion")))
    stop("Wrong platform specified. Choose either 'minion' or 'promethion'")
  if (dplyr::n_distinct(seq_summary$run_id) != 1)
    stop("Sequencing summary must contain only one run_id")
  
  # --- Data prep ---
  sample_name <- dplyr::first(seq_summary$sample_id)
  
  tab      <- data.table::data.table(seq_summary)
  unixtime <- tab$start_time + tab$duration
  runtime  <- round(max(unixtime) / 3600)
  rescaled <- scales::rescale(unixtime, to = c(0, runtime))
  tab$template_unix <- rescaled
  bins <- seq(from = 0, to = max(rescaled), by = time)
  
  if (platform == "minion") {
    n_channels <- 512
    layout     <- getMinIONChannelMap()
  } else {
    n_channels <- 3000
    layout     <- getPromethIONChannelMap()
  }
  
  # --- Channel activity matrices ---
  channels_activity_overtime <- matrix(0, ncol = n_channels, nrow = length(bins) - 1)
  channels_activity          <- rep(0, n_channels)
  
  for (i in seq_len(n_channels)) {
    subtab <- tab[channel == i]
    channels_activity[i] <- sum(subtab$sequence_length_template)
    for (l in seq_len(length(bins) - 1)) {
      from      <- bins[l]
      to        <- bins[l + 1]
      subsubtab <- subtab[template_unix > from & template_unix <= to]
      channels_activity_overtime[l, i] <- sum(subsubtab$sequence_length_template)
    }
  }
  
  channels_activity_labels <- matrix("0", nrow = max(layout$row), ncol = max(layout$col))
  channels_activity_map    <- matrix(0,   nrow = max(layout$row), ncol = max(layout$col))
  
  for (m in seq_len(nrow(layout))) {
    r     <- layout$row[m]
    c     <- layout$col[m]
    label <- layout$channel[m]
    channels_activity_labels[r, c] <- as.character(label)
    channels_activity_map[r, c]    <- channels_activity[label]
  }
  
  # --- Plots ---
  p1 <- plotly::plot_ly() %>%
    plotly::add_trace(
      x             = as.character(seq_len(n_channels)),
      y             = as.character(bins[-length(bins)]),
      z             = channels_activity_overtime,
      type          = "heatmap",
      xgap          = 0.4,
      ygap          = 0.4,
      colors        = c("#f9f9f9", "#B03060"),
      colorbar      = list(x = 1.02, y = 1),
      hovertemplate = "Channel: %{x}<br>Time: %{y} h<br>Bases: %{z}<extra></extra>"
    ) %>%
    plotly::layout(
      title = list(
        text = paste0("<b>", sample_name, "</b>"),
        x    = 0.5,
        font = list(size = 15, color = "#333333", family = "Arial")
      ),
      yaxis = list(
        title    = list(text = "<b>Sequencing run-time [h]</b>",
                        font = list(size = 13, family = "Arial")),
        showgrid = FALSE,
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      xaxis = list(
        title    = list(text = "",
                        font = list(size = 13, family = "Arial")),
        showgrid = FALSE,
        tickfont = list(size = 11, family = "Arial", color = "#333333")
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )
  
  p2 <- plotly::plot_ly() %>%
    plotly::add_trace(
      z             = channels_activity_map,
      type          = "heatmap",
      colors        = c("#f9f9f9", "#B03060"),
      customdata    = apply(channels_activity_labels, 1, as.list),
      hovertemplate = "Channel: %{customdata}<br>Bases: %{z}<extra></extra>",
      xgap          = 0.8,
      ygap          = 0.8,
      colorbar      = list(x = 1.02, y = 0.45)
    ) %>%
    plotly::layout(
      yaxis = list(
        zeroline       = FALSE, showline = FALSE,
        showticklabels = FALSE, showgrid = FALSE, ticks = ""
      ),
      xaxis = list(
        zeroline       = FALSE, showline = FALSE,
        showticklabels = FALSE, showgrid = FALSE, ticks = ""
      ),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )
  
  fig <- plotly::subplot(p1, p2, nrows = 2, titleX = TRUE, titleY = TRUE, margin = 0.05)
  
  return(fig)
}