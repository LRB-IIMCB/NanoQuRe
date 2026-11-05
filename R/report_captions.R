################################################################################
# DETAILED CAPTIONS FOR THE REPORT
################################################################################
#' Report Caption Description
#'
#' Returns the fixed, data-independent description of a report section:
#' what the plot shows and how its quantity is computed. Used by both report
#' templates so the single- and multi-sample texts cannot diverge.
#'
#' @param section One of "read_lengths", "cumulative_yield", "quality",
#'   "speed", "throughput", "channels", "length_qscore", "heatmap".
#'
#' @returns A character string
#' @noRd
caption_description <- function(section) {

  descriptions <- c(
    read_lengths = paste(
      "Histogram of read lengths: number of reads per length bin on a log10",
      "axis. The pink line marks the mean read length and the green line the",
      "N50, the length such that half of all sequenced bases are in reads at",
      "least this long. Both are computed from all reads, pass and fail."
    ),
    cumulative_yield = paste(
      "Cumulative number of sequenced bases (Gb) over the run, ordered by",
      "read start time, separately for reads that passed and failed the",
      "basecaller quality filter."
    ),
    quality = paste(
      "Number of reads per mean read Q-score, coloured by the basecaller",
      "pass/fail flag. The dashed orange line is the cut-off set in NanoQuRe",
      "(Q7 by default). The solid purple line is the lowest Q-score among",
      "passing reads, an estimate of the threshold the basecaller applied."
    ),
    speed = paste(
      "Sequencing speed per hour: bases in reads starting in that hour",
      "divided by the summed duration of those reads, separately for pass and",
      "fail reads. Read duration also includes time outside the basecalled",
      "sequence (e.g. the adapter), so this underestimates translocation",
      "speed slightly. Hours without reads are left as gaps."
    ),
    throughput = paste(
      "Bases sequenced in each hour of the run, by read start time,",
      "separately for pass and fail reads. Hours without reads are shown as",
      "zero. The last hour may be incomplete."
    ),
    channels = paste(
      "Number of channels with at least one read in each 10-minute window.",
      "Dashed lines mark the first window in which this number falls to 50%",
      "and 25% of the highest count in the first hour. Pore scans and pauses",
      "are not recorded in the sequencing summary and can cause short dips",
      "or gaps."
    ),
    length_qscore = paste(
      "Maximum, mean and minimum read length (top, log axis) and mean read",
      "Q-score (bottom) for reads starting in each hour of the run."
    ),
    heatmap = paste(
      "Top: bases produced by each channel in each hour of the run, by read",
      "end time. Bottom: total bases per channel, arranged by the channel",
      "layout of the selected flow cell type. Channels without reads are",
      "shown empty."
    )
  )

  if (!(section %in% names(descriptions)))
    stop("Unknown report section: ", section)

  return(descriptions[[section]])
}


#' Report Caption Numbers
#'
#' Returns one sentence of values computed from the data for a report
#' section. It states quantities only, never their causes.
#'
#' @param section See \code{caption_description}.
#' @param seq_summary A dataframe containing the sequencing summary of one
#'   sample
#' @param platform \code{"minion"} or \code{"promethion"}; sets the number
#'   of channels on the flow cell.
#'
#' @returns A character string
#' @noRd
caption_numbers <- function(section, seq_summary, platform = "minion") {

  lengths <- seq_summary$sequence_length_template
  passed  <- seq_summary$passes_filtering
  qscores <- seq_summary$mean_qscore_template

  num <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits)
  gb  <- function(x) format(signif(x / 1e9, 3), scientific = FALSE)
  med <- function(x) if (length(x) == 0) "none" else num(stats::median(x), 1)
  rate <- function(keep) {
    if (!any(keep)) "none" else
      paste0(num(sum(lengths[keep]) / sum(seq_summary$duration[keep])), " bases/s")
  }

  n_channels <- if (platform == "promethion") 3000 else 512

  text <- switch(section,
                 read_lengths = paste0(
                   num(length(lengths)), " reads; mean length ", num(mean(lengths)),
                   " nt; N50 ", num(calculate_n50(seq_summary)), " nt; longest read ",
                   num(max(lengths)), " nt."
                 ),
                 cumulative_yield = paste0(
                   "Total ", gb(sum(lengths)), " Gb, of which ", gb(sum(lengths[passed])),
                   " Gb (", num(100 * sum(lengths[passed]) / sum(lengths), 1),
                   "%) in pass reads."
                 ),
                 quality = paste0(
                   "Median read Q-score: pass ", med(qscores[passed]), ", fail ",
                   med(qscores[!passed]), "; lowest Q-score among pass reads: ",
                   if (any(passed)) num(min(qscores[passed]), 1) else "no pass reads", "."
                 ),
                 speed = paste0(
                   "Over the whole run: pass reads ", rate(passed), ", fail reads ",
                   rate(!passed), "."
                 ),
                 throughput = {
                   hourly <- tapply(lengths, floor(seq_summary$start_time / 3600), sum)
                   paste0(
                     "Highest hourly yield ", gb(max(hourly)), " Gb in hour ",
                     names(hourly)[which.max(hourly)], "; last hour (", names(hourly)[length(hourly)],
                     ") ", gb(hourly[[length(hourly)]]), " Gb (pass and fail together)."
                   )
                 },
                 channels = paste0(
                   num(dplyr::n_distinct(seq_summary$channel)), " of ", num(n_channels),
                   " channels produced at least one read during the run."
                 ),
                 length_qscore = paste0(
                   "Median read length ", num(stats::median(lengths)), " nt; median read Q-score ",
                   med(qscores), "."
                 ),
                 heatmap = paste0(
                   num(n_channels - dplyr::n_distinct(seq_summary$channel)), " of ",
                   num(n_channels), " channels produced no reads."
                 ),
                 stop("Unknown report section: ", section)
  )

  return(text)
}
