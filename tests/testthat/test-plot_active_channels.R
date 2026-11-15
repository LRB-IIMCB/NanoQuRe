testthat::test_that("plot_active_channels returns a valid plotly object", {

  test_df <- data.frame(
    sample_id = "Channel_Test",
    channel = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration = c(100, 100, 100)
  )

  plt <- plot_active_channels(test_df)

  # plotly objects are lists with class "plotly"
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_active_channels has correct title and axis labels", {

  test_df <- data.frame(
    sample_id = "Channel_Test",
    channel = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration = c(100, 100, 100)
  )

  plt <- plot_active_channels(test_df)

  # plotly_build() forces the full layout to resolve before we inspect it
  built <- plotly::plotly_build(plt)
  layout <- built$x$layout

  # Title is wrapped in <b> tags in your function
  testthat::expect_equal(layout$title$text, "<b>Channel_Test</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of active channels</b>")
})

testthat::test_that("plot_active_channels counts channels with a read in each window", {

  # one read per channel at ~1 h, ~2 h and ~3 h; between the reads no
  # channel is producing anything, so those windows must be zero
  test_df <- data.frame(
    sample_id = "Channel_Test",
    channel = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration = c(100, 100, 100)
  )

  plt <- plot_active_channels(test_df)

  # plotly_build() needed to access resolved trace data
  built <- plotly::plotly_build(plt)
  trace <- built$x$data[[1]]

  # never more than one channel active at the same time
  testthat::expect_equal(max(trace$y), 1)

  # windows between reads are kept, with zero active channels
  testthat::expect_true(any(trace$y == 0))

  # each read overlaps a window boundary, so it is active in two windows
  testthat::expect_equal(sum(trace$y), 6)
})

testthat::test_that("plot_active_channels places thresholds where the count drops", {

  # four channels producing reads for 2 h, then only channel 1 until 4 h
  starts <- c(seq(0, 7000, by = 20), seq(0, 7000, by = 20),
              seq(0, 7000, by = 20), seq(0, 14000, by = 20))
  test_df <- data.frame(
    sample_id = "Channel_Test",
    channel = rep(c(2, 3, 4, 1), c(351, 351, 351, 701)),
    start_time = starts,
    duration = 5
  )

  built <- plotly::plotly_build(plot_active_channels(test_df, bin_width = 0.5))
  trace <- built$x$data[[1]]
  threshold_x <- unlist(lapply(built$x$data[-1], function(tr) tr$x[1]))

  testthat::expect_equal(max(trace$y), 4)
  # 50% (2 channels) and 25% (1 channel) are both first reached at 2 h
  testthat::expect_equal(threshold_x, c(2, 2))
})

testthat::test_that("plot_active_channels errors on a non-positive bin_width", {

  test_df <- data.frame(
    sample_id = "S1", channel = 1, start_time = 0, duration = 10
  )

  testthat::expect_error(
    plot_active_channels(test_df, bin_width = 0),
    "Bin_width must be a positive number of hours"
  )
})

testthat::test_that("plot_active_channels throws correct error messages", {

  # missing argument
  testthat::expect_error(plot_active_channels(), "Sequencing summary is missing")

  # empty data frame
  testthat::expect_error(plot_active_channels(data.frame()),
                         "Empty data frame provided")

  # Missing 'channel' column
  testthat::expect_error(
    plot_active_channels(data.frame(start_time = 0, duration = 10, sample_id = "S1")),
    "Required columns missing: channel"
  )

  # Missing 'sample_id' column
  testthat::expect_error(
    plot_active_channels(data.frame(start_time = 0, duration = 10, channel = 1)),
    "Required columns missing: sample_id"
  )

  # start_time is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_active_channels(data.frame(
      start_time = factor("noon"), duration = 10,
      channel = 1, sample_id = "S1"
    )),
    "Column start_time must be numeric"
  )

  # duration is wrong type
  testthat::expect_error(
    plot_active_channels(data.frame(
      start_time = 10, duration = factor("long"),
      channel = 1, sample_id = "S1"
    )),
    "Column duration must be numeric"
  )
})

testthat::test_that("plot_active_channels errors on invalid thresholds", {

  test_df <- data.frame(
    sample_id = "S1", channel = 1, start_time = 0, duration = 10
  )

  for (bad in list(0, c(0.5, 1.5), -0.1, NA_real_, "0.5", numeric(0))) {
    testthat::expect_error(
      plot_active_channels(test_df, thresholds = bad),
      "Thresholds must be numeric fractions greater than 0 and at most 1"
    )
  }
})

testthat::test_that("plot_active_channels errors on an invalid bin_width type or length", {

  test_df <- data.frame(
    sample_id = "S1", channel = 1, start_time = 0, duration = 10
  )

  testthat::expect_error(
    plot_active_channels(test_df, bin_width = "10"),
    "Bin_width must be a positive number of hours"
  )
  testthat::expect_error(
    plot_active_channels(test_df, bin_width = c(0.1, 0.2)),
    "Bin_width must be a positive number of hours"
  )
})

testthat::test_that("plot_active_channels errors when no read has complete data", {

  test_df <- data.frame(
    sample_id = "S1", channel = 1, start_time = NA_real_, duration = 10
  )

  testthat::expect_error(
    plot_active_channels(test_df),
    "No reads with start_time, duration and channel available"
  )
})

testthat::test_that("plot_active_channels notes a threshold that is never reached", {

  # two channels stay active in every window, so 50% (1 channel) is never hit
  starts <- seq(0, 7200, by = 20)
  test_df <- data.frame(
    sample_id = "S1",
    channel = rep(c(1, 2), each = length(starts)),
    start_time = rep(starts, 2),
    duration = 5
  )

  built <- plotly::plotly_build(
    plot_active_channels(test_df, thresholds = 0.5, bin_width = 0.5)
  )

  testthat::expect_length(built$x$data, 1)
  testthat::expect_match(built$x$layout$annotations[[1]]$text,
                         "50% threshold\\(s\\) not reached")
})

testthat::test_that("plot_active_channels ignores the partial final window for crossings", {

  # four channels for 2 h, then only channel 1 in the last (partial) window;
  # the drop in the final window must not count as a threshold crossing
  starts <- seq(0, 7199, by = 20)
  test_df <- data.frame(
    sample_id = "S1",
    channel = c(rep(1:4, each = length(starts)), 1),
    start_time = c(rep(starts, 4), 7300),
    duration = 5
  )

  built <- plotly::plotly_build(plot_active_channels(test_df, bin_width = 0.5))

  # only the active-channel line: no threshold line was drawn
  testthat::expect_length(built$x$data, 1)
  testthat::expect_equal(tail(built$x$data[[1]]$y, 1), 1)
})

testthat::test_that("plot_active_channels labels thresholds without floating-point noise", {

  starts <- seq(0, 7199, by = 20)
  test_df <- data.frame(
    sample_id = "S1",
    channel = c(rep(1:10, each = length(starts)), rep(1, 400)),
    start_time = c(rep(starts, 10), seq(7200, by = 20, length.out = 400)),
    duration = 5
  )

  built <- plotly::plotly_build(
    plot_active_channels(test_df, thresholds = 0.29, bin_width = 0.5)
  )

  nms <- unlist(lapply(built$x$data, function(tr) tr$name))
  testthat::expect_true("29% of first-hour maximum" %in% nms)
})
