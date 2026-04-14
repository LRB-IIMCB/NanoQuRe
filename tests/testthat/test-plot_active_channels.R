testthat::test_that("plot_active_channels returns a valid plotly object", {
  
  test_df <- data.frame(
    sample_id  = "Channel_Test",
    channel    = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration   = c(100, 100, 100)
  )
  
  plt <- plot_active_channels(test_df)
  
  # plotly objects are lists with class "plotly"
  testthat::expect_s3_class(plt, "plotly")
})

testthat::test_that("plot_active_channels has correct title and axis labels", {
  
  test_df <- data.frame(
    sample_id  = "Channel_Test",
    channel    = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration   = c(100, 100, 100)
  )
  
  plt <- plot_active_channels(test_df)
  
  # plotly_build() forces the full layout to resolve before we inspect it
  built  <- plotly::plotly_build(plt)
  layout <- built$x$layout
  
  # Title is wrapped in <b> tags in your function
  testthat::expect_equal(layout$title$text, "<b>Channel_Test</b>")
  testthat::expect_equal(layout$xaxis$title$text, "<b>Time [h]</b>")
  testthat::expect_equal(layout$yaxis$title$text, "<b>Number of active channels</b>")
})

testthat::test_that("plot_active_channels calculates channel decay correctly", {
  
  # 3 channels going inactive at 1h, 2h, 3h
  # so active_channels should go: 2, 1, 0
  test_df <- data.frame(
    sample_id  = "Channel_Test",
    channel    = c(1, 2, 3),
    start_time = c(3500, 7100, 10700),
    duration   = c(100, 100, 100)
  )
  
  plt <- plot_active_channels(test_df)
  
  # plotly_build() needed to access resolved trace data
  built <- plotly::plotly_build(plt)
  trace <- built$x$data[[1]]
  
  # at the last time point all channels are inactive
  testthat::expect_equal(min(trace$y), 0)
  
  # at the first time point 2 channels are still active
  testthat::expect_equal(max(trace$y), 2)
})

testthat::test_that("plot_active_channels throws correct error messages", {
  
  # Missing 'channel' column
  testthat::expect_error(
    plot_active_channels(data.frame(start_time = 0, duration = 10, sample_id = "S1")),
    "The data frame is missing the 'channel' column"
  )
  
  # Missing 'sample_id' column
  testthat::expect_error(
    plot_active_channels(data.frame(start_time = 0, duration = 10, channel = 1)),
    "The data frame is missing 'sample_id' column"
  )
  
  # start_time is wrong type (factor avoids coercion warning)
  testthat::expect_error(
    plot_active_channels(data.frame(
      start_time = factor("noon"), duration = 10,
      channel = 1, sample_id = "S1"
    )),
    "Column 'start_time' must be numeric"
  )
  
  # duration is wrong type
  testthat::expect_error(
    plot_active_channels(data.frame(
      start_time = 10, duration = factor("long"),
      channel = 1, sample_id = "S1"
    )),
    "Column 'duration' must be numeric"
  )
})