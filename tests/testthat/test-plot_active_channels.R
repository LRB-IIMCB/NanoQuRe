testthat::test_that("plot_active_channels returns a valid ggplot and correctly calculates decay", {
  # 1. Mock data
  # We have 3 channels. 
  # Channel 1 ends at 1h, Channel 2 at 2h, Channel 3 at 3h.
  test_df <- data.frame(
    sample_id = "Channel_Test",
    channel = c(1, 2, 3),
    start_time = c(3500, 7100, 10700), # seconds
    duration = c(100, 100, 100)        # ends at exactly 1h, 2h, 3h
  )
  
  # 2. Run function
  plt <- plot_active_channels(test_df)
  
  # 3. Object Check
  testthat::expect_s3_class(plt, "ggplot")
  
  # 4. Check Metadata
  testthat::expect_equal(plt$labels$title, "Channel_Test")
  testthat::expect_equal(plt$labels$x, "Time [h]")
  testthat::expect_equal(plt$labels$y, "Number of active channels")
  
  # 5. Verify the data behind the plot
  # The maximum y value should be the total number of unique channels (3)
  plot_data <- ggplot2::ggplot_build(plt)$plot$data
  testthat::expect_equal(max(plot_data$channel_no_start), 3)
  
  # Check if active_channels decreases
  # At the last time point, active_channels should be 0
  testthat::expect_equal(min(plot_data$active_channels), 0)
})

testthat::test_that("plot_active_channels throws specific assertion messages", {
  
  # 1. Test Missing Column 'channel'
  df_no_chan <- data.frame(start_time = 0, duration = 10, sample_id = "S1")
  testthat::expect_error(
    plot_active_channels(df_no_chan), 
    "The data frame is missing the 'channel' column"
  )
  
  # 2. Test Missing Column 'sample_id'
  df_no_sample <- data.frame(start_time = 0, duration = 10, channel = 1)
  testthat::expect_error(
    plot_active_channels(df_no_sample), 
    "The data frame is missing 'sample_id' column"
  )
  
  # 3. Test Numeric Check (start_time)
  # Using a factor to avoid "NAs introduced by coercion" warnings
  df_bad_start <- data.frame(
    start_time = factor("noon"), 
    duration = 10, 
    channel = 1, 
    sample_id = "S1"
  )
  testthat::expect_error(
    plot_active_channels(df_bad_start), 
    "Column 'start_time' must be numeric"
  )
  
  # 4. Test Numeric Check (duration)
  df_bad_dur <- data.frame(
    start_time = 10, 
    duration = factor("long"), 
    channel = 1, 
    sample_id = "S1"
  )
  testthat::expect_error(
    plot_active_channels(df_bad_dur), 
    "Column 'duration' must be numeric"
  )
})