testthat::test_that("plot_average_speed returns a valid ggplot and correct data", {
  # 1. Mock data
  # Speed = length / duration. 
  # Read 1: 1000 bp / 2s = 500 bp/s
  # Read 2: 2000 bp / 4s = 500 bp/s
  test_df <- data.frame(
    sample_id = "Speed_Sample",
    start_time = c(0, 3600, 0),
    duration = c(2, 4, 10),
    passes_filtering = c(TRUE, TRUE, FALSE),
    sequence_length_template = c(1000, 2000, 500)
  )
  
  # 2. Run function
  plt <- plot_average_speed(test_df)
  
  # 3. Basic Object Check
  testthat::expect_s3_class(plt, "ggplot")
  
  # 4. Check Plot Metadata
  testthat::expect_equal(plt$labels$title, "Speed_Sample")
  testthat::expect_equal(plt$labels$y, "Speed [bp/s]")
  testthat::expect_equal(plt$labels$x, "Time [h]")
  
  # 5. Check mapping
  testthat::expect_equal(rlang::as_name(plt$mapping$x), "hour")
  testthat::expect_equal(rlang::as_name(plt$mapping$y), "speed")
  testthat::expect_equal(rlang::as_name(plt$mapping$colour), "pass_status")
})

testthat::test_that("plot_average_speed throws specific assertion messages", {
  
  # 1. Test Empty Data Frame
  testthat::expect_error(
    plot_average_speed(data.frame()), 
    "The input data frame is empty"
  )
  
  # 2. Test Missing Column (Sample ID) - matching your exact string
  df_no_sample <- data.frame(start_time = 0, duration = 10, passes_filtering = TRUE, sequence_length_template = 100)
  testthat::expect_error(
    plot_average_speed(df_no_sample), 
    "The data frame is missing 'sample_id'column"
  )
  
  # 3. Test Logical Check (passes_filtering)
  df_bad_logic <- data.frame(
    sample_id = "S1", start_time = 0, duration = 10,
    passes_filtering = "TRUE", # Character instead of logical
    sequence_length_template = 100
  )
  testthat::expect_error(
    plot_average_speed(df_bad_logic), 
    "Column 'passes_filtering' must be logical"
  )
  
  # 4. Test Numeric Check (duration)
  # Using a factor to avoid "NAs introduced by coercion" warning
  df_bad_dur <- data.frame(
    sample_id = "S1", start_time = 0, 
    duration = factor("10s"), 
    passes_filtering = TRUE,
    sequence_length_template = 100
  )
  testthat::expect_error(
    plot_average_speed(df_bad_dur), 
    "Column 'duration' must be numeric"
  )
  
  # 5. Test Missing Column (start_time)
  df_no_start <- data.frame(sample_id = "S1", duration = 10, passes_filtering = TRUE, sequence_length_template = 100)
  testthat::expect_error(
    plot_average_speed(df_no_start), 
    "The data frame is missing the 'start_time' column"
  )
})