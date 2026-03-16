testthat::test_that("plot_cumulative_yield returns a valid ggplot and correct data", {
  # 1. Mock data
  # We use 1e9 (1 billion) to get exactly 1, 2, 3 Gb in the cumsum
  test_df <- data.frame(
    sample_id = "Cum_Sample",
    start_time = c(0, 3600, 7200, 0),
    duration = c(10, 10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE, FALSE),
    sequence_length_template = c(1e9, 1e9, 1e9, 0.5e9)
  )
  
  # 2. Run function
  plt <- plot_cumulative_yield(test_df)
  
  # 3. Basic Object Check
  testthat::expect_s3_class(plt, "ggplot")
  
  # 4. Check if the Title is correct
  testthat::expect_equal(plt$labels$title, "Cum_Sample")
  
  # 5. Check Axis Labels
  testthat::expect_equal(plt$labels$x, "Time [h]")
  testthat::expect_equal(plt$labels$y, "Yield [Gb]")
  
  # 6. Verify Mapping
  testthat::expect_equal(rlang::as_name(plt$mapping$x), "h_start_time")
  testthat::expect_equal(rlang::as_name(plt$mapping$y), "bases_gb")
  testthat::expect_equal(rlang::as_name(plt$mapping$colour), "pass_status")
})

testthat::test_that("plot_cumulative_yield throws specific assertion messages", {
  
  # 1. Test Missing Column (Check names first as per your code)
  df_missing <- data.frame(wrong_col = 1)
  testthat::expect_error(
    plot_cumulative_yield(df_missing), 
    "The data frame is missing the 'sample_id' column"
  )
  
  # 2. Test Numeric Check (sequence_length_template)
  # Using a factor to trigger is.numeric == FALSE without a warning
  df_bad_type <- data.frame(
    sample_id = "S1", start_time = 0, duration = 10,
    passes_filtering = TRUE,
    sequence_length_template = factor("100")
  )
  testthat::expect_error(
    plot_cumulative_yield(df_bad_type), 
    "Column 'sequence_length_template' must be numeric"
  )
  
  # 3. Test Empty Data Frame
  # We provide the names but 0 rows to trigger the nrow > 0 check
  df_empty <- data.frame(
    sample_id = character(), start_time = numeric(), 
    duration = numeric(), passes_filtering = logical(), 
    sequence_length_template = numeric()
  )
  testthat::expect_error(
    plot_cumulative_yield(df_empty), 
    "The input data frame is empty"
  )
  
  # 4. Test Logical Check
  df_bad_logic <- data.frame(
    sample_id = "S1", start_time = 10, duration = 10,
    sequence_length_template = 100,
    passes_filtering = 1 # Numeric instead of Logical
  )
  testthat::expect_error(
    plot_cumulative_yield(df_bad_logic), 
    "Column 'passes_filtering' must be logical"
  )
})