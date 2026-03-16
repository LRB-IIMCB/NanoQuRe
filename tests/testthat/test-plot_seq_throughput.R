
testthat::test_that("plot_seq_throughput returns a valid ggplot with correct data", {
  # 1. Create dummy data
  test_df <- data.frame(
    sample_id = "Sample_123",
    start_time = c(0, 3600, 7200, 0),
    duration = c(10, 10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE, FALSE), # 3 pass, 1 fail
    sequence_length_template = c(1e9, 1e9, 1e9, 1e9)
  )
  
  # 2. Run function
  plt <- plot_seq_throughput(test_df)
  
  # 3. Assertions for ggplot object
  testthat::expect_s3_class(plt, "ggplot")
  
  # 4. Check if the title matches the sample_id
  testthat::expect_equal(plt$labels$title, "Sample_123")
  
  # 5. Check if the mapping uses the correct columns
  testthat::expect_equal(rlang::as_name(plt$mapping$x), "hour")
  testthat::expect_equal(rlang::as_name(plt$mapping$y), "yield_per_hour")
  testthat::expect_equal(rlang::as_name(plt$mapping$colour), "pass_status")
})

testthat::test_that("plot_seq_throughput throws specific assertion messages", {
  
  # 1. Test Missing Column (using your specific messages)
  df_missing <- data.frame(wrong_col = 1)
  testthat::expect_error(
    plot_seq_throughput(df_missing), 
    "The data frame is missing the 'sample_id' column"
  )
  
  # 2. Test Empty Data Frame
  # Note: because you check names first, we need to provide the names but with 0 rows
  df_empty <- data.frame(
    sample_id = character(), start_time = numeric(), 
    duration = numeric(), passes_filtering = logical(), 
    sequence_length_template = numeric()
  )
  testthat::expect_error(
    plot_seq_throughput(df_empty), 
    "The input data frame is empty"
  )
  
  # 3. Test Logical Check
  df_bad_logic <- data.frame(
    sample_id = "S1", start_time = 10, duration = 10,
    sequence_length_template = 100,
    passes_filtering = "TRUE" # Character instead of Logical
  )
  testthat::expect_error(
    plot_seq_throughput(df_bad_logic), 
    "Column 'passes_filtering' must be logical"
  )
  
  # 4. Test Numeric Check
  df_bad_numeric <- data.frame(
    sample_id = "S1", start_time = "zero", duration = 10,
    sequence_length_template = 100,
    passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_seq_throughput(df_bad_numeric), 
    "Column 'start_time' must be numeric"
  )
})