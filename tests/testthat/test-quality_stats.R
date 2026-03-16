testthat::test_that("quality_stats returns the correct table structure and values", {
  # Mock data
  test_df <- data.frame(
    sample_id = "SampleA",
    run_id = "Run1",
    passes_filtering = c(TRUE, FALSE),
    mean_qscore_template = c(10, 12),
    sequence_length_template = c(100, 200)
  )
  
  output <- quality_stats(test_df)
  
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_equal(output$`sample id`, "SampleA")
  testthat::expect_equal(output$`all reads`, 2)
  testthat::expect_equal(output$`mean qscore`, 11)
})

testthat::test_that("quality_stats throws specific assertion messages", {
  
  # 1. Test Empty Data Frame
  testthat::expect_error(
    quality_stats(data.frame()), 
    "The input data frame is empty")
  
  # 2. Test Numeric Check (Mean Qscore)
  # We provide the column but as a character to trigger this specific message
  df_char_qscore <- data.frame(
    mean_qscore_template = "High", 
    sequence_length_template = 100,
    sample_id = "S1", run_id = "R1", passes_filtering = TRUE )
  testthat::expect_error(
    quality_stats(df_char_qscore), 
    "Column 'mean_qscore_template' must be numeric"
  )
  
  # 3. Test Numeric Check (Sequence Length)
  df_char_len <- data.frame(
    mean_qscore_template = 10, 
    sequence_length_template = "Long",
    sample_id = "S1", run_id = "R1", passes_filtering = TRUE
  )
  testthat::expect_error(
    quality_stats(df_char_len), 
    "Column 'sequence_length_template' must be numeric"
  )
  
  # 4. Test Missing Column (Sample ID)
  # We must provide the first two numeric columns so R reaches this check
  df_no_sample <- data.frame(
    mean_qscore_template = 10, 
    sequence_length_template = 100,
    run_id = "R1", passes_filtering = TRUE
  )
  testthat::expect_error(
    quality_stats(df_no_sample), 
    "The data frame is missing the 'sample_id' column"
  )
  
  # 5. Test Missing Column (Run ID)
  df_no_run <- data.frame(
    mean_qscore_template = 10, 
    sequence_length_template = 100,
    sample_id = "S1", passes_filtering = TRUE
  )
  testthat::expect_error(
    quality_stats(df_no_run), 
    "The data frame is missing the 'run_id' column"
  )
})