testthat::test_that("plot_double returns a list of two valid ggplot objects", {
  # 1. Mock data
  test_df <- data.frame(
    sample_id = "DoubleSample",
    start_time = c(0, 3600, 7200),
    duration = c(10, 10, 10),
    passes_filtering = c(TRUE, TRUE, TRUE),
    sequence_length_template = c(100, 500, 1000),
    mean_qscore_template = c(8, 10, 12)
  )
  
  # 2. Run function
  result <- plot_double(test_df)
  
  # 3. Check list structure
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("length_plot", "qscore_plot"))
  
  # 4. Check Plot 1 (Length)
  testthat::expect_s3_class(result$length_plot, "ggplot")
  testthat::expect_equal(result$length_plot$labels$title, "Length DoubleSample")
  testthat::expect_equal(result$length_plot$labels$y, "Length [bp]")
  
  # 5. Check Plot 2 (Q-score)
  testthat::expect_s3_class(result$qscore_plot, "ggplot")
  testthat::expect_equal(result$qscore_plot$labels$title, "Q-score DoubleSample")
  testthat::expect_equal(result$qscore_plot$labels$y, "Q-score")
})

testthat::test_that("plot_double throws specific assertion messages", {
  
  # 1. Test Empty Data Frame
  testthat::expect_error(
    plot_double(data.frame()), 
    "The input data frame is empty"
  )
  
  # 2. Test Logical Check
  # Using character "TRUE" instead of logical TRUE to trigger error
  df_bad_logic <- data.frame(
    sample_id = "S1", start_time = 0, duration = 10,
    sequence_length_template = 100, mean_qscore_template = 10,
    passes_filtering = "TRUE" 
  )
  testthat::expect_error(
    plot_double(df_bad_logic), 
    "Column 'passes_filtering' must be logical"
  )
  
  # 3. Test Numeric Check
  # Using factor to avoid "NAs introduced by coercion" warning
  df_bad_numeric <- data.frame(
    sample_id = "S1", 
    start_time = factor("12:00"), 
    duration = 10,
    sequence_length_template = 100, mean_qscore_template = 10,
    passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_double(df_bad_numeric), 
    "Column 'start_time' must be numeric"
  )
  
  # 4. Test Missing Column (Duration)
  df_no_dur <- data.frame(
    sample_id = "S1", start_time = 0, 
    passes_filtering = TRUE,
    sequence_length_template = 100, mean_qscore_template = 10
  )
  testthat::expect_error(
    plot_double(df_no_dur), 
    "The data frame is missing the 'duration' column"
  )
})