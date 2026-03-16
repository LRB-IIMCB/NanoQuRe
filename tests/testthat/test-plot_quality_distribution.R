testthat::test_that("plot_quality_distribution returns a valid ggplot and correct data", {
  # 1. Mock data
  test_df <- data.frame(
    sample_id = "Quality_Sample",
    sequence_length_template = c(100, 200, 300, 400),
    mean_qscore_template = c(6, 8, 10, 12),
    passes_filtering = c(FALSE, TRUE, TRUE, TRUE)
  )
  
  # 2. Run function
  plt <- plot_quality_distribution(test_df, qscore_cutoff = 7)
  
  # 3. Object checks
  testthat::expect_s3_class(plt, "ggplot")
  testthat::expect_equal(plt$labels$title, "Quality_Sample")
  
  # 4. Check internal math for y-axis limit
  # 4 rows * 0.05 = 0.2
  testthat::expect_equal(plt$coordinates$limits$y, c(0, 0.2))
  
  # 5. Check if the vline exists
  layer_geoms <- sapply(plt$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomVline" %in% layer_geoms)
})

testthat::test_that("plot_quality_distribution throws specific assertion messages", {
  
  # 1. Test Empty Data Frame
  testthat::expect_error(
    plot_quality_distribution(data.frame()), 
    "The input data frame is empty."
  )
  
  # 2. Test Missing Column 'sequence_length_template'
  # We provide other columns to ensure it reaches this specific check
  df_no_len <- data.frame(
    sample_id = "S1", 
    mean_qscore_template = 10, 
    passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_quality_distribution(df_no_len), 
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # 3. Test Missing Column 'sample_id'
  df_no_sample <- data.frame(
    sequence_length_template = 100, 
    mean_qscore_template = 10, 
    passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_quality_distribution(df_no_sample), 
    "Missing 'sample_id' column"
  )
  
  # 4. Test Non-numeric Q-score column
  # Using a factor triggers is.numeric == FALSE without a coercion warning
  df_bad_q <- data.frame(
    sample_id = "S1",
    sequence_length_template = 100,
    mean_qscore_template = factor("High"),
    passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_quality_distribution(df_bad_q), 
    "Q-score column must be numeric"
  )
  
  # 5. Test Invalid qscore_cutoff
  # We pass NA specifically to trigger your !is.na(qscore_cutoff) check 
  # This avoids the "NAs introduced by coercion" warning
  test_df <- data.frame(
    sample_id = "S1", sequence_length_template = 100,
    mean_qscore_template = 10, passes_filtering = TRUE
  )
  testthat::expect_error(
    plot_quality_distribution(test_df, qscore_cutoff = NA), 
    "qscore_cutoff must be a number"
  )
})

testthat::test_that("plot_quality_distribution handles numeric strings for qscore_cutoff", {
  # This ensures as.numeric("7.5") works as intended
  test_df <- data.frame(
    sample_id = "S1", sequence_length_template = 100,
    mean_qscore_template = 10, passes_filtering = TRUE
  )
  
  testthat::expect_no_error(plot_quality_distribution(test_df, qscore_cutoff = "7.5"))
})