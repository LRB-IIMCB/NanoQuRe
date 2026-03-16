testthat::test_that("plot_read_lengths returns a plot with correct lines", {
  # 1. Mock data
  test_df <- data.frame(
    sample_id = "Sample_A",
    sequence_length_template = c(100, 200, 300, 400, 500)
  )
  
  # 2. Run function (ensure calculate_n50 and nanoqure_theme exist)
  plt <- plot_read_lengths(test_df, upper_limit = 1000)
  
  # 3. Assertions for the plot object
  testthat::expect_s3_class(plt, "ggplot")
  testthat::expect_equal(plt$labels$title, "Sample_A")
  
  # 4. Check if the vertical lines (Mean and N50) are present
  # In ggplot, these are usually layers 3 and 4 based on your assembly order
  layers <- sapply(plt$layers, function(x) class(x$geom)[1])
  testthat::expect_true("GeomVline" %in% layers)
  
  # 5. Check if the x-axis limit was applied correctly
  testthat::expect_equal(plt$coordinates$limits$x, c(0, 1000))
})

testthat::test_that("plot_read_lengths throws specific assertion messages", {
  
  # 1. Test missing column
  testthat::expect_error(
    plot_read_lengths(data.frame(x = 1)), 
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # 2. Test non-numeric length column
  df_bad_type <- data.frame(
    sample_id = "S1", 
    sequence_length_template = "100"
  )
  testthat::expect_error(
    plot_read_lengths(df_bad_type), 
    "Column 'sequence_length_template' must be numeric"
  )
  
  # 3. Test invalid upper_limit (e.g., passing a word)
  test_df <- data.frame(sample_id = "S1", sequence_length_template = 100)
  testthat::expect_error(
    plot_read_lengths(test_df, upper_limit = "abc"), 
    "upper_limit must be a number"
  )
})

testthat::test_that("plot_read_lengths handles numeric strings for upper_limit", {
  # Your function uses as.numeric(upper_limit), 
  # so passing "5000" as a string should actually work!
  test_df <- data.frame(sample_id = "S1", sequence_length_template = 100)
  
  testthat::expect_no_error(
    plot_read_lengths(test_df, upper_limit = "5000")
  )
})