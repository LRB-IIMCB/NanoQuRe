# STYLE NOTE: refactored comments for clarity, added NA and duplicate tests

testthat::test_that("calculate_n50 returns correct N50 value", {
  
  # Lengths: 10, 4, 3, 2, 1 | Total = 20, half = 10
  # Sorted descending cumsum: 10 (>= 10 ✓) → N50 = 10
  test_df_1 <- data.frame(sequence_length_template = c(1, 2, 3, 4, 10))
  testthat::expect_equal(calculate_n50(test_df_1), 10)
  
  # Lengths: 100, 100, 20, 20, 10 | Total = 250, half = 125
  # Sorted descending cumsum: 100, 200 (>= 125 ✓) → N50 = 100
  test_df_2 <- data.frame(sequence_length_template = c(100, 100, 20, 20, 10))
  testthat::expect_equal(calculate_n50(test_df_2), 100)
})

testthat::test_that("calculate_n50 handles edge cases", {
  
  # Single row: that read IS the N50
  testthat::expect_equal(
    calculate_n50(data.frame(sequence_length_template = 5000)),
    5000
  )
  
  # All identical values: N50 = that value
  testthat::expect_equal(
    calculate_n50(data.frame(sequence_length_template = c(100, 100, 100))),
    100
  )
  
  # NA values: na.rm = TRUE in sum(), so NAs should be ignored
  # Non-NA values: 10, 4 | Total = 14, half = 7
  # Sorted descending cumsum: 10 (>= 7 ✓) → N50 = 10
  test_na <- data.frame(sequence_length_template = c(10, 4, NA))
  testthat::expect_equal(calculate_n50(test_na), 10)
})

testthat::test_that("calculate_n50 throws correct error messages", {
  
  # Missing the required column entirely
  testthat::expect_error(
    calculate_n50(data.frame(wrong_col = 1)),
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # Correct column name but zero rows
  testthat::expect_error(
    calculate_n50(data.frame(sequence_length_template = numeric())),
    "The input data frame is empty"
  )
  
  # Column exists but is the wrong type (factor avoids coercion warnings)
  testthat::expect_error(
    calculate_n50(data.frame(sequence_length_template = factor("1000"))),
    "Column 'sequence_length_template' must be numeric"
  )
})