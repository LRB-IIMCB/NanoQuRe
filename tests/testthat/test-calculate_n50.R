testthat::test_that("calculate_n50 returns the correct mathematical N50 value", {
  # 1. Mock data where we know the N50
  # Lengths: 10, 4, 3, 2, 1
  # Total sum = 20. Half = 10.
  # Descending Cumulative Sum:
  # 10 (>= 10 is TRUE!) -> Index 1.
  # So N50 should be 10.
  test_df_1 <- data.frame(sequence_length_template = c(1, 2, 3, 4, 10))
  testthat::expect_equal(calculate_n50(test_df_1), 10)
  
  # 2. Another check
  # Lengths: 100, 100, 20, 20, 10
  # Total = 250. Half = 125.
  # Cumulative: 100, 200 (>= 125 is TRUE!) -> Index 2.
  # So N50 should be 100.
  test_df_2 <- data.frame(sequence_length_template = c(100, 100, 20, 20, 10))
  testthat::expect_equal(calculate_n50(test_df_2), 100)
})

testthat::test_that("calculate_n50 throws specific assertion messages", {
  
  # 1. Test Missing Column
  testthat::expect_error(
    calculate_n50(data.frame(wrong_col = 1)), 
    "The data frame is missing the 'sequence_length_template' column"
  )
  
  # 2. Test Empty Data Frame
  # We provide the column name but 0 rows to trigger the empty check
  testthat::expect_error(
    calculate_n50(data.frame(sequence_length_template = numeric())), 
    "The input data frame is empty"
  )
  
  # 3. Test Non-numeric Column
  # Using a factor to avoid "NAs introduced by coercion" warnings
  df_bad_type <- data.frame(sequence_length_template = factor("1000"))
  testthat::expect_error(
    calculate_n50(df_bad_type), 
    "Column 'sequence_length_template' must be numeric"
  )
})

testthat::test_that("calculate_n50 handles single-row data", {
  # If there is only one read, that read is the N50
  test_df <- data.frame(sequence_length_template = 5000)
  testthat::expect_equal(calculate_n50(test_df), 5000)
})