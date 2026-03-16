
testthat::test_that("Function runs and gives a table", {
  # 1. Give it some fake data
  my_test_data <- data.frame(
    sample_id = "test", run_id = "1", start_time = 0,
    duration = 10, sequence_length_template = 1000,
    passes_filtering = TRUE
  )

  # 2. Run your function
  output <- sequencing_stats(my_test_data)

  # 3. Just check if the output is actually a data frame
  testthat::expect_s3_class(output, "data.frame")

  # 4. Check if the sample ID is still there
  testthat::expect_equal(output$`sample id`, "test")
})



testthat::test_that("sequencing_stats handles correct data accurately", {
  # 1. Create realistic dummy data
  test_df <- data.frame(
    sample_id = c("S1", "S1"),
    run_id = c("R1", "R1"),
    start_time = c(0, 3600),
    duration = c(10, 10),
    sequence_length_template = c(1e9, 1e9), # 2 Gigabases total
    passes_filtering = c(TRUE, FALSE)       # 50% pass rate
  )
  
  output <- sequencing_stats(test_df)
  
  # Check structure
  testthat::expect_s3_class(output, "data.frame")
  testthat::expect_equal(nrow(output), 1)
  
  # Check calculations
  testthat::expect_equal(output$`number of reads`, 2)
  testthat::expect_equal(output$`total bases sequenced [Gb]`, 2.000)
  testthat::expect_equal(output$`passed reads [%]`, 50.00)
})

testthat::test_that("sequencing_stats throws errors for bad input", {
  # Test empty data frame
  testthat::expect_error(
    sequencing_stats(data.frame()), 
    "The input data frame is empty"
  )
  
  # Test missing columns
  bad_df <- data.frame(wrong_col = 1:10)
  testthat::expect_error(
    sequencing_stats(bad_df), 
    "The data frame is missing the 'sample_id' column"
  )
})

testthat::test_that("sequencing_stats handles NAs gracefully", {
  test_df <- data.frame(
    sample_id = "S1", run_id = "R1", start_time = 0,
    duration = 10, 
    sequence_length_template = NA_real_, # Missing length
    passes_filtering = TRUE
  )
  
  # If sum() doesn't have na.rm=TRUE, this might result in NA
  output <- sequencing_stats(test_df)
  testthat::expect_true(is.na(output$`total bases sequenced [Gb]`))
})

