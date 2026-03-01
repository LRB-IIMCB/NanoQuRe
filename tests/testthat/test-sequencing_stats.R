
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





