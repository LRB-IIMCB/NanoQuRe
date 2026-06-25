testthat::test_that("generate_report errors when 'sample_id' column is missing", {
  
  bad_df <- data.frame(start_time = 0, duration = 10)
  
  testthat::expect_error(
    generate_report(bad_df),
    "The data frame is missing the 'sample_id' column."
  )
})

testthat::test_that("generate_report errors on a non-existent file path", {
  
  testthat::expect_error(
    generate_report("this/path/does/not/exist.tsv"),
    "File not found"
  )
})

testthat::test_that("generate_report errors on an unsupported input type", {
  
  testthat::expect_error(
    generate_report(list(1, 2, 3)),
    "Each input must be either a file path"
  )
})

testthat::test_that("generate_report errors on an empty data frame", {
  
  testthat::expect_error(
    generate_report(data.frame()),
    "The input data frame is empty."
  )
})

testthat::test_that("generate_report errors on an invalid platform", {
  
  test_df <- data.frame(
    sample_id = "S1", run_id = "R1", channel = 1,
    start_time = 0, duration = 10,
    sequence_length_template = 100, mean_qscore_template = 10,
    passes_filtering = TRUE
  )
  
  testthat::expect_error(
    generate_report(test_df, platform = "gridion"),
    "platform must be either"
  )
})

# --- The tests below render a full report and are slower (~5s+ each), driven
# by pore_activity_heatmap()'s fixed per-channel loop cost regardless of how
# little data is supplied. ---

testthat::test_that("generate_report renders a single-sample report into output_dir", {
  
  test_df <- data.frame(
    sample_id = "OnlySample", run_id = "R1", channel = 1:2,
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = TRUE
  )
  
  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  out <- generate_report(test_df, output_file = "single_test.html", output_dir = out_dir)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  
  # written to the requested directory, not the package install directory
  testthat::expect_identical(out, file.path(out_dir, "single_test.html"))
  testthat::expect_true(file.exists(out))
  
  # single-sample template was selected, not the multi-sample one
  testthat::expect_true(grepl("OnlySample", html, fixed = TRUE))
  testthat::expect_false(grepl("Multiple Samples", html, fixed = TRUE))
})

testthat::test_that("generate_report binds multiple data frame inputs and renders the multi-sample report", {
  
  df1 <- data.frame(
    sample_id = "A", run_id = "R1", channel = 1,
    start_time = 0, duration = 5,
    sequence_length_template = 100, mean_qscore_template = 8,
    passes_filtering = TRUE
  )
  df2 <- data.frame(
    sample_id = "B", run_id = "R2", channel = 1,
    start_time = 0, duration = 5,
    sequence_length_template = 150, mean_qscore_template = 9,
    passes_filtering = TRUE
  )
  
  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  out <- generate_report(df1, df2, output_file = "multi_test.html", output_dir = out_dir)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  
  testthat::expect_identical(out, file.path(out_dir, "multi_test.html"))
  testthat::expect_true(grepl("Multiple Samples", html, fixed = TRUE))
})

testthat::test_that("generate_report accepts a TSV file path input", {
  
  tmp_tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp_tsv), add = TRUE)
  
  readr::write_tsv(
    data.frame(
      sample_id = "FileInput", run_id = "R1", channel = 1,
      start_time = 0, duration = 5,
      sequence_length_template = 100, mean_qscore_template = 8,
      passes_filtering = TRUE
    ),
    tmp_tsv
  )
  
  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  
  out <- generate_report(tmp_tsv, output_file = "file_input_test.html", output_dir = out_dir)
  
  testthat::expect_true(file.exists(out))
})
