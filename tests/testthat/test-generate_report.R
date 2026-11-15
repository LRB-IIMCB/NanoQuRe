testthat::test_that("generate_report errors when 'sample_id' column is missing", {

  bad_df <- data.frame(start_time = 0, duration = 10)

  testthat::expect_error(
    generate_report(bad_df),
    "Required columns missing: sample_id"
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
    "Empty data frame provided as an input"
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
    "Platform must be either"
  )
})

testthat::test_that("generate_report validates platform, output_file and output_dir", {

  test_df <- data.frame(
    sample_id = "S1", run_id = "R1", channel = 1,
    start_time = 0, duration = 10,
    sequence_length_template = 100, mean_qscore_template = 10,
    passes_filtering = TRUE
  )

  testthat::expect_error(
    generate_report(test_df, platform = c("minion", "promethion")),
    "Platform must be a single character string"
  )
  testthat::expect_error(
    generate_report(test_df, platform = 1),
    "Platform must be a single character string"
  )
  testthat::expect_error(
    generate_report(test_df, output_file = c("a.html", "b.html")),
    "Output_file must be a single character string"
  )
  testthat::expect_error(
    generate_report(test_df, output_file = "sub/x.html"),
    "use output_dir instead"
  )
  testthat::expect_error(
    generate_report(test_df, output_dir = NULL),
    "Output_dir must be a single character string"
  )
})

# --- The tests below render a full report and are slower (~5s+ each), driven
# by pore_activity_heatmap()'s fixed per-channel loop cost regardless of how
# little data is supplied. ---

testthat::test_that("generate_report renders a single-sample report into output_dir", {
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


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
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


  df1 <- data.frame(
    sample_id = "SampleAlpha", run_id = "R1", channel = 1,
    start_time = 0, duration = 5,
    sequence_length_template = 100, mean_qscore_template = 8,
    passes_filtering = TRUE
  )
  df2 <- data.frame(
    sample_id = "SampleBeta", run_id = "R2", channel = 1,
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
  testthat::expect_true(grepl("SampleAlpha", html, fixed = TRUE))
  testthat::expect_true(grepl("SampleBeta", html, fixed = TRUE))
})

testthat::test_that("generate_report accepts a TSV file path input", {
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


  tmp_tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp_tsv), add = TRUE)

  vroom::vroom_write(
    data.frame(
      sample_id = "FileInput", run_id = "R1", channel = 1,
      start_time = 0, duration = 5,
      sequence_length_template = 100, mean_qscore_template = 8,
      passes_filtering = TRUE
    ),
    tmp_tsv, delim = "\t"
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  out <- generate_report(tmp_tsv, output_file = "file_input_test.html", output_dir = out_dir)

  testthat::expect_true(file.exists(out))
})


testthat::test_that("generate_report errors on missing sample_id values", {

  df <- data.frame(sample_id = c("A", NA), passes_filtering = c(TRUE, FALSE))

  testthat::expect_error(
    generate_report(df),
    "Column sample_id contains missing values"
  )
})

testthat::test_that("generate_report errors on missing passes_filtering values", {

  df <- data.frame(sample_id = c("A", "A"), passes_filtering = c(TRUE, NA))

  testthat::expect_error(
    generate_report(df),
    "Column passes_filtering contains missing values"
  )
})

testthat::test_that("generate_report errors on unrecognised passes_filtering values", {

  df <- data.frame(sample_id = c("A", "A"), passes_filtering = c("yes", "no"))

  testthat::expect_error(
    generate_report(df),
    "Column passes_filtering must contain TRUE/FALSE or 1/0 values"
  )
})

testthat::test_that("generate_report accepts passes_filtering stored as text", {
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


  # some summary files carry the flag as "True"/"False" strings
  test_df <- data.frame(
    sample_id = "TextFlags", run_id = "R1", channel = 1:2,
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = c("True", "False")
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  out <- generate_report(test_df, output_file = "text_flags_test.html", output_dir = out_dir)

  testthat::expect_true(file.exists(out))
})

testthat::test_that("generate_report renders a single-sample report when the heatmap fails", {
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


  # channel 600 does not exist on the default minion platform
  test_df <- data.frame(
    sample_id = "BadChannel", run_id = "R1", channel = c(1, 600),
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = TRUE
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  out <- generate_report(test_df, output_file = "heatmap_notice_test.html", output_dir = out_dir)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  testthat::expect_true(file.exists(out))
  testthat::expect_true(grepl("Pore activity heatmap could not be generated", html, fixed = TRUE))
})


testthat::test_that("generate_report writes nothing next to the installed template", {
  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")


  test_df <- data.frame(
    sample_id = "OnlySample", run_id = "R1", channel = 1:2,
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = TRUE
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  # read-only (system) libraries fail if knit intermediates land here
  template_dir <- system.file("rmd", package = "NanoQuRe")
  testthat::expect_true(nzchar(template_dir))
  before <- list.files(template_dir, recursive = TRUE, all.files = TRUE)
  generate_report(test_df, output_file = "clean_test.html", output_dir = out_dir)

  testthat::expect_identical(
    list.files(template_dir, recursive = TRUE, all.files = TRUE),
    before
  )
})

testthat::test_that("generate_report renders two TSV inputs with numeric-looking sample ids", {

  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  make_tsv <- function(id) {
    path <- tempfile(fileext = ".tsv")
    vroom::vroom_write(
      data.frame(
        sample_id = id, run_id = "R1", channel = 1:2,
        start_time = c(0, 10), duration = 5,
        sequence_length_template = c(100, 200),
        mean_qscore_template = c(8, 9), passes_filtering = TRUE
      ),
      path, delim = "\t"
    )
    return(path)
  }
  tsv1 <- make_tsv("101")
  tsv2 <- make_tsv("202")

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(c(tsv1, tsv2, out_dir), recursive = TRUE), add = TRUE)

  out <- generate_report(tsv1, tsv2, output_file = "numeric_ids_test.html", output_dir = out_dir)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  testthat::expect_true(grepl("Multiple Samples", html, fixed = TRUE))
  testthat::expect_true(grepl("101", html, fixed = TRUE))
  testthat::expect_true(grepl("202", html, fixed = TRUE))
})

testthat::test_that("generate_report renders the heatmap on promethion for a channel above 512", {

  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  test_df <- data.frame(
    sample_id = "ProSample", run_id = "R1", channel = c(1, 600),
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = TRUE
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  out <- generate_report(test_df, output_file = "promethion_test.html",
                         output_dir = out_dir, platform = "promethion")
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  testthat::expect_false(grepl("could not be generated", html, fixed = TRUE))
})

testthat::test_that("generate_report warns when one sample spans several run_ids", {

  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  test_df <- data.frame(
    sample_id = "MixedSample", run_id = c("R1", "R2"), channel = 1:2,
    start_time = c(0, 10), duration = 5,
    sequence_length_template = c(100, 200),
    mean_qscore_template = c(8, 9), passes_filtering = TRUE
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  testthat::expect_warning(
    out <- generate_report(test_df, output_file = "mixed_test.html", output_dir = out_dir),
    "run_id"
  )
  # the report is still rendered
  testthat::expect_true(file.exists(out))
})

testthat::test_that("generate_report errors when no input is given", {

  testthat::expect_error(
    generate_report(),
    "Sequencing summary is missing"
  )
})

testthat::test_that("generate_report takes sample_id from the file name and reads only report columns", {

  testthat::skip_on_cran()
  testthat::skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")

  # a raw summary: no sample_id, one column the report does not use
  in_dir <- tempfile("nqr_in_")
  dir.create(in_dir)
  tsv <- file.path(in_dir, "RawSummaryName.txt")
  vroom::vroom_write(
    data.frame(
      run_id = "R1", channel = 1:2,
      start_time = c(0, 10), duration = 5,
      sequence_length_template = c(100, 200),
      mean_qscore_template = c(8, 9), passes_filtering = TRUE,
      unused_column = "UnusedMarkerValue"
    ),
    tsv, delim = "\t"
  )

  out_dir <- tempfile("nqr_test_")
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  out <- generate_report(tsv, output_file = "derived_id_test.html", output_dir = out_dir)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  testthat::expect_true(grepl("RawSummaryName", html, fixed = TRUE))
  testthat::expect_false(grepl("RawSummaryName.txt", html, fixed = TRUE))
  testthat::expect_false(grepl("UnusedMarkerValue", html, fixed = TRUE))
})
