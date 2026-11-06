# Minimal sample: 2 pass reads, 1 fail read, 2 channels, 2 hours
make_caption_df <- function() {
  data.frame(
    sample_id = "Caption_Sample",
    channel = c(1L, 2L, 1L),
    start_time = c(0, 100, 3700),
    duration = c(10, 10, 10),
    sequence_length_template = c(1000, 3000, 2000),
    mean_qscore_template = c(12, 14, 6),
    passes_filtering = c(TRUE, TRUE, FALSE)
  )
}

testthat::test_that("caption_numbers reports values computed from the data", {

  df <- make_caption_df()

  testthat::expect_equal(
    NanoQuRe:::caption_numbers("read_lengths", df),
    "3 reads; mean length 2,000 nt; N50 3,000 nt; longest read 3,000 nt."
  )
  testthat::expect_match(
    NanoQuRe:::caption_numbers("cumulative_yield", df),
    "(66.7%) in pass reads", fixed = TRUE
  )
  testthat::expect_equal(
    NanoQuRe:::caption_numbers("channels", df),
    "2 of 512 channels produced at least one read during the run."
  )
  testthat::expect_match(
    NanoQuRe:::caption_numbers("heatmap", df, platform = "promethion"),
    "2,998 of 3,000 channels", fixed = TRUE
  )
})

testthat::test_that("caption_numbers handles a sample without fail reads", {

  df <- make_caption_df()
  df$passes_filtering <- TRUE

  speed <- NanoQuRe:::caption_numbers("speed", df)

  testthat::expect_match(speed, "fail reads none", fixed = TRUE)
  testthat::expect_false(grepl("NaN|Inf", speed))
})

testthat::test_that("every report section has a description and values", {

  sections <- c("read_lengths", "cumulative_yield", "quality", "speed",
                "throughput", "channels", "length_qscore", "heatmap")

  for (section in sections) {
    testthat::expect_true(nchar(NanoQuRe:::caption_description(section)) > 0)
    testthat::expect_true(nchar(NanoQuRe:::caption_numbers(section, make_caption_df())) > 0)
  }
})

testthat::test_that("caption functions error on an unknown section", {

  testthat::expect_error(NanoQuRe:::caption_description("nope"), "Unknown report section")
  testthat::expect_error(NanoQuRe:::caption_numbers("nope", make_caption_df()), "Unknown report section")
})
