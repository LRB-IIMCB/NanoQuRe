
testthat::test_that("sequencing_stats has all columns",{
  expect_type("sample id", "character")
  expect_type("duration [h]", "integrer")
})





