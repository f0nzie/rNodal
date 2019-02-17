library(testthat)

rda_file <- "expected_geothermal.rda"
if (!file.exists(rda_file))
  stop("No test table.\nUse the table generator first\n")
load(rda_file)


context("geothermal C13")

test_that("return list has been read", {
    geothermal_txt <- c("
      TVD    temp
      0      120
      2670   150
    ")

    result <- as_dataframe_geothermal_data(geothermal_txt)
    expected <- geothermal_c13
    expect_true(identical(expected, result))




})
