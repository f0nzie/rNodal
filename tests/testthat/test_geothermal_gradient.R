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
    expect_true(all.equal(expected, result))
})


context("geothermal well C44")
test_that("return list has been read", {
  geothermal_txt <- "
    TVD   temp
    0     120
    3590  150
  "
  result <- as_dataframe_geothermal_data(geothermal_txt)
  expected <- geothermal_c44
  expect_true(all.equal(expected, result))
})


context("geothermal well P44")
test_that("return list has been read", {
  geothermal_txt <- c("
    TVD    temp
    0       80
    9700   180
    ")
  result <- as_dataframe_geothermal_data(geothermal_txt)
  expected <- geothermal_p44
  expect_true(all.equal(expected, result))
})


context("geothermal well T01_OW")
test_that("return list has been read", {
  geothermal_txt <- c("
    TVD    temp
    0      60
    600    40
    9000   210
    ")
  result <- as_dataframe_geothermal_data(geothermal_txt)
  expected <- geothermal_t01_ow
  expect_true(all.equal(expected, result))
})
