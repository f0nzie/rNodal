library(testthat)


context("geothermal C13")

test_that("return list has been read", {
    geothermal_txt <- c("
      TVD    temp
      0      120
      2670   150
    ")

    result <- as_dataframe_geothermal_data(geothermal_txt)
    expected <- geothermal_c13
    print(expected)




})
