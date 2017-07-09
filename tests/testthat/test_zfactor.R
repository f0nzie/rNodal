library(testthat)

context("test functions at Zfactor.R")

test_that("Hall-Yarborough 14.7, 60, 0.65", {
    result <- z.hallyarborough(14.7, 60, 0.65)
    expect_equal(result, 0.9972209, tolerance = 1e-7)
})


test_that("Hall-Yarborough 14.7, 60, 0.70", {
    result <- z.hallyarborough(14.7, 60, 0.70)
    expect_equal(result, 0.9968212, tolerance = 1e-7)
})
