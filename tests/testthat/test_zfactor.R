library(testthat)

context("test functions at Zfactor.R")

test_that("Hall-Yarborough give correct value", {
    result <- z.hallyarborough(14.7, 60, 0.65)
    expect_equal(result$z, 0.9964127, tolerance = 1e-7)
    result <- z.hallyarborough(14.7, 60, 0.70)
    expect_equal(result$z, 0.9957821, tolerance = 1e-7)
    # print(result)
})
