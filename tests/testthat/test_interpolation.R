library(testthat)


context("interpolation")

test_that("GLR > 3000", {
    result <- interp.fanbr(drhov = 10, GLR = 3200, degree = 4)$ff
    expect_equal(result, 0.009646559)
})

test_that("GLR <= 1500", {
    result <- interp.fanbr(drhov = 10, GLR = 1500, degree = 4)$ff
    expect_equal(result, 0.02258941)
})

test_that("GLR < 1500", {
    result <- interp.fanbr(drhov = 10, GLR = 1000, degree = 4)$ff
    expect_equal(result, 0.03546744)
})


test_that("3000 < GLR < 1500", {
    result <- interp.fanbr(drhov = 10, GLR = 2000, degree = 4)$ff
    expect_equal(result, 0.02258941)
})
