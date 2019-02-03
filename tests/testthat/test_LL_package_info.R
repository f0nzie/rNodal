library(testthat)
# test functions in package R.utils

context("test if package installed")

test_that("rNodal is installed", {
    result <- R.utils::isPackageInstalled("rNodal")
    expect_true(result)
})

test_that("rNodal is loaded", {
    result <- R.utils::isPackageLoaded("rNodal")
    expect_true(result)
})

