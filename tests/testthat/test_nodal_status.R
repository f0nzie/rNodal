library(testthat)

context("nodal_status function")


test_that("basename of getwd() match *testthat*", {
    result <- basename(getwd())
    if (!is_checking_package()) {
        expect_equal(result, "testthat")
    } else {
        expect_equal(result, "testthat")
    }
})

