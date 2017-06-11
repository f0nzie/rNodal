library(testthat)

context("nodal_status function")

# Goal: trying to find if there is a Hdf5 file present

test_that("getwd() match folders when testing or checking", {
    result <- getwd()
    if (!is_checking_package()) {
        expect_true(result == "C:/Users/msfz751/Documents/rNodal/tests/testthat" ||
            result == "I:/src/rNodal/tests/testthat")
    } else {
        expect_equal(result, "C:/Users/msfz751/Documents/rNodal.Rcheck/tests/testthat")
    }
})

test_that("there is a HDF5 file under extdata", {
    result <- nodal_status()
    if (!is_checking_package()) {
        expect_equal(result, "C:/Users/msfz751/Documents/rNodal/inst/extdata/default.hdf5")
    } else {
        # during devtools::check() the extdata files are saved under a Temp folder
        # "C:/Users/msfz751/AppData/Local/Temp/Rtmpwte0U6/RLIBS_36c05e7c1415/rNodal/inst/extdata/default.hdf5"
        result <- unlist(strsplit(result, "/"))
        expect_true("Temp" %in% result)
        expect_true("AppData" %in% result)
        expect_true("default.hdf5" %in% result)
    }
})

test_that("basename of getwd() match *testthat*", {
    result <- basename(getwd())
    if (!is_checking_package()) {
        expect_equal(result, "testthat")
    } else {
        expect_equal(result, "testthat")
    }
})

