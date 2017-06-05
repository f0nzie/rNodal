library(testthat)

context("test_paths")


test_that("testthat is the last folder in getwd()",
    expect_true(c("testthat") %in% unlist(strsplit(getwd(), split = "/")))
)

test_that("relative path is a dot",
          expect_equal(R.utils::getRelativePath(getwd()), ".") )

test_that("parent folder of getwd() is tests", {
    res <- unlist(strsplit(R.utils::getParent(getwd()), split = "/"))
    .res <- res[length(res)]
    expect_equal(.res, "tests")
})

test_that("system.file(pkg) creates Temp dir at check time", {
    res <- unlist(strsplit(system.file(package = "rNodal"), split = "/"))
    .res <- paste(res[length(res)-1], res[length(res)], sep = "/")
    if (is_checking_package()) {
        # happens during check
        expect_true("Temp" %in% res)
        expect_true("rNodal" %in% res)
    } else {
        # happens when only testing
        expect_equal(.res, "rNodal/inst")
    }

})



# checking tests ... ERROR
# Running 'testthat.R'
# Running the tests in 'tests/testthat.R' failed.
# Last 13 lines of output:
#     1/1 mismatches
# x[1]: "RLIBS_4b94ef4537/rNodal"
# y[1]: "rNodal/inst"
#
#
#
# [1] "C:/Users/msfz751/AppData/Local/Temp/Rtmpc5fq1D/RLIBS_4b94ef4537/rNodal/../inst/extdata/default.hdf5"
# [1] "default.hdf5"
# [1] "C:/Users/msfz751/Documents/rNodal.Rcheck/tests/testthat/default.hdf5"
# testthat results ================================================================
#     OK: 13 SKIPPED: 0 FAILED: 1
# 1. Failure: system.file(pkg) match rNodal/inst (@test_LL_paths.R#22)
