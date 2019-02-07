library(testthat)

context("test is_checking_package function")

prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))
# from check package mode:
#      "C:/Users/msfz751/Documents/rNodal.Rcheck"
# from testing mode:
#      "C:/Users/msfz751/Documents/rNodal"

test_that("the folder for rNodal changes during devtools::check", {
    if (is_checking_package())
        expect_true("rNodal.Rcheck" %in% unlist(strsplit(getwd(), split = "/")))
    else
        expect_false(is_checking_package())
})


test_that(".Rcheck is part of the getwd() path when devtools::check", {
    if (is_checking_package())
        expect_true(grepl(".Rcheck", prj_dir))
    else
        expect_false(grepl(".Rcheck", prj_dir))
})


test_that("getwd() contains these member in its path", {
    result <<- unlist(strsplit(getwd(), split = "/"))
    # print(result)
    # print(R.version$os)
    if (is_checking_package()) {

        expect_true("testthat" %in% result)
        expect_true("tests" %in% result)
        expect_true("rNodal.Rcheck" %in% result)
    }
    else {

        # not building package but testing
        expect_true("testthat" %in% result)
        expect_true("tests" %in% result || "rNodal-tests" %in% result)
        expect_false("rNodal.Rcheck" %in% result)
    }

})
