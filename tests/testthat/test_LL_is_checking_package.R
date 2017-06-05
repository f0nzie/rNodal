library(testthat)

context("is_checking_package function")

prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))
# from check:
#      "C:/Users/msfz751/Documents/rNodal.Rcheck"
# from test:
#      "C:/Users/msfz751/Documents/rNodal"

test_that("the folder for rNodal changes during devtools::check", {
    if (is_checking_package())
        expect_true("rNodal.Rcheck" %in% unlist(strsplit(getwd(), split = "/")))
})

test_that(".Rcheck is part of the getwd() path when devtools::check", {
    if (is_checking_package())
        expect_true(grepl(".Rcheck", prj_dir))
})


test_that("getwd() contains these member in its path", {
    result <- unlist(strsplit(getwd(), split = "/"))
    if (is_checking_package()) {
        # expect_true(is_checking_package())
        expect_true("testthat" %in% result)
        expect_true("tests" %in% result)
        expect_true("rNodal.Rcheck" %in% result)
    }
})
