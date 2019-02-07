library(testthat)

context("test paths")


test_that("testthat is the last folder in getwd()",
    expect_true(c("testthat") %in% unlist(strsplit(getwd(), split = "/")))
)

test_that("relative path is a dot",
          expect_equal(R.utils::getRelativePath(getwd()), ".") )

test_that("parent folder of getwd() is tests", {
    # split the current path
    res <- unlist(strsplit(R.utils::getParent(getwd()), split = "/"))
    .res <- res[length(res)] # get the last part of the path
    cat("\n")
    print(res)
    print(.res)
    expect_true((.res == "tests") || ("rNodal-tests" %in% res))
})

test_that("system.file(pkg) creates a Temp dir at check time", {
    res <- unlist(strsplit(system.file(package = "rNodal"), split = "/"))
    # join previous and next string
    .res <- paste(res[length(res)-1], res[length(res)], sep = "/")
    cat("\n")
    print(res)
    print(.res)
    if (is_checking_package()) {
        print(R.version$os)
        # happens during check
        cat("in package ...")
        # expect_true("Temp" %in% res)
        expect_true("rNodal" %in% res)
        # passes tests in Travis
        if(.Platform$OS.type == "unix") expect_true("rNodal.Rcheck" %in% res)
        if(.Platform$OS.type == "windows") expect_true("Temp" %in% res)
    } else {
        # happens when only testing
        cat("\t testing now ...")
        if(.Platform$OS.type == "windows") {
            expect_true(.res == "rNodal/inst" || ("Temp" %in% res))
        }

    }
})


