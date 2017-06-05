library(testthat)

context("test_paths")

# disabling test lines because they do not work while building check
#
# pkg_root_folder <- system.file("..", package = "rNodal")
# print(pkg_root_folder)
# user_root_folder <- rprojroot::find_rstudio_root_file()
# print(user_root_folder)

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

test_that("system.file(pkg) match rNodal/inst", {
    res <- unlist(strsplit(system.file(package = "rNodal"), split = "/"))
    .res <- paste(res[length(res)-1], res[length(res)], sep = "/")
    expect_equal(.res, "rNodal/inst")
})



