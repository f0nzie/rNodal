library(testthat)

context("test paths")


test_that("testthat is the last folder in getwd()",
    expect_true(c("testthat") %in% unlist(strsplit(getwd(), split = "/")))
)

test_that("relative path is a dot",
          expect_equal(R.utils::getRelativePath(getwd()), ".") )

# test_that("parent folder of getwd() is tests", {
#     res <- unlist(strsplit(R.utils::getParent(getwd()), split = "/"))
#     .res <- res[length(res)]
#     expect_equal(.res, "tests")
# })

# test_that("system.file(pkg) creates Temp dir at check time", {
#     res <- unlist(strsplit(system.file(package = "rNodal"), split = "/"))
#     .res <- paste(res[length(res)-1], res[length(res)], sep = "/")
#     if (is_checking_package()) {
#         # happens during check
#         expect_true("Temp" %in% res)
#         expect_true("rNodal" %in% res)
#     } else {
#         # happens when only testing
#         expect_equal(.res, "rNodal/inst")
#     }
# })


