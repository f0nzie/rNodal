library(testthat)

context("is_checking_package")

prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))
# from check:
#      "C:/Users/msfz751/Documents/rNodal.Rcheck"
# from test:
#      "C:/Users/msfz751/Documents/rNodal"


if (is_checking_package()) # print(getwd()) else print(prj_dir)
    expect_false("rNodal" %in% getwd())

if (is_checking_package())  expect_true(grepl(".Rcheck", prj_dir))


if (is_checking_package()) expect_true(is_checking_package())
