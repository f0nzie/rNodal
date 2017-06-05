library(testthat)

context("is_checking_package")

prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))

if (is_checking_package()) print(getwd()) else print(prj_dir)

if (is_checking_package())
    expect_true(grepl(".Rcheck", prj_dir))
