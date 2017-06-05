library(testthat)

context("is_hdf5_files")

expect_false(is_hdf5_files(where = "local"))
expect_true(is_hdf5_files(where = "package"))
