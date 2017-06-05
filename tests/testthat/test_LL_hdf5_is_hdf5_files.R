library(testthat)

context("is_hdf5_files in user folder or package extdata")

test_that("there are not HDF5 files in the user project folder", {
    expect_false(is_hdf5_files(where = "local"))
})

test_that("there are HDF5 files in package extdata folder", {
    expect_true(is_hdf5_files(where = "package"))
})
