library(testthat)

context("HDF5 management")

test_that("hdf5 file is created", {
    setHDF5DumpFile()
    hdf5_file <- getHDF5DumpFile()
    expect_true(file.exists(hdf5_file))
})

test_that("hdf5 file has a name", {
    setHDF5DumpFile()
    hdf5_name <- getHDF5DumpName()
    expect_equal(hdf5_name, "/HDF5ArrayAUTO00000")
})

test_that(".check_HDF5_dump_file returns error with no argument", {
    setHDF5DumpFile()
    expect_error(.check_HDF5_dump_file(), 'argument "file" is missing, with no default')
})


test_that(".check_HDF5_dump_file creates a file here", {
    setHDF5DumpFile()
    hdf5_file <- "any_file.h5"
    .check_HDF5_dump_file(hdf5_file)
    expect_true(file.exists(hdf5_file))
    if (file.exists(hdf5_file)) file.remove(hdf5_file)    # clean up
})

# test_that("setHDF5DumpFile can write to indicated file name", {
# hdf5_file <- "./data/another_file.h5"
# file_content <- setHDF5DumpFile(hdf5_file)
# expect_true(file.exists(hdf5_file))
# })
