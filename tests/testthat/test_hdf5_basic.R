library(testthat)

context("HDF5 basic functions")

library(rhdf5)
load("test_hdf5_basic.rda")
h5_file <- "ex_hdf5file.h5"

if (file.exists(h5_file)) file.remove(h5_file)
result <- h5createFile(h5_file)
expect_true(result)

# write a matrix
.B = array(seq(0.1,2.0, by=0.1), dim=c(5,3,2))
attr(.B, "scale") <- "liter"
result <- h5write(.B, h5_file, "B")
expect_equal(result, NULL)

# read a matrix
result = h5read(h5_file,"B")
expect_equal(result, B)

# write and read submatrix
h5createDataset(h5_file, "S", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file=h5_file, name="S", index=list(NULL,1))
result <- h5read(h5_file, "S")
expect_equal(result, S)

result <- h5read(h5_file, "S", index=list(NULL,2:3))
expect_equal(result, SS)

# list content of hdf5 file
h5ls(h5_file)
H5close()
if (file.exists(h5_file)) file.remove(h5_file)
# save(B, S, SS, file = "test_hdf5_basic.rda")


