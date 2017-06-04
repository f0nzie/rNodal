library(testthat)

context("nodal_status function")
cat("\n")

# Goal: trying to find if there is a Hdf5 file present
result <- nodal_status()
print(result)
# from test:
#    "C:/Users/msfz751/Documents/rNodal/default.hdf5"
# from check()
#    x[1]: "C:/Users/msfz751/AppData/Local/Temp/Rtmp080rW5/RLIBS_6ca440db6735/rNodal/
#          ../inst/extdata/default.hdf5"

expected <- basename(paste(getwd(), "default.hdf5", sep = "/"))
print(expected)
# from test:
#     "default.hdf5"
# from check:
#    y[1]: "C:/Users/msfz751/Documents/rNodal.Rcheck/tests/testthat/default.hdf5"

expected <- paste(getwd(), "default.hdf5", sep = "/")
print(expected)
# from test:
#     "C:/Users/msfz751/Documents/rNodal/tests/testthat/default.hdf5"
# from check:
#

setwd("../..")
expected <- paste(getwd(), "default.hdf5", sep = "/")
# "C:/Users/msfz751/Documents/rNodal"

# expect_equal(result, expected)

#
# pkg_name <- getwd()
# # "C:/Users/msfz751/Documents/rNodal/tests/testthat"
#
# # shortPathName
# pkg_name <- shortPathName(getwd())
# # "C:\\Users\\msfz751\\DOCUME~1\\rNodal\\tests\\testthat"
#
#
#
# pkg_name <- setwd("../..")
# pkg_name <- getwd()
#
# prj_dir_above <- dirname(getwd())
#
# expect_equal(pkg_name, "rNodal")
# # expect_equal(prj_dir_above, "C:/Users/msfz751/Documents")
