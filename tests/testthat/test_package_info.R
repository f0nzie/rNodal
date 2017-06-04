library(testthat)


result <- R.utils::isPackageInstalled("rNodal")
expect_true(result)

result <- R.utils::isPackageLoaded("rNodal")
expect_true(result)
