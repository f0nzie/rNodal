library(testthat)

context("test_paths\n")

# disabling test lines because they do not work while building
#
# pkg_root_folder <- system.file("..", package = "rNodal")
# print(pkg_root_folder)
# user_root_folder <- rprojroot::find_rstudio_root_file()
# print(user_root_folder)

# print(getwd())
# print(R.utils::getAbsolutePath(getwd()))
# print(R.utils::getRelativePath(getwd()))
# print(R.utils::getParent(getwd()))
# print(R.utils::getParent(R.utils::getParent(getwd())))

prj_dir <- R.utils::getParent(R.utils::getParent(getwd()))
# from check:
#      "C:/Users/msfz751/Documents/rNodal.Rcheck"
# from test:
#      "C:/Users/msfz751/Documents/rNodal"

expect_true(grepl(".Rcheck", prj_dir))
# expect_equal(prj_dir, "C:/Users/msfz751/Documents/rNodal")

if (is_checking_package()) print(getwd()) else print(prj_dir)

expect_true(is_checking_package())



