library(testthat)

context("test_low_level")

test_that("test_low_level()", {
    res <- test_dir(test_path('test_low_level'), reporter = "silent")
    df <- as.data.frame(res)
    df$user <- df$system  <- df$real <- NULL

    expect_equal_to_reference(df, "test_dir.rds")
})


test_that('test_low_level() helpers', {
    res <- test_dir('test_low_level', reporter = 'silent', filter = 'hdf5_basic')
    df <- as.data.frame(res)
    expect_true(all(!df$error & df$failed == 0))
})
