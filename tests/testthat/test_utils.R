library(testthat)

context("test stopif()")

test_that("stopif all false returns NULL", {
    expect_equal(NULL, stopif(FALSE))
    expect_equal(NULL, stopif(FALSE, FALSE))
    expect_equal(NULL, stopif(FALSE, FALSE, FALSE))
})

test_that("stopif causes error if condition is TRUE", {
    expect_error(stopif(TRUE))
    expect_error(stopif(TRUE, FALSE))
    expect_error(stopif(TRUE, FALSE, TRUE))
})

test_that("stopif causes error if condition is TRUE", {
    expect_equal(NULL, stopif())
})


context("test get_list_examples()")

test_that("examples is empty", {
    expect_error(get_list_examples()) # must specify the package
    examples <- get_list_examples(aPackage = "rNodal")
    # print(class(examples))
    if (length(examples) == 0) expect_equal(unlist(examples), character())

})

context("shift()")

test_that("", {
    x <- c(1, 2, 3)
    sx <- shift(x, 1)
    # print(sx)
    expect_true(identical(sx, c(NA, 1, 2)))
    sx <- shift(x, -1)
    # print(sx)
    expect_true(identical(sx, c(2, 3, NA)))
    sx <- shift(x, 0)
    expect_true(identical(sx, c(1, 2, 3)))
    expect_error(shift(x))
})
