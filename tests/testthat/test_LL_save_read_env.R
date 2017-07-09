library(testthat)

context("test save/read to environment")

test_that("save value to environment", {
    expect_equal(readFromProjectEnv("PRES.ATM"), 14.7)
    saveToProjectEnv("PRES.ATM", 14.69)
    expect_equal(readFromProjectEnv("PRES.ATM"), 14.69)
})

test_that("can read most important variables", {
    expect_equal(readFromProjectEnv("TEMP.STD"), 60)
    expect_equal(readFromProjectEnv("TEMP.RANKINE"), 460)

})
