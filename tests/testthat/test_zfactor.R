library(testthat)

context("test functions at Zfactor.R")

test_that("Hall-Yarborough 14.7, 60, 0.65", {
    result <- z.hallyarborough(14.7, 60, 0.65)
    expect_equal(result, 0.9972209, tolerance = 1e-7)
})


test_that("Hall-Yarborough 14.7, 60, 0.70", {
    result <- z.hallyarborough(14.7, 60, 0.70)
    expect_equal(result, 0.9968212, tolerance = 1e-7)
})


test_that("z Hall-Yarborough as a list 14.7, 60, 0.60", {
    result <- z.hallyarborough(pres.a = 14.7, temp.f = 60, gas.sg = 0.6, as_list = TRUE)
    expected = list(
        z       = 0.9975919,
        pres.pr = 0.02179802,
        temp.pr = 1.465212,
        temp.r  = 0.682495)
    expect_equal(result, expected, tolerance = 1e-6)
})


test_that("z Hall-Yarborough as a list 200, 120, 0.70", {
    result <- z.hallyarborough(pres.a = 200, temp.f = 120, gas.sg = 0.7, as_list = TRUE)
    expected = list(
        z       = 0.9699606,
        pres.pr = 0.2991766,
        temp.pr = 1.504025,
        temp.r  = 0.6648824)
    expect_equal(result, expected, tolerance = 1e-6)
})


test_that("z Hall-Yarborough as a list 500, 150, 0.70", {
    result <- z.hallyarborough(pres.a = 500, temp.f = 150, gas.sg = 0.7, as_list = TRUE)
    expected = list(
        z       = 0.9387585,
        pres.pr = 0.7479416,
        temp.pr = 1.58182,
        temp.r  = 0.6321833)
    expect_equal(result, expected, tolerance = 1e-6)
})


test_that("z Hall-Yarborough as a list 1500, 250, 0.70", {
    result <- z.hallyarborough(pres.a = 1500, temp.f = 250, gas.sg = 0.7, as_list = TRUE)
    expected = list(
        z       = 0.9180181,
        pres.pr = 2.243825,
        temp.pr = 1.841134,
        temp.r  = 0.5431434)
    expect_equal(result, expected, tolerance = 1e-6)
    # print(result)
})

test_that("z Hall-Yarborough as a list 3000, 280, 0.70", {
    result <- z.hallyarborough(pres.a = 3000, temp.f = 280, gas.sg = 0.75, as_list = TRUE)
    expected = list(
        z       = 0.913885,
        pres.pr = 4.507445,
        temp.pr = 1.845391,
        temp.r  = 0.5418905)
    expect_equal(result, expected, tolerance = 1e-6)
    # print(result)
})



test_that("HY matrix from zFactor HY matches solution of Ppr, Tpr matrix", {

    tpr <- seq(1.40, 2.0, 0.05)      # Tpr range for wells
    ppr <- seq(0.02, 8.0, 0.5)       # Ppr range for wells

    # hy <- zFactor::z.HallYarborough(ppr, tpr); save(hy, file = "hy_13x16.rda")
    load(file = "hy_13x16.rda");
    expect_equal(zFactor::z.HallYarborough(ppr, tpr), hy)
})

