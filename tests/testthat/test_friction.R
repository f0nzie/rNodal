library(testthat)

context("friction factor by Chen")


test_that("Re <= 2000", {
    # laminar flow
    result <- ff.chen(ed = 0.001, Re = 1500)
    expect_equal(result, 0.0136519)
})

test_that("Re <= 5000", {
    result <- ff.chen(ed = 0.001, Re = 5000)
    expect_equal(result, 0.009614769)
})


test_that("R > 5000", {
    result <- ff.chen(ed = 0.001, Re = 9000)
    expect_equal(result, 0.008305504)
})


test_that("R > 10000", {
    result <- ff.chen(ed = 0.001, Re = 12000)
    expect_equal(result, 0.007780164)
})


test_that("R > 10000, ed = 0.0005", {
    result <- ff.chen(ed = 0.0005, Re = 12000)
    expect_equal(result, 0.007574594)
})



context("friction factor by Moody")

test_that("Re <= 2000", {
    # laminar flow
    result <- moody.ff(ed = 0.001, Re = 1500)
    expect_equal(result, 0.04266667)
})

test_that("Re <= 5000", {
    result <- moody.ff(ed = 0.001, Re = 5000)
    expect_equal(result, 0.03850763)

})

test_that("R > 5000", {
    result <- moody.ff(ed = 0.001, Re = 9000)
    expect_equal(result, 0.03319837 )

})

test_that("R > 10000", {
    result <- moody.ff(ed = 0.001, Re = 12000)
    expect_equal(result, 0.03107582 )

})


test_that("R > 10000, ed = 0.0005", {
    result <- moody.ff(ed = 0.0005, Re = 12000)
    expect_equal(result, 0.03028062)
})




context("friction factor by Colebrooke")

test_that("Re <= 2000", {
    # laminar flow
    result <- friction.factor(ED = 0.001, REY = 1500)$FF
    expect_equal(result, 0.04266667)
})

test_that("Re <= 5000", {
    result <- friction.factor(ED = 0.001, REY = 5000)$FF
    expect_equal(result, 0.03849744)

})

test_that("R > 5000", {
    result <- friction.factor(ED = 0.001, REY = 9000)$FF
    expect_equal(result, 0.03318801 )

})

test_that("R > 10000", {
    result <- friction.factor(ED = 0.001, REY = 12000)$FF
    expect_equal(result, .03106532 )

})

test_that("R > 10000, ed = 0.0005", {
    result <- friction.factor(ED = 0.0005, REY = 12000)$FF
    expect_equal(result, 0.03026978)
})
