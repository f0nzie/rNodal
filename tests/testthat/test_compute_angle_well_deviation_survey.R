library(testthat)

context("test compute angle WDS")


test_that("WDS first argument is a dataframe", {
    wds <- "
        MD	TVD
        0	    0
        600	    600
        1005	1000
        4075	4000
        7700	7500
        9275	9000
    "
    wds_df <- read.table(header = TRUE, text = wds)
    expect_true(is.data.frame(compute_angle_deviation_survey(wds_df)))
})


test_that("WDS dataframe has at least two columns", {
    wds <- "
        MD
        0
        600
        1005
        4075
        7700
        9275
    "
    wds_df = read.table(header = TRUE, text = wds)
    expect_error(compute_angle_deviation_survey(wds_df),
                 "WDS should have at least two columns")
})


test_that("WDS first argument is not a dataframe but text", {
    wds <- "
    MD	TVD
    0	    0
    600	    600
    1005	1000
    4075	4000
    7700	7500
    9275	9000
    "
    expect_error(compute_angle_deviation_survey(wds), "WDS must be a dataframe*")
})

test_that("WDS columns are MD and TVD", {
    wds <- "
    MD1	TVD
    0	    0
    600	    600
    1005	1000
    4075	4000
    7700	7500
    9275	9000
    "
    wds_df = read.table(header = TRUE, text = wds)
    expect_error(compute_angle_deviation_survey(wds_df))


    wds <- "
    MD1	TVD2
    0	    0
    600	    600
    1005	1000
    4075	4000
    7700	7500
    9275	9000
    "
    wds_df = read.table(header = TRUE, text = wds)
    expect_error(compute_angle_deviation_survey(wds_df))

    wds <- "
    MD	TVD2
    0	    0
    600	    600
    1005	1000
    4075	4000
    7700	7500
    9275	9000
    "
    wds_df = read.table(header = TRUE, text = wds)
    expect_error(compute_angle_deviation_survey(wds_df))
})

test_that("angles in degrees match horizontal reference", {
    wds <- "
            MD	TVD
            0	    0
            600	    600
            1005	1000
            4075	4000
            7700	7500
            9275	9000
        "
    wds_df <- read.table(header = TRUE, text = wds)

    expected <-  c(0.00000, 90.00000, 80.98755, 77.74125, 74.90981, 72.24721)
    result <- compute_angle_deviation_survey(wds_df, reference = "horiz")[, "degrees"]
    expect_equal(expected, result, tolerance = 1e-6)
})



test_that("angles in degrees match vertical reference", {
    wds <- "
    MD	TVD
    0	    0
    600	    600
    1005	1000
    4075	4000
    7700	7500
    9275	9000
    "
    wds_df <- read.table(header = TRUE, text = wds)

    expected <- c(0.000000, 0.000000, 9.012451, 12.258749, 15.090185, 17.752790)
    result <- compute_angle_deviation_survey(wds_df, reference = "vertical")[, "degrees"]
    # print(result)
    expect_equal(expected, result, tolerance = 1e-6)
})
