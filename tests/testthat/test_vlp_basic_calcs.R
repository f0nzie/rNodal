library(testthat)


rda_file <- "basic_calcs.rda"
if (!file.exists(rda_file))
    stop("No test table.\nUse the table generator first\n")
load(rda_file)



context("getBasicCalcs() C13")

test_that("list saved matches actual list", {

    geothermal_2p_txt <- c("
      TVD    temp
      0      120
      2670   150
    ")

    deviation_survey_2p_txt <- "
       TVD    MD
         0     0
      2670  2670
    "

    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Brown_C13",
        depth.wh = 0, depth.bh = 2670,
        diam.in = 1.995,
        GLR = 500,
        liq.rt = 1000,
        wcut = 0.6,
        thp = 500,
        tht = 120,
        bht = 150,
        API = 22,
        gas.sg = 0.65,
        wat.sg = 1.07,
        if.tens = 30,
        geotherm = geothermal_2p_txt,
        dev_survey = deviation_survey_2p_txt
        )

    result_list <- getBasicCalcs(well_input)
    expected_list <- basic_calcs_c13         # read from RDA file

    g <- function(x) {
        # compare two lists
        # cat("\t", x, basic_calcs[[x]], result_list[[x]], "\n")
        expect_equal(result_list[[x]], expected_list[[x]])
    }

    invisible(
        sapply(names(expected_list), function(x) {
            g(x)
        })
    )
})



context("getBasicCalcs() p44")

test_that("list saved matches actual list", {

    geothermal_2p_txt <- c("
    TVD    temp
    0       80
    9700   180
    ")

    deviation_survey_2p_txt <- "
       TVD    MD
         0     0
      9700  9700
    "

    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Guo_P44",
        depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
        GLR = 362.7, liq.rt = 758, wcut = 0.1,
        thp = 100, tht = 80, bht = 180,
        API = 40, gas.sg = 0.70, wat.sg = 1.05,
        if.tens = 30,
        U = 4,
        geotherm = geothermal_2p_txt,
        dev_survey = deviation_survey_2p_txt
        )

    result_list <- getBasicCalcs(well_input)
    expected_list <- basic_calcs_ep44

    g <- function(x) {
        # compare two lists
        expect_equal(result_list[[x]], expected_list[[x]])
    }

    invisible(
        sapply(names(expected_list), function(x) {
            g(x)
        })
    )
})



context("getBasicCalcs() C44")

test_that("list saved matches actual list", {

    geothermal_2p_txt <- "
    TVD   temp
    0     120
    3590  150
    "
    deviation_survey_2p_txt <- "
    TVD   MD
    0     0
    3590  3590
    "
    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Brown_C44",
        depth.wh = 0, depth.bh = 3590,
        diam.in = 1.995,
        GLR = 1000, liq.rt = 600, wcut = 0.0,
        thp = 500, tht = 120, bht = 150,
        API = 42, oil.visc = 1.0,
        gas.sg = 0.65, wat.sg = 1.07, if.tens = 30,
        U = 8,
        geotherm = geothermal_2p_txt,
        dev_survey = deviation_survey_2p_txt
    )

    result_list <- getBasicCalcs(well_input)
    expected_list <- basic_calcs_c44

    g <- function(x) {
        # compare two lists
        expect_equal(result_list[[x]], expected_list[[x]])
    }

    invisible(
        sapply(names(expected_list), function(x) {
            g(x)
        })
    )
})



context("getBasicCalcs() T01_Oil_Well")

test_that("list saved matches actual list", {

    geothermal_3p_txt <- c("
      TVD    temp
      0      60
      600    40
      9000   210
    ")
    deviation_survey_6p_txt <- "
      MD      TVD
      0	     0
      600	  600
      1005	 1000
      4075	 4000
      7700	 7500
      9275	 9000
    "
    well_input <-  setWellInput(field.name = "HAGBR.MOD",
                                well.name = "T01_Oil_Well",
                                depth.wh = 0,
                                depth.bh = 9275,
                                diam.in = 4.052,
                                GLR = 800, liq.rt = 983, wcut = 0.0,
                                thp = 100,
                                tht = 60,
                                bht = 210,
                                API = 37,
                                oil.visc = 5.0,
                                gas.sg = 0.76,
                                wat.sg = 1.07,
                                if.tens = 30,
                                salinity = 23000,
                                U = 8,
                                dev_survey = deviation_survey_6p_txt,
                                geotherm = geothermal_3p_txt
    )

    result_list <- getBasicCalcs(well_input)
    expected_list <- basic_calcs_t01_ow

    g <- function(x) {
        # compare two lists
        # cat("\t", x, result_list[[x]], expected_list[[x]], "\n")
        expect_equal(result_list[[x]], expected_list[[x]])
    }

    invisible(
        sapply(names(expected_list), function(x) {
            g(x)
        })
    )
})
