library(testthat)


# read the expected values table
rda_file <- "vlp_run_output.rda"
if (!file.exists(rda_file))
    stop("No test table.\nUse the table generator first\n")
load(rda_file)



context("runVLP() C13")
test_that("list saved matches actual list", {

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
        if.tens = 30)

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 11,
                              tol = 0.000001)

    result <- runVLP(well.input = well_input, well_model)
    expected <- output_c13

    expect_equal(dim(result), c(12,45))
    expect_equal(tail(result, 1)[["pres"]], 1043.1094, tolerance = 1e-7)
    expect_equal(tail(result, 1)[["z"]], 0.9000327, tolerance = 1e-7)
    expect_equal(result, expected)
})




context("runVLP() Guo_P44")
test_that("list saved matches actual list", {

    well_input <- setWellInput(
        field.name = "HAGBR.MOD",
        well.name = "Guo_P44",
        depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
        GLR = 362.7, liq.rt = 758, wcut = 0.1,
        thp = 100, tht = 80, bht = 180,
        API = 40, gas.sg = 0.70, wat.sg = 1.05,
        if.tens = 30)

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 29,
                              tol = 0.000001)

    result <- runVLP(well.input = well_input, well_model)
    expected <- output_ep44

    # print(tail(expected, 1)[["pres"]])
    # print(tail(result, 1)[["z"]])

    expect_equal(dim(result), c(30,45))
    expect_equal(tail(result, 1)[["pres"]], 1909.763, tolerance = 1e-5)
    expect_equal(tail(result, 1)[["z"]], 0.8529847, tolerance = 1e-7)
    expect_equal(result, expected)
})


context("runVLP() Guo_P44")
test_that("list saved matches actual list", {

        # Brown_C44
        well_input <- setWellInput(
            field.name = "HAGBR.MOD",
            well.name = "Brown_C44",
            depth.wh = 0, depth.bh = 3590,
            diam.in = 1.995,
            GLR = 1000, liq.rt = 600, wcut = 0.0,
            thp = 500, tht = 120, bht = 150,
            API = 42, oil.visc = 1.0,
            gas.sg = 0.65, wat.sg = 1.07, if.tens = 30
        )

        well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                                  segments = 15,
                                  tol = 0.00001)

        result <- runVLP(well.input = well_input, well_model)
        expected <- output_c44

        # print(tail(expected, 1)[["pres"]])
        # print(tail(result, 1)[["z"]])

        expect_equal(dim(result), c(16, 45))
        expect_equal(tail(result, 1)[["pres"]], 905.1789, tolerance = 1e-5)
        expect_equal(tail(result, 1)[["z"]], 0.9109475, tolerance = 1e-7)
        expect_equal(result, expected)
})


context("runVLP() Oilwell_Dry_01")
test_that("list saved matches actual list", {

        # Oilwell_Dry_01
        well_input <-  setWellInput(field.name = "HAGBR.MOD",
                                    well.name = "Oilwell_01_Dry",
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
                                    salinity = 23000
        )

        well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                                  segments = 29,
                                  tol = 0.00001)

        result <- runVLP(well.input = well_input, well_model)
        expected <- output_ow_dry

        # print(tail(expected, 1)[["pres"]])
        # print(tail(result, 1)[["z"]])

        expect_equal(dim(result), c(30, 45))
        expect_equal(tail(result, 1)[["pres"]], 769.3803, tolerance = 1e-5)
        expect_equal(tail(result, 1)[["z"]], 0.9235756, tolerance = 1e-7)
        expect_equal(result, expected)
})
