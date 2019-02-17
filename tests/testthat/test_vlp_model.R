library(testthat)

context("Test setVLPmodel(), case I")

test_that("model 1 match", {
    # Brown_C13
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

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 11,
                              tol = 0.000001,
                              well_input = well_input
                              )

    expect_equal(well_model$vlp.model, "hagbr.mod")
    expect_equal(well_model$segments, 11)
    expect_equal(well_model$tol, 0.000001)
})


context("Test setVLPmodel(), case II")
test_that("model 2 match", {
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

    well_model <- setVLPmodel(vlp.model = "hagbr.mod",
                              segments = 15,
                              tol = 0.00001,
                              well_input = well_input
                              )

    expect_equal(well_model$vlp.model, "hagbr.mod")
    expect_equal(well_model$segments, 15)
    expect_equal(well_model$tol, 0.00001)
})



# These tests may be too simplistic and unnecessary
#
# context("setWellInput() C13")
#
# test_that("setWellInput returns correct parameters", {
#     input_example <- setWellInput(
#         field.name = "HAGBR.MOD",
#         well.name = "Brown_C13",
#         depth.wh = 0, depth.bh = 2670,
#         diam.in = 1.995,
#         GLR = 500,
#         liq.rt = 1000,
#         wcut = 0.6,
#         thp = 500,
#         tht = 120,
#         bht = 150,
#         API = 22,
#         gas.sg = 0.65,
#         wat.sg = 1.07,
#         if.tens = 30)
#
#     expect_equal(input_example$GLR, 500)
#     expect_equal(input_example$liq.rt, 1000)
#     expect_equal(input_example$wcut, 0.6)
#     expect_equal(input_example$salinity, 0)
#     expect_equal(input_example$U, 8.0)
#     expect_equal(input_example$angle, pi/2)
#
# })
#

# context("setWellInput() EP4.4")
#
# test_that("setWellInput returns correct parameters", {
#     input_example_P44 <- setWellInput(
#         field.name = "HAGBR.MOD",
#         well.name = "Guo_P44",
#         depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
#         GLR = 362.7, liq.rt = 758, wcut = 0.1,
#         thp = 100, tht = 80, bht = 180,
#         API = 40, gas.sg = 0.70, wat.sg = 1.05,
#         if.tens = 30)
#
#     expect_equal(input_example_P44$well.name, "Guo_P44")
#     expect_equal(input_example_P44$GLR, 362.7)
#     expect_equal(input_example_P44$liq.rt, 758)
#     expect_equal(input_example_P44$wcut, 0.1)
#     expect_equal(input_example_P44$thp, 100)
#     expect_equal(input_example_P44$salinity, 0)
#     expect_equal(input_example_P44$U, 8.0)
#     expect_equal(input_example_P44$angle, pi/2)
#
# })
#
#
# context("setWellInput() C44")
#
# test_that("setWellInput returns correct parameters", {
#     input_example <- setWellInput(
#         field.name = "HAGBR.MOD",
#         well.name = "Brown_C44",
#         depth.wh = 0, depth.bh = 3590,
#         diam.in = 1.995,
#         GLR = 1000, liq.rt = 600, wcut = 0.0,
#         thp = 500, tht = 120, bht = 150,
#         API = 42, oil.visc = 1.0,
#         gas.sg = 0.65, wat.sg = 1.07, if.tens = 30
#     )
#
#     expect_equal(input_example$well.name, "Brown_C44")
#     expect_equal(input_example$depth.bh, 3590)
#     expect_equal(input_example$GLR, 1000)
#     expect_equal(input_example$liq.rt, 600)
#     expect_equal(input_example$wcut, 0.0)
#     expect_equal(input_example$thp, 500)
#     expect_equal(input_example$salinity, 0)
#     expect_equal(input_example$U, 8.0)
#     expect_equal(input_example$angle, pi/2)
#
# })
#
#
# context("setWellInput() Oilwell_01_Dry")
#
# test_that("setWellInput returns correct parameters", {
#     input_example <-  setWellInput(field.name = "HAGBR.MOD",
#                                    well.name = "Oilwell_01_Dry",
#                                    depth.wh = 0,
#                                    depth.bh = 9275,
#                                    diam.in = 4.052,
#                                    GLR = 800, liq.rt = 983, wcut = 0.0,
#                                    thp = 100,
#                                    tht = 60,
#                                    bht = 210,
#                                    API = 37,
#                                    oil.visc = 5.0,
#                                    gas.sg = 0.76,
#                                    wat.sg = 1.07,
#                                    if.tens = 30,
#                                    salinity = 23000
#     )
#
#     expect_equal(input_example$well.name, "Oilwell_01_Dry")
#     expect_equal(input_example$depth.bh, 9275)
#     expect_equal(input_example$diam.in, 4.052)
#     expect_equal(input_example$GLR, 800)
#     expect_equal(input_example$liq.rt, 983)
#     expect_equal(input_example$wcut, 0.0)
#     expect_equal(input_example$thp, 100)
#     expect_equal(input_example$bht, 210)
#     expect_equal(input_example$API, 37)
#     expect_equal(input_example$salinity, 23000)
#     expect_equal(input_example$U, 8.0)
#     expect_equal(input_example$angle, pi/2)
#
# })
