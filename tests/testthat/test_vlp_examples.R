library(testthat)

context("test example C44")



test_that("VLP C44 output is the same as check rda file", {
    # Example from C.44 in Brown's book
    # P2 (pressure at end point is given in the original example).
    # The original question is: what is the length of the tubing.
    #
    # In our case we aproximately know the length (from the book), so we will try to
    # match P2 (known)
    #
    # Differences with Example C.13:
    # GLR, watercut, oil viscosity and API are different in C.44
    #
    # The final results are very close to those of Brown.
    input_example <- setWellInput(field.name = "HAGBR.MOD",
                                  well.name = "Brown_C44",
                                  depth.wh = 0, depth.bh = 3590,
                                  diam.in = 1.995,
                                  GLR = 1000, liq.rt = 600, wcut = 0.0,
                                  thp = 500, tht = 120, bht = 150,
                                  API = 42, oil.visc = 1.0,
                                  gas.sg = 0.65, wat.sg = 1.07, if.tens = 30
    )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.00001)
    result <- runVLP(well.input = input_example, well_model)

    rda_file <- "brown_c44.rda"
    ds_name <- tools::file_path_sans_ext(rda_file)
    if (!file.exists(rda_file)) {
        assign(ds_name, result)
        save(list = ds_name, file = rda_file)
    }
    load(file = rda_file)
    expect_equal(result, get(ds_name))
    # print(get(ds_name))
})


context("test example C13")
test_that("VLP C13 output is the same as check rda file", {
    # Example from C.13 in Brown
    # P2 (pressure at end point is given).
    # The question is: what is the length of the tubing.
    # P2 = 1000 psia

    input.example.C13 <- setWellInput(field.name = "HAGBR.MOD",
                                      well.name = "Brown_C13",
                                      depth.wh = 0, depth.bh = 2670, diam.in = 1.995,
                                      GLR = 500, liq.rt = 1000, wcut = 0.6,
                                      thp = 500, tht = 120, bht = 150,
                                      API = 22, gas.sg = 0.65, wat.sg = 1.07, if.tens = 30)


    well.model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.000001)
    result <- runVLP(well.input = input.example.C13, well.model)

    rda_file <- "brown_c13.rda"
    ds_name <- tools::file_path_sans_ext(rda_file)
    if (!file.exists(rda_file)) {
        assign(ds_name, result)
        save(list = ds_name, file = rda_file)
    }
    load(file = rda_file)
    expect_equal(result, get(ds_name))

})

context("test example oilfield_01")
test_that("VLP Oilwell_01 output is the same as check rda file", {

    # Example from Prosper oil well 01
    input_example <-  setWellInput(field.name = "HAGBR.MOD",
                                   well.name = "Oilwell_01",
                                   depth.wh = 0, depth.bh = 9275,
                                   diam.in = 4.052,
                                   GLR = 800, liq.rt = 700, wcut = 0.0,
                                   thp = 100, tht = 60, bht = 210,
                                   API = 37, oil.visc = 1.0,
                                   gas.sg = 0.76, wat.sg = 1.07, if.tens = 30,
                                   salinity = 23000
    )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.00001)
    result <- runVLP(well.input = input_example, well_model)

    rda_file <- "brown_oilwell01.rda"
    ds_name <- tools::file_path_sans_ext(rda_file)
    if (!file.exists(rda_file)) {
        assign(ds_name, result)
        save(list = ds_name, file = rda_file)
    }
    load(file = rda_file)
    expect_equal(result, get(ds_name))

})
