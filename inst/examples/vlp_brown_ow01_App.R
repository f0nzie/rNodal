library(rNodal)
library(tibble)

# Example from Prosper oil well 01. Dry version

vlp_brown_ow01_App <- function(vars, verbose = FALSE) {

    input_example <-  setWellInput(field.name = "HAGBR.MOD",
                                   well.name = "Oilwell_01_Dry",
                                   depth.wh = 0, depth.bh = 9275,
                                   diam.in = 4.052,
                                   GLR = 800, liq.rt = 983, wcut = 0.0,
                                   thp = 100, tht = 60, bht = 210,
                                   API = 37, oil.visc = 5.0,
                                   gas.sg = 0.76, wat.sg = 1.07, if.tens = 30,
                                   salinity = 23000
    )

    well_model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.00001)
    as.tibble(runVLP(well.input = input_example, well_model))[, vars]
}

