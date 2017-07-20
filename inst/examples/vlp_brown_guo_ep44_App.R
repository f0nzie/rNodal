library(rNodal)
library(tibble)

# Example from Prosper oil well 01. Dry version

.vlp_brown_guo_ep44_App <- function(...) {

    # Example Problem from Guo book
    input.example.P44 <- setWellInput(field.name = "HAGBR.MOD",
                                      well.name = "Guo_P44",
                                      depth.wh = 0, depth.bh = 9700, diam.in = 1.995,
                                      GLR = 362.7, liq.rt = 758, wcut = 0.1,
                                      thp = 100, tht = 80, bht = 180,
                                      API = 40, gas.sg = 0.70, wat.sg = 1.05, if.tens = 30)


    well.model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.000001)
    as.tibble(runVLP(well.input = input.example.P44, well.model))
}


vlp_brown_guo_ep44_App <- function(vars, verbose = FALSE) {
    .vlp_brown_guo_ep44_App(verbose)[, vars]
}

# print(vlp_brown_guo_ep44_App(vars = c("depth")))
