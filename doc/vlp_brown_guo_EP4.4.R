## ----setup, include=F, error=T, message=F, warning=F---------------------
knitr::opts_chunk$set(echo=T, comment=NA, error=T, warning=F, message = F, fig.align = 'center')

## ----rows.print=30-------------------------------------------------------
library(rNodal)
library(tibble)

# Example Problem from Guo book

input.example.P44 <- setWellInput(field.name = "HAGBR.MOD",
                                    well.name = "Guo_P44", 
                                    depth.wh = 0, depth.bh = 9700, diam.in = 1.995, 
                                    GLR = 362.7, liq.rt = 758, wcut = 0.1, 
                                    thp = 100, tht = 80, bht = 180, 
                                    API = 40, gas.sg = 0.70, wat.sg = 1.05, if.tens = 30)


well.model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.000001)

as.tibble(runVLP(well.input = input.example.P44, well.model))

## ----rows.print=30-------------------------------------------------------
# Example Problem from Guo book
library(rNodal)
# get only the variable we need for heat transfer. But what we really want 
# is the deviation survey: MD, TVD
well_table <- runVLP(well.input = input.example.P44,
                     well.model)[, c("depth", "dL", "pres", "temp")]    

input.example.guo.44 <- setWellInput(field.name = "HAGBR.MOD",
                                    well.name = "Guo_P44", 
                                    depth.wh = 0, depth.bh = 9700, diam.in = 1.995, 
                                    GLR = 362.7, liq.rt = 758, wcut = 0.1, 
                                    thp = 100, tht = 80, bht = 180, 
                                    API = 40, gas.sg = 0.70, wat.sg = 1.05, if.tens = 30,
                                    U = 3)   # decreasing the U


well_parameters <- get_well_parameters(input.example.guo.44)

# temp.gradient calculates the fluid temperature coming from the wellbore
rNodal:::temp.gradient(well_table, well_parameters)  

