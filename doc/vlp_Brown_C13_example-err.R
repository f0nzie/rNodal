## ----setup, include=F, error=T, message=F, warning=F---------------------
knitr::opts_chunk$set(echo=T, comment=NA, error=T, warning=F, message = F, fig.align = 'center')

## ----rows.print=30-------------------------------------------------------
library(rNodal)

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


well.model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 11, tol = 0.000001)

runVLP(well.input = input.example.C13, well.model)

## ----rows.print=30-------------------------------------------------------
# calculate the fluid temperature in the well
# input: deviation survey and well calculated parameters: uses new functions: 
#        get_well_parameters
#        rNodal:::temp.gradient

library(rNodal)

# get only the variable we need for heat transfer. But what we really want 
# is the deviation survey: MD, TVD
well_table <- runVLP(well.input = input.example.C13, 
                     well.model)[, c("depth", "dL", "pres", "temp")]    

input.example.C13 <- setWellInput(field.name = "HAGBR.MOD",
                                    well.name = "Brown_C13", 
                                    depth.wh = 0, depth.bh = 2670, diam.in = 1.995, 
                                    GLR = 500, liq.rt = 1000, wcut = 0.6, 
                                    thp = 500, tht = 120, bht = 150, 
                                    API = 22, gas.sg = 0.65, wat.sg = 1.07,
                                  U = 17)

well_parameters <- get_well_parameters(input.example.C13)

# temp.gradient calculates the fluid temperature coming from the wellbore
rNodal:::temp.gradient(well_table, well_parameters)         


## ----rows.print=30-------------------------------------------------------
# this tests if new function get_well_parameters() returns all what's needed for heat transfer
library(rNodal)

well_table <- runVLP(well.input = input.example.C13, 
                     well.model)[, c("depth", "dL", "pres", "temp")]   

input.example.C13 <- setWellInput(field.name = "HAGBR.MOD",
                                    well.name = "Brown_C13", 
                                    depth.wh = 0, depth.bh = 2670, diam.in = 1.995, 
                                    GLR = 500, liq.rt = 1000, wcut = 0.6, 
                                    thp = 500, tht = 120, bht = 150, 
                                    API = 22, gas.sg = 0.65, wat.sg = 1.07,
                                  U = 17)
# input.example.C13
# getBasicCalcs(input.example.C13)
well_params <- get_well_parameters(input.example.C13)
Hmisc::list.tree(well_params, maxcomp = 40)

# temp.gradient calculates the fluid temperature coming from the wellbore
rNodal:::temp.gradient(well_table, well_parameters)    

## ----rows.print=30-------------------------------------------------------
# this in an old version where all well parameters had to be spelled out
# parameters necessary to calculate the fluid temperature
well_table <- runVLP(well.input = input.example.C13, 
                     well.model)[, c("depth", "dL", "pres", "temp")] 

theta   <-  pi /2
diam.in <- 1.995
diam.ft <- diam.in / 12
tht     <- 120
bht     <- 150
depth   <- 2670
ge      <- (bht - tht) / depth
mass.rate <- 228145
U <-  17
# U <- 4
cp.avg <- (0.53 + 0.5 + 1 ) / 3

# calculate dT/dx for the well
rNodal:::temp.fluid(well_table, theta, depth, bht, tht, U, cp.avg, diam.ft, mass.rate)
# we don't want all parameters spelled out      ^   ^      ^     ^      ^      ^     

## ------------------------------------------------------------------------
library(rNodal)

well_as_text <- "
MD      TVD    
0	    0   
242.7	242.7
485.5	485.5
728.2	728.2
970.9	970.9
1213.6	1213.6
1456.4	1456.4
1699.1	1699.1
1941.8	1941.8
2184.5	2184.5
2427.3	2427.3
2670.0	2670.0
"

deviation_survey <- set_deviation_survey(well_as_text)

## ------------------------------------------------------------------------
# rNodal:::calc_angle_deviation_survey(deviation_survey)

