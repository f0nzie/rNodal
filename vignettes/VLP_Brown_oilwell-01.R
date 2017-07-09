## ----rows.print=30-------------------------------------------------------
library(rNodal)

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

runVLP(well.input = input_example, well_model)

