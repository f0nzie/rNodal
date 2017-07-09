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


well.model <- setVLPmodel(vlp.model = "hagbr.mod", segments = 29, tol = 0.000001)

df <- runVLP(well.input = input.example.C13, well.model)
df

