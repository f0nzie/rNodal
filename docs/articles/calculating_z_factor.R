## ------------------------------------------------------------------------
# inputs
pres   <- 200  # psia
temp   <- 180  # deg F
gas.sg <- 0.65
n2     <- 0.1
co2    <- 0.08
h2s    <- 0.02

## ------------------------------------------------------------------------
library(rNodal)

# calculating without considering gas impurities
z.hallyarborough(pres, temp, gas.sg) # output is a list

## ------------------------------------------------------------------------
z.hallyarborough(pres, temp, gas.sg)

## ------------------------------------------------------------------------
Z(correlation = "HY", pres.a = 5000, temp.f = 180, gas.sg = 0.65, 
  n2.frac = 0.1, co2.frac = 0.08, h2s.frac = 0.02)

## ------------------------------------------------------------------------
Z(correlation = 1, pres.a = pres, temp.f = temp, gas.sg)

## ------------------------------------------------------------------------
z.brillbeggs(pres, temp, gas.sg)

## ------------------------------------------------------------------------
Z(correlation = "BB", pres, temp, gas.sg)

## ------------------------------------------------------------------------
Z(correlation = "2", pres.a = pres, temp.f = temp, gas.sg)

## ------------------------------------------------------------------------
Z(correlation = "2", pres.a = 5000, temp.f = 180, gas.sg = 0.65, 
  n2.frac = 0.1, co2.frac = 0.08, h2s.frac = 0.02)

## ------------------------------------------------------------------------
# check temp.pr
# worksheet has a bug in the Farenheit add formula in the book
rNodal:::.z.brillbeggs(5000, 180, 0.65, 
                       n2.frac = 0.1, co2.frac = 0.08, h2s.frac = 0.02)

## ------------------------------------------------------------------------
# check temp.pr
# worksheet has a bug in the Farenheit add formula in the book
rNodal:::.z.brillbeggs(5000, 180, 0.65, 
                       n2.frac = 0.1, co2.frac = 0.08, h2s.frac = 0.02)

## ------------------------------------------------------------------------
# calculating without considering gas impurities
# data from paper pg 490
z.hallyarborough(2000, 150, 0.7) # output is a list

## ------------------------------------------------------------------------
Z(correlation = "BB", 2000, 150, 0.7)

