# source("zfactor.R")

#' @include zfactor.R
NULL

#' Correlation for gas base viscosity
#' at atmospheric conditions. Dempsey (1965)
#' Guo, pg. 2/23 Eq 2.36
#' @param temp temperature deg F
#' @param sgg specific gravity of gas
gas.viscBase <- function(temp, sgg) {
  8.188 / 1000 - 6.15 / 1000 * log10(sgg) +
  (1.709 / 100000 - 2.062 / 1000000 * sgg) * temp

}


#' Reduced viscosity of gas
#' Dempsey correlation to find the viscosity of gas
#' Guo, pg 2/23
#' @param pres.pr pseudo reduced pressure
#' @param temp.pr pseudo reduced temperature
mu.r <- function(pres.pr, temp.pr) {
  a0 =  -2.46211820
  a1 =   2.97054714
  a2 =  -0.28626405
  a3 =   0.00805420
  a4 =   2.80860949
  a5 =  -3.49803305
  a6 =   0.36037302
  a7 =  -0.01044324
  a8 =  -0.79338568
  a9 =   1.39643306
  a10 = -0.14914493
  a11 =  0.00441016
  a12 =  0.08393872
  a13 = -0.18640885
  a14 =  0.02033679
  a15 = -0.00060958

  out <- (a0 + a1 * pres.pr + a2 * pres.pr^2 + a3 * pres.pr^3 +
    temp.pr   * (a4 + a5 * pres.pr + a6 * pres.pr^2 + a7 * pres.pr^3) +
    temp.pr^2 * (a8 + a9 * pres.pr + a10 * pres.pr^2 + a11 * pres.pr^3) +
    temp.pr^3 * (a12 + a13 * pres.pr + a14 * pres.pr^2 + a15 * pres.pr^3) )
  return(out)
}


#' Calculate gas viscosity
#' Guo, pg 2/23
#' @param pres pressure
#' @param temp temperature
#' @param gas.sg gas specific gavity
#' @export
mu.gas <- function(pres, temp, gas.sg) {
  # gas base viscosity
  visc.base <- gas.viscBase(temp, gas.sg)

  crit <- calcCriticals(pres, temp, gas.sg)
  pres.pr <- crit$pres.pr
  temp.pr <- crit$temp.pr

  visc.r <- mu.r(pres.pr, temp.pr)         # Dempsey relation for viscosity

  visc.gas <- visc.base / temp.pr * exp(visc.r)

  # return a list with visc gas and visc reduced Dempsey
  list(pres.pr       = pres.pr,
       temp.pr       = temp.pr,
       gas.visc.r    = visc.r,
       gas.visc.base = visc.base,
       gas.visc      = visc.gas
       )
}




