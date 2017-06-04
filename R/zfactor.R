#' Calculate Z factor using Hall-Yarborough method
#' @param pres.a absolute pressure, psia
#' @param temp.f temperature, deg F
#' @param gas.sg gas specific gravity
#' @param n2.frac N2
#' @param co2.frac CO2
#' @param h2s.frac H2S
#' @rdname zfactor-functions
#' @export
z.hallyarborough <-function(pres.a, temp.f, gas.sg,
                            n2.frac = 0, co2.frac = 0, h2s.frac = 0) {
  # pres.a = absolute pressure, psia
  # temp.f  = temperature, deg F
  funcY <- function(y) {
    # implicit equation
    (y + y^2 + y^3 - y^4) / (1 - y)^3 - A * pres.pr - B * y^2 + C * y^D
  }
  # calculate pseudo-critical pressure and temperature
  # get pseudo-reduced
  crit <- calcCriticals(pres.a, temp.f, gas.sg, co2.frac, h2s.frac, n2.frac)
  pres.pr <- crit$pres.pr
  temp.pr <- crit$temp.pr
  temp.r <- crit$temp.r

  A <- 0.06125 * temp.r * exp(-1.2 * (1 - temp.r)^2)
  B <- temp.r * (14.76 - 9.76 * temp.r + 4.58 * temp.r^2)
  C <- temp.r * (90.7 - 242.2 * temp.r + 42.4 * temp.r^2)
  D <- 2.18 + 2.82 * temp.r

  All <- rootSolve::uniroot.all(funcY, c(0, 10))   # find the root of the equation
  Y <- min(All)                         # minimum value
  z <- A * pres.pr / Y                  # calculate z
  zfactors <- list(z = z,
                   Y = Y, A = A, B = B, C = C, D = D,
                   pres.pr = pres.pr, temp.pr = temp.pr)
  return(zfactors)
}



#' Calculate the pressure and temperature criticals
#' Returns list
#' @inheritParams z.hallyarborough
#' @rdname zfactor-functions
calcCriticals <- function(pres.a, temp.f, gas.sg,
                          co2.frac = 0, h2s.frac = 0, n2.frac = 0) {
  if (h2s.frac < 0.03 & n2.frac < 0.05) {
    # calculate pseudo-criticals.
    # Guo pg 2/22
    # cat("\nsmall H2S and N2\n")
    # Valid for H2S < 3%; N2 < 5% and inorganic < 7%
    pres.pc <- 709.604 - 58.718 * gas.sg      # Eq 2.22
    temp.pc <- 170.491 + 307.344 * gas.sg     # Eq 2.23
  } else {
    # Pseudo-criticals for impurity corrections
    # Ahmed, Guo pg 2/23
    pres.pc <- 678 - 50 * (gas.sg - 0.5) - 206.7 * n2.frac +
      440 * co2.frac + 606.7 * h2s.frac
    temp.pc <- 326 + 315.7 * (gas.sg - 0.5) - 240 * n2.frac -
      83.3 * co2.frac + 133.3 * h2s.frac
  }
  # calculate pseudo-reduced
  pres.pr <- pres.a / pres.pc
  temp.pr <- (temp.f + 460) / temp.pc  # worksheet has bug in the Farenheit add
  temp.r <- 1 / temp.pr  # wrong division in worksheet cell c15

  criticals <- list(pres.pr = pres.pr,
                    temp.pr = temp.pr,
                    temp.r = temp.r,
                    pres.pc = pres.pc,
                    temp.pc = temp.pc)
  return(criticals)
}

#' @param pabs absolute pressure, psia
#' @param tempFar temperature degF
#' @inheritParams z.hallyarborough
#' @rdname zfactor-functions
z.brillbeggs <- function(pabs, tempFar, gas.sg,
                         n2.frac, co2.frac, h2s.frac) {

  pres.pc <- 678 - 50*(gas.sg - 0.5) - 206.7 * n2.frac + 440 * co2.frac + 606.7 * h2s.frac
  temp.pc <- 326+315.7 * (gas.sg - 0.5) - 240 * n2.frac - 83.3 * co2.frac + 133.3 * h2s.frac
  pres.pr <- pabs / pres.pc
  temp.pr <- (tempFar + 460) / temp.pc       # worksheet has a bug in the Farenheit add

  A <- 1.39 *(temp.pr - 0.92)^0.5 - 0.36 * temp.pr - 0.101
  B <- (0.62 - 0.23 * temp.pr) * pres.pr +
    (0.066 / (temp.pr - 0.86) - 0.037) * pres.pr^2 +
    0.32/10^(9 *(temp.pr - 1))*pres.pr^6
  C <- 0.132 - 0.32 * log10(temp.pr)
  D <- 10^(0.3106-0.49*temp.pr+0.1824*temp.pr^2)
  z <- A + (1 - A) / exp(B) + C * pres.pr^D
}

