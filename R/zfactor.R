#' @param pres.a absolute pressure, psia
#' @param temp.f temperature, deg F
#' @param gas.sg gas specific gravity
#' @param n2.frac N2
#' @param co2.frac CO2
#' @param h2s.frac H2S
#' @rdname Z
#' @name zcorr
NULL

#' Calculate Z factors. Generic function
#'
#' @param correlation a string or number that identifies the correlation. It could the
#'     values of "HY" for "Hall-Yarborough or "BB" for Brill-Beggs. TIt can also
#'     take numeric values such as 1 or 2.
#' @param ... any additional parameter
#' @inheritParams zcorr
#' @family Z factor correlations
#' @export
Z <- function(correlation, pres.a, temp.f, gas.sg,
              n2.frac = 0, co2.frac = 0, h2s.frac = 0, ...) {

    if (correlation == "HY" || correlation == 1) {
        arggs <- c(as.list(environment()), list(...))
        arggs$correlation <- NULL
        return(do.call(z.hallyarborough, arggs)$z)
    }
    if (correlation == "BB" || correlation == 2) {
        arggs <- c(as.list(environment()), list(...))
        arggs$correlation <- NULL
        z <- do.call(z.brillbeggs, arggs)
        return(z)
    }
}



#' Calculate the Z factor using the Hall-Yarborough method
#'
#' @inheritParams zcorr
#' @family Z factor correlations
#' @section Gas correlations
#' @export
z.hallyarborough <-function(pres.a, temp.f, gas.sg,
                            n2.frac = 0, co2.frac = 0, h2s.frac = 0) {
    # pres.a = absolute pressure, psia; temp.f  = temperature, deg F

    # calculate pseudo-critical pressure and temperature
    # get pseudo-reduced
    crit <- calcCriticals(pres.a, temp.f, gas.sg, co2.frac, h2s.frac, n2.frac)
    pres.pr <- crit$pres.pr
    temp.pr <- crit$temp.pr
    temp.r  <- crit$temp.r

    zFactor::z.HallYarborough(pres.pr = pres.pr, temp.pr = temp.pr)
}


old.z.hallyarborough <-function(pres.a, temp.f, gas.sg,
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
  temp.r  <- crit$temp.r

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
#'
#' Returns a list of calculated pseudo-reduced and pseudo-critical
#' pressure and temperature
#' @return list
#' @inheritParams zcorr
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


#' Calculate the Z factor with the Brill-Beggs correlation
#'
#' @inheritParams zcorr
#' @family Z factor correlations
#' @section Gas correlations
#' @export
z.brillbeggs <- function(pres.a, temp.f, gas.sg,
                         n2.frac = 0, co2.frac = 0, h2s.frac = 0) {

    .z <- .z.brillbeggs(pres.a, temp.f, gas.sg,
                              n2.frac, co2.frac, h2s.frac)
    return(.z$z)
}


.z.brillbeggs <- function(pres.a, temp.f, gas.sg,
                         n2.frac = 0, co2.frac = 0, h2s.frac = 0) {
    # Brill and Beggs compressibility factor (1973)

    pres.pc <- 678 - 50*(gas.sg - 0.5) - 206.7 * n2.frac + 440 * co2.frac +
        606.7 * h2s.frac
    temp.pc <- 326+315.7 * (gas.sg - 0.5) - 240 * n2.frac - 83.3 * co2.frac +
        133.3 * h2s.frac
    pres.pr <- pres.a / pres.pc

    # worksheet has a bug in the Farenheit add formula in the book
    temp.pr <- (temp.f + 460) / temp.pc

    A <- 1.39 *(temp.pr - 0.92)^0.5 - 0.36 * temp.pr - 0.101
    E <- 9 * (temp.pr - 1)
    F <- 0.3106 - 0.49 * temp.pr + 0.1824 * temp.pr^2
    B <- (0.62 - 0.23 * temp.pr) * pres.pr +
        (0.066 / (temp.pr - 0.86) - 0.037) * pres.pr^2 +
        0.32 * pres.pr^2 / 10^E
    C <- 0.132 - 0.32 * log10(temp.pr)
    # D <- 10^(0.3106 - 0.49 * temp.pr+0.1824*temp.pr^2)
    D <- 10^F

    z <- A + (1 - A) / exp(B) + C * pres.pr^D

    return(named.list(pres.pc, temp.pc, pres.pr, temp.pr, A, B, C, D, z))
}
