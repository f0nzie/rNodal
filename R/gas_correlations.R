
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







gas.visc.lee <- function(pressure = 120, temperature = 60, molweight = 19){

    p <- pressure
    t <- temperature
    MW <- molweight

    TT <- 460 + t
    sg <- MW / 28.9
    Z <- 1 / (1 + (p * 344400 * 10^1.78 * sg / TT^3.82)) # compressibility
    cat("z=", Z)
    rho <- p * MW / (10.7316 * TT * Z) * 0.0160185       # density

    K <- 0.0001*(7.77+0.0063 * MW) * TT^1.5 / (122.4+12.9*MW + TT)
    X <- 2.57+1914.5 / TT +0.0095*MW
    Y <- 1.11+0.04*X
    mu <- K * exp(X * rho^Y)
    return(mu)
}

gas.z.stkatz <- function() {


}

gas.z.lee <- function() {

}

gas.z.dranchuk <- function() {
    # Calculates compressibility factor for natural gas based on
    # Dranchuk and Abou-Kassem equation of state (DAK - EOS).

}



gas.visc.lee <- function(pressure = 120, temperature = 60, molweight = 19){

    p <- pressure
    t <- temperature
    MW <- molweight

    TT <- 460 + t
    sg <- MW / 28.9
    z <- 1 / (1 + (p * 344400 * 10^1.78 * sg / TT^3.82))
    cat(z)
    rho <- p * MW / (10.7316 * TT * Z) * 0.0160185

    K <- 0.0001*(7.77+0.0063 * MW) * TT^1.5 / (122.4+12.9*MW + TT)
    X <- 2.57+1914.5 / TT +0.0095*MW
    Y <- 1.11+0.04*X
    mu <- K * exp(X * rho^Y)
    return(mu)
}


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
        return(do.call(z.hallyarborough, arggs))
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
#' @param as_list FALSE to return single value z; otherwise returns a list
#' @family Z factor correlations
#' @section Gas correlations
#' @export
z.hallyarborough <-function(pres.a, temp.f, gas.sg,
                            n2.frac = 0, co2.frac = 0, h2s.frac = 0,
                            as_list = FALSE) {
    # pres.a = absolute pressure, psia; temp.f  = temperature, deg F

    # calculate pseudo-critical pressure and temperature
    # get pseudo-reduced
    crit <- calcCriticals(pres.a, temp.f, gas.sg, co2.frac, h2s.frac, n2.frac)
    pres.pr <- crit$pres.pr
    temp.pr <- crit$temp.pr
    temp.r  <- crit$temp.r

    z <- zFactor::z.HallYarborough(pres.pr = pres.pr, temp.pr = temp.pr)
    if (as_list) {
        z <- named.list(z, pres.pr, temp.pr, temp.r)
    }
    return(z)
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
