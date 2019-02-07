
#' Glaso
#'
#' @param pres pressure
#' @param temp temperature
#' @param API oil density
#' @param gas.sg gas specific gravity
Rs.glaso <- function(pres, temp, API, gas.sg) {
    a <- -0.30218
    b <- 1.7447
    c <- 1.7669 - log10(pres)
    chi <-  10^((-b + sqrt(b^2 -4 * a * c)) / (2 * a))
    out <- (chi * API^0.989 / temp^0.172)^(1 / 0.816) * gas.sg
    #return(list(a, b, c, chi, out))
    return(out)
}



#' Formation Volume Factor by Glaso
#' @param temp temperature
#' @param Rs solution ratio
#' @param oil.sg oil specific gravity
#' @param gas.sg gas specific gravity
oil.fvf.glaso <- function(temp, Rs, oil.sg, gas.sg) {
    y <- Rs * (gas.sg / oil.sg)^0.526 + 0.968 * temp
    logbo_1 <- -6.58511 + 2.91329 * log10(y) - 0.27683 * log10(y)^2
    bo_1 <- 10^logbo_1
    bo <- bo_1 + 1
    #return(list(y, bo))
    return(bo)

}



#' Dead Oil viscosity by Beggs-Robinson
#' @param temp temperature
#' @param API oil specific gravity
oil.dead_visc.BeggsRobinson <- function(temp, API) {
    z <- 3.0324 - 0.02023 * API
    y <- 10^z
    x <- y * temp^-1.163
    return(10^x-1)
}


#' Saturated oil viscosity by Beggs-Robinson
#' Valid for pressures below the bubble point
#' @param temp temperature
#' @param API oil density
#' @param Rs gas oil solution ratio
oil.sat_visc.BeggsRobinson <- function(temp, API, Rs) {
    muod <- oil.dead_visc.BeggsRobinson(temp, API)
    A <- 10.715 * (Rs + 100)^-0.515
    B <- 5.44 * (Rs + 150)^-0.338
    return(A * muod^B)
}



#' Oil Formation Volumme Factor
#' @param pres pressure
#' @param temp temperature
#' @param oil.sg oil specific gravity
oil.fvf <- function(pres, temp, oil.sg) {
    1.111213
}

wat.fvf  <- function(pres, temp, wat.sg) {
    1.01010101
}


oil.Rs <- function(pres, temp, oil.sg, GOR) {
    GOR * 0.7
}


#' Gas oil solution ratio
#' @param pres pressure
#' @param temp temperature
#' @param API oil specific gravity
#' @param gas.sg gas specific gravity
oil.Rs.standing <- function(pres, temp, API, gas.sg) {
    ((pres / 18.2+1.4) * 10^(0.0125 * API) / 10^(0.00091 * temp))^(1/0.83) * gas.sg
}




#' Water Formation Volume Factor for water saturated wit gas. McCoy
#' From book Correlaciones Numericas PVT by Banzer pg 118
#'
#' @param pres      pressure                                  psia
#' @param temp      temperature                               deg F
#' @param salinity  salinity of water                         ppm
#'
water.fvf.mccoy <- function(pres, temp, salinity = 0) {
    # for water saturated with gas
    A = 0.9911 + 6.35 * 10^-5 * temp + + 8.5 * 10^-7 * temp^2
    B = -1.093 * 10^-6 - 3.497 * 10^-9 * temp + 4.57 * 10^-12 * temp^2
    C = -5.0 * 10^-11 + 6.429 * 10^-13 * temp - 1.43 * 10^-15 * temp^2

    # salinity correction factor. Since salinity is in ppm divide by 10,000
    # to get salinity in percentage in weight of dissolved solids
    salinity <- salinity / 10000
    bw.bwp <- 1 + salinity * (5.1 * 10^-8 * pres +
                           (5.47 * 10^-6 - 1.95 * 10^-10 * pres) *
                           (temp - 60) - (3.23 * 10^-8 - 8.5 * 10^-13 * pres) *
                           (temp -60)^2 )

    Bwp <- A + B * pres + C * pres^2

    Bw <- Bwp * bw.bwp
    return(Bw)
}



#' Oil Formation Volume Factor. Saturated. Standing
#' @param temp temperature
#' @param Rs gas oil solution ratio
#' @param oil.sg oil specific gravity
#' @param gas.sg gas specific gravity
oil.fvf.standing <- function(temp, Rs, oil.sg, gas.sg) {
    Bo <- 0.972 + 1.47 * 10^-4 * (Rs * (gas.sg / oil.sg)^0.5 + 1.25 * temp)^1.175
    return(Bo)
}



#' Oil Shrinkage
#' Craft, pg 35
#' @param oil.fvf    Oil Formation Volume factor         bbl/stb
oil.shrinkage <- function(oil.fvf) {
    oil.fvf
}



