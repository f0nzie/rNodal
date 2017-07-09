#' Duns and Ros correlation
#' @param pres      pressure                  psia
#' @param temp      temperature               deg Far
#' @param surf.params surface parameters list
dunsros.0 <- function(pres, temp, surf.params) {

    # calculate gradient as function of pres and temp
    dp.dz <- 1/log10(pres) * (2.5/log10(temp))^0.15

    out <- named.list(dp.dz
    )

    return(out)
}




#' Holdup function according to Duns & Ros
#' Equation in 2.62 pg 43 Ovadia Shoham
#' Equation 2.39, 2.310, 2.311, 2.312 in Brown pg 113
#'
#' @param pres      pressure                           psia
#' @param vsl       liquid superficial velocity        ft/s
#' @param vsg       superficial velocity of gas        ft/s
#' @param diam      diameter of tubing                 ft
#' @param liq.visc  liquid viscosity                   cp
#' @param liq.sg    liquid specif gravity
#' @param if.tens   interfacial tension
#'
#' TODO: pass liq.dens instead of liq.sg
#' TODO: move pres to start of params list
#'
dunsros.holdup <- function(vsl, liq.sg, if.tens, vsg, diam, liq.visc, pres) {

    NvL <- 1.938 * vsl * (62.4 * liq.sg/if.tens)^0.25
    NvG <- 1.938 * vsg * (62.4 * liq.sg/if.tens)^0.25
    ND <- 120.872 * diam * sqrt(62.4 * liq.sg/if.tens)

    NL  <- dunsros.NL(liq.visc, liq.sg, if.tens)
    CNL <- dunsros.CNL(NL)


    X2 <- NvL * pres^0.1 * CNL / (NvG^0.575 * PRES.ATM^0.1 * ND)

    YL.psi <- -0.10307 + 0.61777 * (log10(X2) + 6) - 0.63295 *
        (log10(X2) + 6)^2 + 0.29598 * (log10(X2) + 6)^3 - 0.0401 *
        (log10(X2) + 6)^4

    X3 <- NvG * NL^0.38/ND^2.14

    idx = (X3 - 0.012)/abs(X3 - 0.012)
    X3.mod = (1 - idx)/2 * 0.012 + (1 + idx)/2 * X3

    psi = 0.91163 - 4.82176 * X3.mod + 1232.25 * X3.mod^2 - 22253.6 *
        X3.mod^3 + 116174 * X3.mod^4

    # Holdup for original Hagedorn-Brown
    YL <- psi * YL.psi

    # return all values in function environment as a list
    # but remove input parameters first. HDF5 files do not like duplicate members
    rm(vsl, liq.sg, if.tens, vsg, diam, liq.visc, pres)
    out <- sapply(ls(), function(x) get(x), simplify = F, USE.NAMES = T)
    return(out)
}


#' Duns and Ros adimensional numbers
#' @param pres      pressure                           psia
#' @param vsl       liquid superficial velocity        ft/s
#' @param vsg       superficial velocity of gas        ft/s
#' @param liq.dens  liquid density
#' @param diam      diameter of tubing                 ft
#' @param liq.visc  liquid viscosity                   cp
#' @param liq.surft liquid surface tension
dunsros.numbers <- function(pres, vsl, vsg, liq.dens, liq.visc, liq.surft, diam) {
    # liq.sg is liquid specific gravity. TODO: change later to density
    NL <- 0.15726 * liq.visc * (1 / (liq.dens * liq.surft^3))^0.25

    # CNL to account for the viscosity of the liquid
    CNL <- 10^(-2.69851 + 0.15841 * (log10(NL) + 3) - 0.551 *
                   (log10(NL) + 3)^2 + 0.54785 * (log10(NL) + 3)^3 - 0.12195 *
                   (log10(NL) + 3)^4)

    # Liquid velocity number NLV. step 18 in C.42 Brown
    NLV <- 1.938 * vsl * (liq.dens / liq.surft)^0.25

    # gas velocity number NGV. C42.20
    # TODO: remove the abs() after downhole correlations completed
    NGV <- 1.938 * vsg * (liq.dens / liq.surft)^0.25

    # pipe diameter number ND, C42.22
    ND  <- 120.872 * diam * sqrt(liq.dens / liq.surft)

    # Calculate the holdup correlating function phi, C.42.23
    # cat(NLV, NGV, pres, PRES.ATM, CNL, ND)
    X2 <- (NLV / NGV^0.575) * (pres / PRES.ATM)^0.1 * (CNL / ND)
    #phi <- X2

    # Obtain YL.psi. C42.24
    HL.psi <- -0.10307 + 0.61777 * (log10(X2) + 6) - 0.63295 *
        (log10(X2) + 6)^2 + 0.29598 * (log10(X2) + 6)^3 - 0.0401 *
        (log10(X2) + 6)^4

    # Determine the secondary correction factor phi. C42.25
    X2.mod <- NGV * NL^0.38 / ND^2.14

    # obtain psi, C42.26
    psi = 0.91163 - 4.82176 * X2.mod + 1232.25 * X2.mod^2 - 22253.6 *
        X2.mod^3 + 116174 * X2.mod^4

    # calculate a value for HL. C42.27
    # TODO: psi = 1 for low viscosities
    HL <- psi * HL.psi


    named.list(CNL, NL, NLV, NGV, ND, X2, HL.psi, X2.mod, psi, HL)
}




dunsros.NGV <- function(vsg, liq.dens, liq.surft) {
    1.938 * vsg * (liq.dens / liq.surft)^0.25
}


dunsros.NLV <- function(vsl, liq.dens, liq.surft) {
    # step 18 in C.42 Brown
    1.938 * vsl * (liq.dens / liq.surft)^0.25
}


dunsros.NL <- function(liq.visc, liq.sg, if.tens) {
    # liq.sg is liquid specific gravity. TODO: change later to density
    NL <- 0.15726 * liq.visc * (1/(62.4 * liq.sg * if.tens^3))^0.25
}

dunsros.CNL <- function(NL) {
    # CNL to account for the viscosity of the liquid
    CNL <- 10^(-2.69851 + 0.15841 * (log10(NL) + 3) - 0.551 *
                   (log10(NL) + 3)^2 + 0.54785 * (log10(NL) + 3)^3 - 0.12195 *
                   (log10(NL) + 3)^4)
}


#' Griffith correlation for bubble-flow regime
#' Mentioned in Brown 2.3332 Generalized correlation of HagBr
#' @param vsm    superficial velocity of mixture
#' @param vsg    superficial velocity of gas
#' @param vsl    superficial velocity of liquid
holdup.griffith <- function(vsm, vsg, vsl = 0.8) {
    # vsm = superficial velocity of mixture, ft/2
    # vsl = superficial velocity for liquid phase, ft/2
    # vsg = superficial velocity for gas phase, ft/2

    1 - 0.5 * (1 + vsm / vsl) - sqrt((1 + vsm / vsl)^2 - 4 * vsg / vsl)
}

