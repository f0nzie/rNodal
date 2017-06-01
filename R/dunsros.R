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
