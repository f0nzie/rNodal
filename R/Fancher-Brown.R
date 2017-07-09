#' Fancher and Brown correlation
#' @param pres        pressure                           psia
#' @param temp        temperature                        degF
#' @param surf.params surface parameters list
fanbr.fanbr <- function(pres, temp, surf.params) {
    with(as.list(surf.params), {


    # # calculate gradient as function of pres and temp
    # dp.dz <- 1/log10(pres) * (2.5/log10(temp))^0.15

    # 1 Determine the total mass
    # mass.total

    # 2 determine w, the total mass flow rate per day
    mass.rt = mass.total * liq.rt

    # 3 select first pressure point
    # addressed by vlp.control iteration

    # 4 calculate the total volume of oil, gas and water. No slippage
    # total volume of oil, gas and water as per C.13.6

    # calculate z factor
    z <- z.hallyarborough(pres.a = pres, temp.f = temp, gas.sg)

    # assuming no slippage and calculate according to C.12
    # Rs <- oil.Rs(pres, temp, oil.sg, GOR)
    Rs <- oil.Rs.standing(pres, temp, API, gas.sg)
    gas.free = GOR - Rs  # scf/stb

    oil.fvf <- oil.fvf.standing(temp = temp, Rs = Rs, oil.sg = oil.sg, gas.sg = gas.sg)
    wat.fvf <- water.fvf.mccoy(pres, temp, salinity)

    mixL.volume = 5.61 * oil.fvf + 5.61 * wat.fvf +
        gas.free * (PRES.ATM / pres) * (460+temp) / (460+TEMP.STD) * z

    # 5 determine the density
    mix.dens <- mass.total / mixL.volume

    # 6 calculate the abscissa  of Fig 2.41
    xaxis.drhov <- 1.4737 * 10^-5 * mass.rt / diam

    # 7. call the interpolation function with degree = 4 from Fig 2.41
    ff <- interp.fanbr(drhov = xaxis.drhov, GLR = GLR, degree = 4)$ff

    # 8 Calculate dp/dh
    dp.dz <- 1/ 144 * (mix.dens + ff * (mass.rt^2 / (7.413 * 10^10 * mix.dens *
                                                         diam^5)) )

    out <- named.list(
                    GOR, GLR,
                    gas.free,
                    mass.total, mass.rt,
                    z, Rs, oil.fvf, wat.fvf,
                    mixL.volume, mix.dens,
                    xaxis.drhov, ff,
                    dp.dz
                      )

    return(out)
    }) # end-with
}
