#' @include zfactor.R gascorrs.R moody.R
NULL

# this script contains the HAGBR correlation in pure R
# based on Brown, Guo books
# It is ready to be called from other R script or notebook

# source("zfactor.R")
# source("gascorrs.R")
# source("moody.R")


#' Hagedorn-Brown correlation as shown in Guo, Lyons, Ghalambor book
#' Beware that this correlation does not make P, T corrections for
#' fluid properties with exception of z factor
#' @param pres pressure
#' @param temp temperature degF
#' @param surf.params a list of surface parameters
hagbr.guo <- function(pres, temp, surf.params) {
  with(as.list(surf.params), {

    # calculate gas compressibility
    z <- z.hallyarborough(pres.a = pres, temp.f = temp, gas.sg)

    # calculate gas.massrt
    # calculate gas.dens

    # calculate superficial velocities
    vsg <- 1 / area * gas.rt * z * (460 + temp) / (460 + 60) * (14.7 / pres) / 86400
    vsl <- liq.rt * 5.615 / 86400 / area
    # vsl = (oil.rt * oil.fvf + wat.rt * wat.fvf * 5.615) / (86400 * area)
    vsm <- vsl + vsg   # mixture velocity

    liq.sg   <- (oil.rt * oil.sg + wat.sg * wat.rt) / liq.rt
    liq.visc <- (oil.visc * oil.rt + 0.5 * wat.rt) / liq.rt

    # mu.gas() returns a list. Need to extract $gas.visc
    gas.visc <- mu.gas(pres = pres, temp = temp, gas.sg)$gas.visc

    # holdup calculations from Duns and Ros
    holdup.out <- dunsros.holdup(vsl, liq.sg, if.tens, vsg, diam, liq.visc, pres)
    YL = holdup.out$YL                 # get only the liquid holdup from the list

    mix.visc <- liq.visc^YL * gas.visc^(1-YL)
    rg <- 28.97 * gas.sg * pres / (z * 10.73 * (460 + temp))           # lbm/ft^3
    mix.dens <- YL * liq.sg * 62.4 + (1 - YL) * rg       # lbm-ft^3
    gas.dens =28.97 * gas.sg * pres / (z * 10.74 * (460 + temp))

    liq.avgdens = 62.4 * liq.sg * YL
    gas.avgdens = gas.dens * (1 - YL)
    mix.avgdens = liq.avgdens + gas.avgdens

    # Reynolds
    mass.rt <- liq.sg * 62.4 * liq.rt * 5.615 + 0.0765 * gas.sg * gas.rt
    Re = 0.022 * mass.rt / (diam * mix.visc)

    # friction factor
    ff <- ff.chen(ed, Re)

    # pressure losses as gradients
    elev.grad <- 1 / 144 * mix.dens
    # equation 2.34 in Brown. Derivation in Appendix C.11
    fric.grad <- 1 / 144 * ff * mass.rt^2 / ( 7.413 * 10^10 * diam^5 * mix.dens)

    dp.dz <- elev.grad + fric.grad

    # return objects for visualization, report, plotting
    # $$$ FIXED: merge the list `holdup.out` with the rest
    # to merge list use c() instead of list
    out <-    c(named.list(z, vsg, vsm, gas.visc),       # list # 1
                holdup.out,                              # list # 2
                named.list(mix.visc, mass.rt, Re,        # list # 3
                ff, gas.dens, liq.avgdens,
                gas.avgdens, mix.avgdens,
                elev.grad, fric.grad, dp.dz)
                )

    return(out)
  }) # end with
}




#' Hagedorn correlation from the Brown's book. Procedure C.42
#'
#' @param    pres    pressure at depth          psia    dbl
#' @param    temp    temperature at depth       deg F   dbl
#' @param    surf.params  surface parameters            list
#'
hagbr.mod <- function(pres, temp, well.params) {
  with(as.list(well.params), {

    # calculate gradient as function of pres and temp

    # 3. calculate the total mass per STB at standard conditions
    #    already calculated in VLP script
    # mass = mass.total

    # 4. calculate the mass flow rate w = qm
    mass.rt = mass.total * liq.rt

    #out <- named.list(dp.dz, mass, mass.rt)

    # 5.1 Calculate Rs at P, T
    # Rs = GLR * 0.777          # Rs has always to be greater than GLR. Check
    #Rs = oil.Rs.standing(pres = pres, temp = temp, API = API, gas.sg = gas.sg)
    # Rs <- oil.Rs.standing(pres, temp, API, gas.sg)
    Rs <- Rs.glaso(pres, temp, API, gas.sg)

    # 5.2 Get Bo at P, T
    oil.fvf = oil.fvf.standing(temp = temp, Rs = Rs, oil.sg = oil.sg, gas.sg = gas.sg)
    oil.fvf <- oil.fvf.glaso(temp, Rs, oil.sg, gas.sg)

    # 6. Calculate the density of the liquid phase
    liq.dens = ( (62.4 * oil.sg + Rs * gas.sg * 0.0764 / 5.614) / oil.fvf ) *
      (1 / (1+WOR)) + 62.4 * wat.sg * (WOR / (1+WOR))


    # 7. Assuming T = constant, calculate z(P, T)
    z <- z.hallyarborough(pres.a = pres, temp.f = temp, gas.sg)

    # 8. calculate the average density of the gas phase
    gas.dens = 0.0764 * gas.sg * (pres / PRES.ATM) *
        ((460 + TEMP.STD) / (460 + temp)) * (1 / z)

    # this is missing from C.42 procedure
    # mu.gas() returns a list. Need to extract $gas.visc
    gas.visc <- mu.gas(pres = pres, temp = temp, gas.sg)$gas.visc

    # 9. Calculate the average viscosity of the oil oil.visc(P,T)
    # TODO: develop and use correlation
    oil.visc <- oil.sat_visc.BeggsRobinson(temp, API, Rs)
    # oil.visc <- 5.12345

    # 10. Calculate the average water viscosity
    # TODO: develop and use correlation
    wat.visc <- 1.00112233

    # 11. Calculate the liquid mixture viscosity
    mixL.visc <- oil.visc * oil.fraction + wat.visc * wat.fraction

    # 12. Calculate the liquid mixture surface tension
    oil.surft = 22.222
    wat.surft = 33.333

    mixL.surft = oil.surft * oil.fraction + wat.surft * wat.fraction


    # 13. Calculate the liquid viscosity number NL. Moved below
    # 14. Calculate CNL. Moved below
    # 15 area of the tubing. Calculated in surface parameters
    # 16. Calculate Bo(P,T). Done in step 5.2

    # 17.1 Calculate Bw(P,T)
    # TODO: use index for correlation instead
    wat.fvf = water.fvf.mccoy(pres = pres, temp = temp)

    # 17.2 Calculate the superficial liquid velocity
    liq.svel = 5.61 * liq.rt / (86400 * area) * (oil.fvf * oil.fraction +
                                                   wat.fvf * wat.fraction)

    # 18. Calculate the liquid velocity number. Moved below
    # 19 Calculate the superficial gas velocity
    gas.svel = liq.rt * (GLR - Rs * oil.fraction) / (86400 * area) *
      (PRES.ATM / pres) *
      (460 + temp) / (460 + TEMP.STD) * z

    # 18. Calculate the liquid velocity number
    # Instead we will obtain all Duns & Ros numbers
    dr.numbers <- dunsros.numbers(pres = pres, vsl = liq.svel, vsg = gas.svel,
                                  liq.dens = liq.dens, liq.visc = mixL.visc,
                                  liq.surft = mixL.surft, diam = diam)

    # 13. Calculate the liquid viscosity number NL
    # NL = 0.15726 * mixL.visc * (1 / (liq.dens * mixL.surft^3))^0.25
    # NL.dr = dunsros.NL(liq.visc = mixL.visc, liq.sg = liq.dens/62.4, if.tens = mixL.surft)
    NL = dr.numbers$NL

    # 14. Calculate CNL
    # CNL = dunsros.CNL(NL)
    CNL = dr.numbers$CNL

    # NLV = dunsros.NLV(vsl = liq.svel, liq.dens = liq.dens, liq.surft = mixL.surft)
    NLV = dr.numbers$NLV

    # 20 Determine the gas velocity number
    # NGV = dunsros.NGV(vsg = gas.svel, liq.dens = liq.dens, liq.surft = mixL.surft)
    NGV = dr.numbers$NGV

    # 21 check the flow regime to decide HAGBR or Griffith
      A = 1.071 - (0.2218 * (liq.svel + gas.svel)^2) / diam
      # if A >= 0.13 use A,if less then use A = 0.13
      B = gas.svel / (liq.svel + gas.svel)
      # if B-A is >= 0, go with HAGBR.
      # if B-A <0 proceed with Griffith
      # See Appendix C.6 under Orkiszewski
      BA = B -A

    # 22 Find the pipe diameter ND
      ND = dr.numbers$ND

    # 23 Calculate the holdup correlation phi
      X2 = dr.numbers$X2

    # 24 Obtain HL over psi
      HL.psi = dr.numbers$HL.psi

    # 25 Get the secondary correction factor phi
      X2.mod = dr.numbers$X2.mod

    # 26 Obtain psi
      psi = dr.numbers$psi

    #27 Calculate HL
      HL = dr.numbers$HL

    # 28 Find the two phase Reynolds number
      Re.TP = 2.2 * 0.01 * mass.rt / ( diam * mixL.visc^HL * gas.visc^(1-HL) )

    # 29 determine a value for relative roughness. Coming through surf.params

    # 30 obtain Friction Factor
      # friction factor
      ff <- ff.chen(ed, Re.TP)

    # 31 calculate the average two phase density of the mixture
      # (a) use HL
      mix.dens <- HL * liq.dens + (1 - HL) * gas.dens       # lbm-ft^3

      # (b) asume no slippage and calculate according to C.12
      gas.free = GOR - Rs  # scf/stb


      # => cat("==>", Rs, GOR, gas.free, "\n")

      # This is Bg
      gas.fvf <- (PRES.ATM / pres) * (460 + temp) / (460 + TEMP.STD) * z

      # total volume of oil, gas and water as per C.13.6 Brown
      mixL.volume = 5.61 * oil.fvf + 5.61 * wat.fvf + gas.free * gas.fvf

      mixL.dens = mass.total / mixL.volume

      # compare densities in (a) and (b) and use the greater
      mixTP.dens = ifelse(mix.dens >= mixL.dens, mix.dens, mixL.dens)

    # 32 Calculate the two-phase mixture velocity
      mixTP.svel = liq.svel + gas.svel

      # pressure losses as gradients
      elev.grad <- 1 / 144 * mixTP.dens
      fric.grad <- 1 / 144 * ff * mass.rt^2 / ( 2.9652 * 10^11 * diam^5 * mixTP.dens)

      dp.dz <- elev.grad + fric.grad


      out <- named.list(
                #mass.total, mass.rt,
                GOR, Rs, gas.fvf, gas.free,
                liq.dens, z, gas.dens,
                oil.visc, wat.visc, mixL.visc,
                oil.fvf, wat.fvf,
                liq.svel, gas.svel,
                NL, CNL, NLV, NGV,
                A, B, BA,
                ND, X2, HL.psi, X2.mod, psi, HL,
                Re.TP, ff,
                mix.dens,
                mixL.volume, mixL.dens,
                mixTP.dens, mixTP.svel,
                elev.grad, fric.grad,
                dp.dz
                )
    return(out)

  } ) # end with

}




#' Hagedorn-Brown dummy function using 1/log10(P)
#' and 1/log10(T) to calculate dp.dz
#' @inheritParams hagbr.guo
hagbr.dummy <- function(pres, temp, surf.params) {
  # calculate gradient as function of pres and temp
  dp.dz <- 1/log10(pres) * (2.5/log10(temp))^0.15

  out <- list(dp.dz = dp.dz)
  return(out)
}
