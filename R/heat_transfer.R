

temp.fluid <- function(well_table, theta, depth, bht, tht, U, cp.avg, diam.ft, mass.rate) {
    # old function
    # DO NOT USE
    ge <- (bht - tht) / depth
    k <- U * pi * diam.ft / mass.rate / cp.avg
    A <- 1 / k                   # relaxation distance by Ramey. Shoham, pg 297
    Ti <- bht

    for (i in nrow(well_table):1) {
        L <- depth - well_table[i, "depth"]
        Tei <- well_table[i, "temp"]
        Ti <- (Tei - ge * L * sin(theta)) +
            (Ti - Tei) * exp(-L/A) +
            ge * A * sin(theta) * (1 - exp(-L/A))

        # cat(sprintf("%3d %10.0f %10.2f \n", i, L, Ti))
        well_table[i, "L"]  <- L
        well_table[i, "Ti"] <- Ti
    }
    well_table
}



#' Calculate dT/dx using Ramey's equation
#' Requires angle, depths, BHT, WHT, U, tubing diameter, mass rate, cp.avg
#' cp.avg is heat capacity part of the basic initial calculations
#' from well_table we only need:
#'      depth table,
#'      temperature at depth
#' from well_parameters we need:
#'      U,
#'      angle,
#'      depth.bh, depth.wh,
#'      bht, tht,
#'      diam.ft
#'      mass.rate
#'      cp.avg
#' @param well_table a table
#' @param well.parameters well params
temp.gradient <- function(well_table, well.parameters) {
    # new function to calculate dT/dx using Ramey's equation
    # well angle is fixed because well is vertical.
    # Think of a vertical well for now.
    # Angle is constant.
    # TODO: make angle change at each segment or measuring point
    with(as.list(c(well.parameters)), {
        theta <- angle
        depth <- depth.bh - depth.wh
        ge    <- (bht - tht) / depth                       # geothermal gradient
        k     <- U * pi * diam.ft / mass.rate / cp.avg
        A     <- 1 / k          # relaxation distance by Ramey. Shoham, pg 297

        Ti <- bht               # initial temperature for the marching algorithm
        # calculate bottom up
        for (i in nrow(well_table):1) {           # ascending from the wellbore
            L <- depth - well_table[i, "depth"]   # calculate dL
            Tei <- well_table[i, "temp"]          # get ground temperature

            # calculate the well fluid temperature at "L" distance from the
            # wellbore at angle theta (beware: angle is constant)
            Ti <- (Tei - ge * L * sin(theta)) +
                (Ti - Tei) * exp(-L/A) +
                ge * A * sin(theta) * (1 - exp(-L/A))

            # cat(sprintf("%3d %10.0f %10.2f \n", i, L, Ti))
            well_table[i, "L"]  <- L     # table variable with "L" from wellbore
            well_table[i, "Ti"] <- Ti    # fluid temperature at depth
        }
        well_table
    })
}
