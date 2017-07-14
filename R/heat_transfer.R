

temp.fluid <- function(well_table, theta, depth, bht, tht, U, cp.avg, diam.ft, mass.rate) {
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



temp.gradient <- function(well_table, well.parameters) {
    with(as.list(c(well.parameters)), {
        theta <-  angle
        depth <- depth.bh - depth.wh
        ge    <- (bht - tht) / depth
        k     <- U * pi * diam.ft / mass.rate / cp.avg
        A     <- 1 / k                   # relaxation distance by Ramey. Shoham, pg 297

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
    })
}
