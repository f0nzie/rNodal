
calc_fluid_temp <- function(depth_table=NULL, fluid_temp_parameters) {
    # depth_table with these columns:
    #       tvd, md, delta.tvd, delta.md, radians, geotherm
    # print(depth_table)
    # Hmisc::list.tree(fluid_temp_params, maxcomp = 20, attr.print=FALSE)

    # if (is.null(depth_table)) depth_table <- build_depth_table()

    diam_ft <- fluid_temp_parameters$diam.ft
    tht <- fluid_temp_parameters$tht
    bht <- fluid_temp_parameters$bht
    depth_wh <- fluid_temp_parameters$depth.wh
    depth_bh <- fluid_temp_parameters$depth.bh
    geo_grad <- fluid_temp_parameters$temp.grad
    U <- fluid_temp_parameters$U
    cp_avg <- fluid_temp_parameters$cp.avg
    mass_rate <- fluid_temp_parameters$mass.rate

    depth <- depth_bh - depth_wh
    k <- U * pi * diam_ft / mass_rate / cp_avg
    A <-  1 / k

    Ti <- bht
    for (i in nrow(depth_table):1) {           # ascending from the wellbore
        # if we are inifinitesimally close to zero depth don't make the angle zero
        # because near zero the angle of a vertical well is 90 degrees
        if (depth_table[i, "tvd"] == 0) theta <- depth_table[i+1, "radians"]
        else theta <- depth_table[i, "radians"]

        L <- depth - depth_table[i, "tvd"]   # calculate dL
        Tei <- depth_table[i, "geotherm"]          # get ground temperature

        # calculate well fluid temperature at "L" distance from the
        # wellbore at angle theta
        Ti <- (Tei - geo_grad * L * sin(theta)) +
            (Ti - Tei) * exp(-L/A) +
            geo_grad * A * sin(theta) * (1 - exp(-L/A))

        cat(sprintf("%3d %10.2f %8.3f %10.2f %10.2f \n", i, L, theta, Tei, Ti))


        depth_table[i, "L_bottom"]  <- L     # table variable with "L" from wellbore
        depth_table[i, "Ti"] <- Ti    # fluid temperature at depth
    }
    depth_table
}
