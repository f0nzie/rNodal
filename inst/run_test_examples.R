#' @include utils.R

# Test all applications under the `examples` folder.
# Gets the list of all applications by filtering those ending with `App`.
# Removes the extension `.R` from each app and starts looping to call each of the
# applications with `do.call`.
#
# A list contains the `expected` results that are compared against the result
# coming out from the call to the R application.


library(testthat)

expected <- list(
        vlp_brown_c13_App = list(
            vars = c("depth", "dL", "pres", "z"),
            row_vector = list(depth = 2670, dL = 242.7273, pres = 1043.109, z = 0.9000327),
            tolerance  = 1e-6),

        vlp_brown_c44_App = list(
            vars = c("depth", "dL", "pres", "z"),
            row_vector = list(depth = 3590, dL = 239.3333, pres = 905.1789, z = 0.9109475),
            tolerance  = 1e-6),

        vlp_brown_ow01_App = list(
            vars = c("depth", "dL", "pres", "z"),
            row_vector = list(depth =  9275, dL = 319.8276, pres = 769.3803, z = 0.9235756),
            tolerance  = 1e-6),

        vlp_brown_guo_ep44_App = list(
            vars = c("depth", "dL", "pres", "z"),
            row_vector = list(depth =  9700, dL = 334.4828, pres = 1909.763, z = 0.8529847),
            tolerance  = 1e-6)

) # end of list for expected values



loop_on_examples <- function(aPackage, goDebug = FALSE) {
    examples <- rNodal:::get_list_examples(aPackage = aPackage)
    # loop to open each file
    if (goDebug) {
        rng <- 1:5
        examples <- examples[rng]          # reduce the list for debugging
    }
    i <- 1
    for (app in examples) {
        application <- sub("\\.R$", '', app)
        cat(sprintf("\n %3d testing ... %30s", i, app))
        source(paste(system.file("examples", package = aPackage), app, sep ="/"))
        vars    <- expected[[application]]$vars
        result  <- do.call(application, list(vars, verbose = TRUE))
        .result <- as.list(result[nrow(result), ]);
        cat(sprintf("%30s", names(expected[application])))
        if (goDebug) {
            cat("\n");
            print(.result)}
        last_row <- expected[[application]]$row_vector
        expect_equal(.result, last_row, tolerance = expected[[application]]$tolerance)
        cat("\t tested")
        i <- i + 1
    }
}

loop_on_examples(aPackage = "rNodal", goDebug = FALSE)




