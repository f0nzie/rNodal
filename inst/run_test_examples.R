#' @include utils.R

# Test all applications under the `examples` folder.
# Gets the list of all applications by filtering those ending with `App`.
# Removes the extension `.R` from each app and starts looping to call each of the
# applications with `do.call`.
# A list contains the `expected` results that are compared against the result
# coming out from the call to the R application.


library(testthat)

expected <- list(AdaptiveStepApp = list(
    rowVector = list(s1 = 9.910181, s2 = 2.697124, t=6.635591),
    tolerance = 1e-7),

    ComparisonRK45App = list(
        rowVector = list(t=45.75203, s1=9.691675e-10, s2=45.75203,
                         xs=1.6393e-21, rc=598, time=49.46946),
        tolerance = 1e-7),

    ComparisonRK45ODEApp = list(
        rowVector = list(t=43.22881, ODE=1.108886e-07, s2=43.22881,
                         exact=5.249407e-21, rate.counts=280, time=48.30561),
        tolerance = 1e-6),

    FallingParticleODEApp = list(
        rowVector = list(t=1.43, y=0.05006, vy=-14.014),
        tolerance = 1e-12),

    KeplerApp = list(
        rowVector = list(t=9.971599,
                         planet1.r=0.5303791, p1anet1.v=-1.339153,
                         planet2.r=0.4781323, p1anet2.v=-0.2645141),
        tolerance = 1e-6),

    KeplerDormandPrince45App = list(
        rowVector = list(t=1.444831, x=-0.9404485, vx=-2.136978,
                         y=0.3400727, vx=-5.908522, energy=-19.73793),
        tolerance = 1e-6),

    KeplerEnergyApp = list(
        rowVector = list(t=1.19, x=0.3757742, vx=-5.817511,
                         y=0.927382, vy=2.363469, E=-19.73918),
        tolerance = 1e-5),

    KeplerEulerApp = list(
        rowVector = list(t=0.98, x=-0.6514722, vx=4.019233,
                         y=-1.578873, vy=-2.069668, E=-12.89498),
        tolerance = 1e-5),

    LogisticApp = list(
        rowVector = list(t=10, s1=0.1001006, s2=-0.006313836),
        tolerance = 1e-5),

    PendulumApp = list(
        rowVector = list(t=39.9, theta=0.2066864, thetadot=-0.05442821),
        tolerance = 1e-5),

    PendulumEulerApp = list(
        rowVector = list(t=50, theta=-0.1246416, thetaDot=0.6912865),
        tolerance = 1e-5),

    PendulumRK4App = list(
        rowVector = list(state1=-0.1969044, state2=-0.06044034, state3=19.9),
        tolerance = 1e-5),

    PlanetApp = list(
        rowVector = list(t=89.95, x=0.5448977, vx=1.754475,
                         y=-8.489961, vy=0.2846164),
        tolerance = 1e-5),

    ProjectileApp = list(
        rowVector = list(t=2.04, x=20.4, vx=10, y=0.00816, vy=-9.992),
        tolerance = 1e-5),

    ReactionApp = list(
        rowVector = list(t=100, X=2.131958, Y=1.105316),
        tolerance = 1e-6),

    RigidBodyNXFApp = list(
        rowVector = list(t=12, y1=-0.7434223, y2=-0.7347173, y3=0.8655997),
        tolerance = 1e-6),

    SHOApp = list(
        rowVector = list(x=-1.007698, v=0.422443, t=499.905),
        tolerance = 1e-6),

    SpringRK4App = list(
        rowVector = list(t=21.9, y1=-0.009364557, y2=0.08782852),
        tolerance = 1e-6),

    VanderpolApp = list(
        rowVector = list(t=19.95867, y1=2.008232, y2=0.04026076),
        tolerance = 1e-6),

    VanderpolMuTimeControlApp = list(
        rowVector = list(t=29.94172, y1=-1.910788, y2=0.07189794),
        tolerance = 1e-7)

) # end of list for expected values



expected <- list(
        vlp_brown_c13_App = list(
            vars = c("depth", "dL", "z"),
            row_vector = list(depth = 2670, dL = 242.7273, z = 0.9000327),
            tolerance  = 1e-6
    )
)

# examples <- rNodal:::get_list_examples(aPackage = "rNodal")


loop_on_examples <- function(aPackage, goDebug = FALSE) {
    examples <- rNodal:::get_list_examples(aPackage = aPackage)
    # loop to open each file
    # goDebug <- FALSE
    nmax <- 0
    if (goDebug) {
        nmax <- 1
        examples <- examples[1:nmax]          # reduce the list for debugging
    }
    i <- 1
    for (app in examples) {
        application <- sub("\\.R$", '', app)
        cat(sprintf("\n %3d testing ... %30s", i, app))
        source(paste(system.file("examples", package = aPackage), app, sep ="/"))
        # result  <- do.call(application, list(vars = c("depth", "dL"), verbose = FALSE))
        vars    <- expected[[application]]$vars
        result  <- do.call(application, list(vars, verbose = TRUE))
        .result <- as.list(result[nrow(result), ]);
        cat(sprintf("%30s", names(expected[application])))
        # if ((goDebug) && (names(expected[application]) == "VanderpolMuTimeControlApp")) {
        if (goDebug) {
            cat("\n");
            print(.result)}
        last_row <- expected[[application]]$row_vector
        expect_equal(.result, last_row, tolerance = expected[[application]]$tolerance)
        # cat(expected[[application]]$tolerance)
        cat("\t tested")
        i <- i + 1
    }

}

loop_on_examples(aPackage = "rNodal", goDebug = FALSE)




