
#' WellDeviationSurvey structure
#'
#' @param md_tvd MD and TVD
#' @param ... additional parameters
#' @rdname WellDeviationSurvey-class
.WellDeviationSurvey <- setClass("WellDeviationSurvey", slots = c(
    md_tvd = "data.frame",
    wds_table = "data.frame",
    md       = "numeric",
    tvd      = "numeric",
    angle    = "numeric",
    cum_disp = "numeric"
    )
)

setMethod("initialize", "WellDeviationSurvey", function(.Object, md_tvd, ...) {
    .Object@md_tvd  <- utils::read.table(header = TRUE, text = md_tvd)
    .Object <- computeAngle(.Object)
    return(.Object)
})


#' @rdname WellDeviationSurvey-methods
#' @export
setMethod("computeAngle", "WellDeviationSurvey", function(object, ...) {
    object@wds_table <- compute_angle_deviation_survey(object@md_tvd)

    invisible(object)
})



#' @rdname WellDeviationSurvey-class
#' @export
setMethod("WellDeviationSurvey", signature(md_tvd = "character"), function(md_tvd, ...) {
    .wds <- .WellDeviationSurvey(md_tvd = md_tvd)
    return(.wds)
})

#' @rdname WellDeviationSurvey-class
#' @export
setMethod("WellDeviationSurvey", signature(md_tvd = "missing"), function(md_tvd, ...) {
    # use this constructor when no ODE object is passed
    if (missing(md_tvd)) {
        md_tvd <- "
        MD	TVD
        0	    0
        600	    600
        1005	1000
        4075	4000
        7700	7500
        9275	9000
        "
        warning("No WDS supplied. Using an empty one!")
    }
    .wds <- .WellDeviationSurvey(md_tvd = md_tvd)
    return(.wds)
})



compute_angle_deviation_survey <- function(well_table,
                                           reference = c("vertical", "horizontal")) {
    # create table with delta MD, TVD, radian, degrees
    # also includes the reference: vertical or horizontal
    # calculate delta MD
    if (!is.data.frame(well_table )) stop("WDS must be a dataframe")
    if (ncol(well_table) != 2) stop("WDS should have at least two columns")
    if (!all(colnames(well_table) %in% c("MD", "TVD"))) stop("MD and TVD must exist in WDS")

    reference <- match.arg(reference)
    sh <- 1
    .md2 <- well_table[, "MD"]
    md2 <- shift(.md2, sh, default = 0)
    well_table[, "delta.md"] <- well_table[, "MD"] - md2

    # calculate delta TVD
    .tvd2 <- well_table[, "TVD"]
    tvd2 <- shift(.tvd2, sh, default = 0)
    well_table[, "delta.tvd"] <- well_table[, "TVD"] - tvd2

    # calculate the angle for each of the segments
    # if (reference == "vertical")
    #     well_table[, "radians"] <- acos(well_table[, "delta.tvd"] / well_table[, "delta.md"])
    # if (reference == "horizontal")
    #     well_table[, "radians"] <- asin(well_table[, "delta.tvd"] / well_table[, "delta.md"])

    switch(reference,
           vertical = well_table[, "radians"] <- acos(well_table[, "delta.tvd"] /
                                                          well_table[, "delta.md"]),
           horizontal = well_table[, "radians"] <- asin(well_table[, "delta.tvd"] /
                                                            well_table[, "delta.md"])
           )
    well_table[, "degrees"] <- well_table[, "radians"] * 180 / pi

    # fill NAs with zeroes
    well_table[, "radians"] <- na.zero(well_table[, "radians"])
    well_table[, "degrees"] <- na.zero(well_table[, "degrees"])

    well_table
}

