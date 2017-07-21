
#' WellDeviationSurvey structure
#'
#' @param md_tvd MD and TVD
#' @param reference angle measured against the vertical or horizontal
#' @param object a class object
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

setMethod("initialize", "WellDeviationSurvey",
          function(.Object, md_tvd, reference, ...) {
    .Object@md_tvd  <- utils::read.table(header = TRUE, text = md_tvd)
    .Object <- computeAngle(.Object, reference)
    return(.Object)
})


#' @rdname WellDeviationSurvey-class
#' @export
setMethod("computeAngle", "WellDeviationSurvey", function(object, reference, ...) {
    object@wds_table <- compute_angle_deviation_survey(object@md_tvd, reference)
    invisible(object)
})



#' @rdname WellDeviationSurvey-class
#' @export
setMethod("WellDeviationSurvey", signature(md_tvd = "character"),
          function(md_tvd, reference = "vertical", ...) {
    .wds <- .WellDeviationSurvey(md_tvd = md_tvd, reference)
    return(.wds)
})


#' @rdname WellDeviationSurvey-class
#' @export
setMethod("WellDeviationSurvey", signature(md_tvd = "missing"),
          function(md_tvd, reference = "vertical", ...) {
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
    .wds <- .WellDeviationSurvey(md_tvd = md_tvd, reference)
    return(.wds)
})


#' Compute the angle of a deviated well and cumulative displacement
#'
#' @rdname WellDeviationSurvey-functions
#' @param well_table a well deviation survey
#' @param reference angle against the vertical or horizontal
#' @export
compute_angle_deviation_survey <- function(well_table,
                                           reference = c("vertical",
                                                         "horizontal")) {
    # create table with delta MD, TVD, radian, degrees
    # also includes the reference: vertical or horizontal
    # calculate delta MD
    if (!is.data.frame(well_table )) stop("WDS must be a dataframe")
    if (ncol(well_table) != 2) stop("WDS should have at least two columns")
    if (!all(colnames(well_table) %in% c("MD", "TVD")))
                                            stop("MD and TVD must exist in WDS")
    reference <- match.arg(reference)
    sh <- 1
    .md2 <- well_table[, "MD"]
    md2 <- shift(.md2, sh, default = 0)
    well_table[, "delta.md"] <- well_table[, "MD"] - md2

    # calculate delta TVD
    .tvd2 <- well_table[, "TVD"]
    tvd2 <- shift(.tvd2, sh, default = 0)
    well_table[, "delta.tvd"] <- well_table[, "TVD"] - tvd2

    # calculate angle depending of the reference
    switch(reference,
       vertical   = {arcFun <- acos; dispFun <- sin},
       horizontal = {arcFun <- asin; dispFun <- cos} )

    well_table[, "radians"] <- arcFun(well_table[, "delta.tvd"] / well_table[, "delta.md"])
    well_table[, "disp"] <- well_table[, "delta.md"] * dispFun(well_table[, "radians"])
    well_table[, "disp"]     <- round(na.zero(well_table[, "disp"]), 3)
    well_table[, "cum_disp"] <- cumsum(well_table[, "disp"])
    # convert angle to degrees
    well_table[, "degrees"] <- well_table[, "radians"] * 180 / pi
    # fill NAs with zeroes
    well_table[, "radians"] <- na.zero(well_table[, "radians"])
    well_table[, "degrees"] <- na.zero(well_table[, "degrees"])
    well_table
}

