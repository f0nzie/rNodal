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
