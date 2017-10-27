
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
setMethod("getMD", "WellDeviationSurvey", function(object, ...) {
    object@wds_table[["MD"]]

})

#' @rdname WellDeviationSurvey-class
#' @export
setMethod("getTVD", "WellDeviationSurvey", function(object, ...) {
    object@wds_table[["TVD"]]

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



split_well_in_deltas <- function(md_vector, by, length.out) {
    stopifnot(is.numeric(md_vector))
    stopifnot(!all(missing(by) && missing(length.out)))

    # split the MD of the well in equal parts but a total of "n" segments
    if (missing(by))
        split <- seq.int(min(md_vector), max(md_vector), length.out = length.out)
    if (missing(length.out))
        split <- seq.int(min(md_vector), max(md_vector), by = by)

    # add the known MD values to the sequence. Now the length is little bit longer
    sort(unique(c(md_vector, split)))
}



build_survey_with_deltas <- function(deviation_survey, md_split) {
    # reconstruct MD v TVD but for the partitioned well in delta-x
    df <- data.frame()     # new long dataframe
    index <- 1             # index the small dataframe
    tvd <- 0
    for (j in 1:length(md_split)) {  # iterate through the sequence
        row = deviation_survey[index, ]   # get a row of the deviation survey
        df[j, "md"]  <- md_split[j]  # assign MD in sequence to md in long dataframe
        df[j, "seg"] <- index      # assign
        if (j == 1)                 # if it is the first row
            df[j, "delta.md"] <- md_split[j]
        else
            df[j, "delta.md"] <- md_split[j] - df[j-1, "md"]

        df[j, "radians"] <- row[["radians"]]
        df[j, "degrees"] <- row[["degrees"]]
        df[j, "delta.tvd"] <- cos(row[["radians"]]) * df[j, "delta.md"] # calculate delta TVD
        tvd <- tvd + df[j, "delta.tvd"]        # add delta.tvd
        df[j, "tvd"] <- tvd                    # tvd column
        if (md_split[j] >= row[["MD"]]) {        # switch to next deviation branch
            index <- index + 1
        }
    }
    df
}
