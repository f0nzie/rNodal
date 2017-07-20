
#' WellDeviationSurvey structure
#'
#' @param md_tvd MD and TVD
#' @param ... additional parameters
#' @rdname WellDeviationSurvey-class
.WellDeviationSurvey <- setClass("WellDeviationSurvey", slots = c(
    md_tvd = "ANY",
    md  = "list",
    tvd = "list",
    angle    = "list",
    cum_disp    = "list"
    )
)

setMethod("initialize", "WellDeviationSurvey", function(.Object, md_tvd, ...) {
    .Object@md_tvd  <- utils::read.table(header = TRUE, text = md_tvd)
    return(.Object)
})


#' @rdname WellDeviationSurvey-class
#' @export
setMethod("WellDeviationSurvey", signature(md_tvd = "character"), function(md_tvd, ...) {
    .wds <- .WellDeviationSurvey(md_tvd = md_tvd)
    # .wds <- init(.euler, .euler@stepSize)
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
    # .euler <- init(.euler, .euler@stepSize)
    return(.wds)
})



