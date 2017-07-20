#' Set the VLP input for the well
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname set_vlp_input
#' @export
setGeneric("set_vlp_input",    function(object, ...) standardGeneric("set_vlp_input"))

#' Set the VLP model definition
#'
#' @param object a class object
#' @param ... additional parameters
#' @rdname set_vlp_model
#' @export
setGeneric("set_vlp_model",    function(object, ...) standardGeneric("set_vlp_model"))

setGeneric("run_vlp_wellhead", function(object, ...) standardGeneric("run_vlp_wellhead"))

#' @rdname WellDeviationSurvey-class
#' @export
setGeneric("WellDeviationSurvey", function(md_tvd, ...) standardGeneric("WellDeviationSurvey"))

#' @rdname WellDeviationSurvey-methods
#' @export
setGeneric("computeAngle", function(object, ...) standardGeneric("computeAngle"))

