#' VLP class
#'
#' @param well_input an ODE object
#' @param vlp_model an ODE object
#' @param ... additional parameters
#' @rdname VLP-class
#' @export
setClass("VLP2", slots = c(
    wellInput = "list",
    vlpModel = "list")
)


setMethod("initialize", "VLP2", function(.Object, well_input, vlp_model, ...) {
    # initialized
    .Object@wellInput <- well_input
    .Object@vlpModel  <- vlp_model
    return(.Object)
})

setGeneric("getState", function(object, ...) standardGeneric("getState"))

setMethod("getState", "VLP2", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


#' VLP constructor
#'
#' @param well_input well production input
#' @param vlp_model the VLP model data
#' @export
#' @importFrom methods new
VLP <- function(well_input, vlp_model) {
    vlp <- new("VLP2", well_input, vlp_model)
    vlp
}


