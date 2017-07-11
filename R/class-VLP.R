#' VLP class
#'
#' @param well_input an ODE object
#' @param vlp_model an ODE object
#' @param ... additional parameters
#' @rdname VLP-class
#' @export
setClass("VLP", slots = c(
    wellParameters = "list",
    modelParameters = "list")
)


setMethod("initialize", "VLP", function(.Object, well.parameters, model.parameters, ...) {
    # initialized
    with(as.list(c(well.parameters, model.parameters)), {

        })

    .Object@wellParameters  <- well.parameters
    .Object@modelParameters <- model.parameters
    return(.Object)
})

setGeneric("getState", function(object, ...) standardGeneric("getState"))

setMethod("getState", "VLP", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


#' VLP constructor
#'
#' @param well_input well production input
#' @param vlp_model the VLP model data
#' @export
#' @importFrom methods new
VLP <- function(well.parameters, model.parameters, verbose = FALSE) {
    vlp <- new("VLP", well.parameters, model.parameters)
    vlp
}


