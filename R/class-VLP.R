#' VLP class
#'
#' @param object     a class object
#' @param well.parameters an ODE object
#' @param model.parameters an ODE object
#' @param vlpInput the VLP input
#' @param vlpModel the VLP model input
#' @param ... additional parameters
#' @rdname VLP-class
#' @export
setClass("VLP", slots = c(
    wellParameters  = "list",
    modelParameters = "list",
    vlpInput        = "list",
    vlpModel        = "list"
    )
)


setMethod("initialize", "VLP", function(.Object, well.parameters, model.parameters, ...) {
    # initialized
    with(as.list(c(well.parameters, model.parameters)), {

        })

    .Object@wellParameters  <- well.parameters
    .Object@modelParameters <- model.parameters
    return(.Object)
})

setMethod("show", "VLP", function(object) {
    print(object@wellParameters)
    print(object@modelParameters)
})


#' Set the VLP inputs
#'
#' @param field.name field name. Usually comprises several wells
#' @param well.name  well name
#' @param depth.wh  depth at wellhead
#' @param depth.bh  depth at bottomhole of well                    feet
#' @param diam.in   well diameter                                  inches
#' @param GLR       gas liquid ratio
#' @param liq.rt    liquid rate
#' @param wcut      watercut
#' @param thp       tubing head pressure
#' @param tht       tubing head temperature
#' @param bht       bottomhole temperature
#' @param API       gravity of oil
#' @param gas.sg    specific gravity of gas
#' @param wat.sg    specific gravity of water
#' @param oil.visc  oil viscosity
#' @param ed        relative rougness
#' @param if.tens   interfacial tension between ...
#' @param salinity  water salinity
#' @rdname set_vlp_input
#' @export
setMethod("set_vlp_input", "VLP",
          function(object,
                   field.name = "HAGBR.GUO",
                   well.name = "ExampleGuo",
                   depth.wh  = 0,                         # depth at wellhead
                   depth.bh  = 9700,                         # depth at bottomhole
                   diam.in   = 1.995,
                   GLR       = 75,
                   liq.rt    = 758,
                   wcut      = 0.10,
                   thp       = 200,                         # tubing head pressure
                   tht       = 80,                         # tubing head temperature
                   bht       = 180,                         # bottomhole temperature
                   API       = 40,
                   gas.sg    = 0.7,
                   wat.sg    = 1.05,
                   oil.visc  = 5,
                   ed        = 0.0006,                         # relative roughness
                   if.tens   = 30,
                   salinity  = 0,
                   ...)
          {
              # well input parameters
        object@vlpInput <- named.list(
                  field.name,
                  well.name,
                  depth.wh,
                  tht,
                  depth.bh,
                  bht,
                  diam.in,
                  ed,
                  thp,
                  liq.rt,
                  wcut,
                  API,
                  oil.visc,
                  gas.sg,
                  GLR,
                  wat.sg,
                  salinity,
                  if.tens
              )
    return(object@vlpInput)
    }
)


#' Set VLP model parameters
#'
#' such as tolerance for iterations, initial value of dp/dz, the HDF5 data file
#' @param vlp.model     the name of the VLP model or correlation
#' @param segments      number of segments for the tubing string
#' @param tol           tolerance for error during interations
#' @param dp.dz.ini     initial gradient
#' @rdname set_vlp_model
#' @export
setMethod("set_vlp_model", "VLP",
          function(object,
                   vlp.model = "hagbr.guo",  # name of the VLP correlation
                   segments = 29,            # table rows = segments + 1
                   tol = 0.0001,             # tolerance in dp.dz calc
                   dp.dz.ini = 0.002,         # initial value for dp.dz
                   ...) {

            object@vlpModel <- named.list(vlp.model,
                                          segments,
                                          tol,
                                          dp.dz.ini
                                          )
    return(object@vlpModel)
})



setMethod("run_vlp_wellhead", "VLP", function(object, ...) {
    return(object)
})

setGeneric("getState", function(object, ...) standardGeneric("getState"))
setMethod("getState", "VLP", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})










#' VLP constructor
#'
#' @param well.parameters well production input
#' @param model.parameters the VLP model data
#' @export
#' @importFrom methods new
VLP <- function(well.parameters, model.parameters) {
    vlp <- new("VLP", well.parameters, model.parameters)
    vlp
}


