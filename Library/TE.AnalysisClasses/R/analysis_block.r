#' @include analysis_block_functions.r
NULL

####################################
#
# VirtualAnalysisBlock Class
#
####################################

#' Virtual S4 class implementing handling of analysis objects.
#'
#' Class that is to be inherited by any objects
#' that will encapsulate specific analysis computations.
#'
#' Inherits from "VirtualDataSourceClient"
#'
#' @slot ggplot object of class "ggplot"
#' @slot ggplot_data    "data.frame",
#' @slot frontend_data  "data.frame",
#' @slot output         "NullableReferenceData"

setClass(
  Class          = "VirtualAnalysisBlock",
  slots = c(
    ggplot       = "ANY",
    ggplot_data  = "data.frame",
    frontend_data    = "data.frame",
    output      = "NullableReferenceData"
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)


#' check if slot class is as required
#'
#' Private method to check if given value has correct class for the slot
#'
#' @rdname private_checkSlotClass
#' @param object object of class "VirtualAnalysisBlock"
#' @param slot "character" slot name which class is beeing checked
#' @param class "character" class name of the slot
#' @param req_class "character" vector of allowed slot classes
#' @return \code{object} object of class "VirtualAnalysisBlock"

setGeneric(".checkSlotClass", function(object,slot, class, req_class){standardGeneric(".checkSlotClass")})

setMethod(".checkSlotClass",
          signature(object = "VirtualAnalysisBlock", slot = "character", class = "character", req_class = "character"),
          function(object,slot, class, req_class){
          if (!any(class %in% req_class))  {
              message(paste("Invalid class", class, "."))
              message(paste("in attempt to set", sQuote(slot), "slot in object of class", class(object) ))
              stop(paste("assignment of an object of class",
                         dQuote(tail(class(class), 1)),
                                "is not valid for @", sQuote(slot), " in an object of class",
                                dQuote(class(object))
                         )
                   )
            } else {
              return(object)
            }


          }
)



#' Returns generated output object
#'
#' Returns Reference Data Object that possibly was computed by
#' analysis object. If module doesn't compute any output
#' returns NULL
#'
#' @param object object of class "VirtualAnalysisBlock"
#' @return \code{output} object of class derived from "VirtualReferenceData"
#'
#' @export

setGeneric("getOutputObject", function(object){standardGeneric("getOutputObject")})

#' @describeIn getOutputObject
#' Returns generated output object
#'
#' Returns Reference Data Object that possibly was computed by
#' analysis object. If module doesn't compute any output
#' returns NULL
#'
#' @inheritParams getOutputObject
#' @return \code{output} object of class derived from "VirtualReferenceData"
#'
#' @export
setMethod("getOutputObject",
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@output)
          }
)


#' Set internaly output object
#'
#' Private method to set output object slot
#'
#' @rdname private_setOutputObject
#' @param object object of class "VirtualAnalysisBlock"
#' @param ref_data_object object of class derived from "VirtualReferenceData"
#' @return \code{object} object object of class "VirtualAnalysisBlock"

setGeneric(".setOutputObject", function(object,ref_data_object){standardGeneric(".setOutputObject")})

setMethod(".setOutputObject",
          signature(object = "VirtualAnalysisBlock", ref_data_object = "VirtualReferenceData" ),
          function(object, ref_data_object){
            object@output <- ref_data_object
            return(object)
          }
)

#' Returns generated output ggplot
#'
#' Returns ggplot object generated as a result of analysis computation.
#'
#' @param object object of class "VirtualAnalysisBlock"
#' @return \code{ggplot} object of class "ggplot"
#'
#' @export

setGeneric("getOutputGGPlot", function(object){standardGeneric("getOutputGGPlot")})

#' @describeIn getOutputGGPlot
#' Returns generated output ggplot
#'
#' Returns ggplot object generated as a result of analysis computation.
#'
#' @inheritParams getOutputGGPlot
#' @return \code{ggplot} object of class "ggplot"
#'
#' @export
setMethod("getOutputGGPlot",
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@ggplot)
          }
)


#' Set internaly output ggplot object
#'
#' Private method to set output ggplot object slot
#'
#' @rdname private_setOutputGGPlot
#' @param object object of class "VirtualAnalysisBlock"
#' @param ggplot object of class "ggplot"
#' @return \code{object} object object of class "VirtualAnalysisBlock"

setGeneric(".setOutputGGPlot", function(object,ggplot){standardGeneric(".setOutputGGPlot")})

setMethod(".setOutputGGPlot",
          signature(object = "VirtualAnalysisBlock", ggplot = "ANY"),
          function(object, ggplot){
            object <- .checkSlotClass(object, "ggplot", class(ggplot), c("ggplot", "grob"))
            object@ggplot <- ggplot
            return(object)
          }
)


#' Returns generated output ggplot data
#'
#' Returns "data.frame" with data used to generate ggplot
#'
#' @param object object of class "VirtualAnalysisBlock"
#' @return \code{ggplot_data} object of class "data.frame"
#'
#' @export

setGeneric("getOutputGGPlotData", function(object){standardGeneric("getOutputGGPlotData")})

#' @describeIn getOutputGGPlotData
#' Returns generated output ggplot data
#'
#' Returns "data.frame" with data used to generate ggplot
#'
#' @inheritParams getOutputGGPlotData
#' @return \code{ggplot_data} object of class "data.frame"
#'
#' @export
setMethod("getOutputGGPlotData",
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@ggplot_data)
          }
)


#' Set internaly data for ggplot object
#'
#' Private method to set slot with data used to generate ggplot object.
#'
#' @rdname private_setOutputGGPlotData
#' @param object object of class "VirtualAnalysisBlock"
#' @param ggplot_data object of class "data.frame"
#' @return \code{object} object object of class "VirtualAnalysisBlock"

setGeneric(".setOutputGGPlotData", function(object, ggplot_data){standardGeneric(".setOutputGGPlotData")})

setMethod(".setOutputGGPlotData",
          signature(object = "VirtualAnalysisBlock", ggplot_data = "data.frame"),
          function(object, ggplot_data){
            object@ggplot_data <- ggplot_data
            return(object)
          }
)


#' Returns generated output frontend data
#'
#' Returns data to be used by any frontend using the module
#'
#' @param object object of class "VirtualAnalysisBlock"
#' @return \code{frontend_data} object of class "data.frame"
#'
#' @export

setGeneric("getOutputFrontendData", function(object){standardGeneric("getOutputFrontendData")})

#' @describeIn getOutputFrontendData
#' Returns generated output frontend data
#'
#' Returns data to be used by any frontend using the module
#'
#' @inheritParams getOutputFrontendData
#' @return \code{frontend_data} object of class "data.frame"
#'
#' @export
setMethod("getOutputFrontendData",
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@frontend_data)
          }
)


#' Set internaly data for ggplot object
#'
#' Private method to set frontend_data slot with data.frame used to provide to frontend services object
#'
#' @rdname private_setOutputFrontendData
#' @param object object of class "VirtualAnalysisBlock"
#' @param frontend_data object of class "data.frame"
#' @return \code{object} object object of class "VirtualAnalysisBlock"

setGeneric(".setOutputFrontendData", function(object, frontend_data){standardGeneric(".setOutputFrontendData")})

setMethod(".setOutputFrontendData",
          signature(object = "VirtualAnalysisBlock", frontend_data = "data.frame"),
          function(object, frontend_data){
            object@frontend_data <- frontend_data
            return(object)
          }
)



#' Trigger computation of analysis data.
#'
#' @param object object of class "VirtualAnalysisBlock"
#' @return \code{object} object object of class "VirtualAnalysisBlock"
#' @export

setGeneric("Process", function(object){standardGeneric("Process")})
