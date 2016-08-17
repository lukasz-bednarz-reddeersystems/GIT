sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)

####################################
#
# VirtualAnalysisBlock Class
#
####################################


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

if (!isGenericS4(".checkSlotClass")) {
  setGeneric(".checkSlotClass", function(object,slot, class, req_class, ...){standardGeneric(".checkSlotClass")})
}
# Private method to check if given value has correct class for the slot 
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualAnalysisBlock"

setMethod(".checkSlotClass",  
          signature(object = "VirtualAnalysisBlock", slot = "character", class = "character", req_class = "character"),
          function(object,slot, class, req_class){
          if (!any(class %in% req_class))  {
              message(paste("Invalid class", class, "."))
              message(paste("in attempt to set", sQuote(slot), "slot in object of class", class(object) ))
              stop(paste("assignment of an object of class",
                         dQuote(tail(class(class), 1),
                                "is not valid for @", sQuote(slot), " in an object of class",
                                dQuote(class(object)))))
            } else {
              return(object)
            }
            
            
          }
)



if (!isGenericS4("getOutputObject")) {
  setGeneric("getOutputObject", function(object,...){standardGeneric("getOutputObject")})
}
# Returns generated GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   ggplot : ggplot object

setMethod("getOutputObject",  
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@output)
          }
)

if (!isGenericS4(".setOutputObject")) {
  setGeneric(".setOutputObject", function(object,ref_data_object, ...){standardGeneric(".setOutputObject")})
}
# Returns generated GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   ggplot : ggplot object

setMethod(".setOutputObject",  
          signature(object = "VirtualAnalysisBlock", ref_data_object = "VirtualReferenceData" ),
          function(object, ref_data_object){
            object@output <- ref_data_object
            return(object)
          }
)

if (!isGenericS4("getOutputGGPlot")) {
  setGeneric("getOutputGGPlot", function(object,...){standardGeneric("getOutputGGPlot")})
}
# Returns generated GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   ggplot : ggplot object

setMethod("getOutputGGPlot",  
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@ggplot)
          }
)

if (!isGenericS4(".setOutputGGPlot")) {
  setGeneric(".setOutputGGPlot", function(object,ggplot, ...){standardGeneric(".setOutputGGPlot")})
}
# Private method to set ggplot slot with generated GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualAnalysisBlock"

setMethod(".setOutputGGPlot",  
          signature(object = "VirtualAnalysisBlock", ggplot = "ANY"),
          function(object, ggplot){
            
            object <- .checkSlotClass(object, "ggplot", class(ggplot), c("ggplot", "grob"))
            object@ggplot <- ggplot
            return(object)
          }
)

if (!isGenericS4("getOutputGGPlotData")) {
  setGeneric("getOutputGGPlotData", function(object,...){standardGeneric("getOutputGGPlotData")})
}
# Returns data for generated GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   ggplot_data : ggplot_data data.frame

setMethod("getOutputGGPlotData",  
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@ggplot_data)
          }
)

if (!isGenericS4(".setOutputGGPlotData")) {
  setGeneric(".setOutputGGPlotData", function(object, ggplot_data,...){standardGeneric(".setOutputGGPlotData")})
}
# Private method to set ggplot_data slot with data.frame used to generate GGPlot object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualAnalysisBlock"

setMethod(".setOutputGGPlotData",  
          signature(object = "VirtualAnalysisBlock", ggplot_data = "data.frame"),
          function(object, ggplot_data){
            object@ggplot_data <- ggplot_data
            return(object)
          }
)

if (!isGenericS4("getOutputFrontendData")) {
  setGeneric("getOutputFrontendData", function(object,...){standardGeneric("getOutputFrontendData")})
}
# Returns data for generated front end services
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   frontend_data : frontend_data data.frame

setMethod("getOutputFrontendData",  
          signature(object = "VirtualAnalysisBlock"),
          function(object){
            return(object@frontend_data)
          }
)

if (!isGenericS4(".setOutputFrontendData")) {
  setGeneric(".setOutputFrontendData", function(object, frontend_data, ...){standardGeneric(".setOutputFrontendData")})
}
# Private method to set frontend_data slot with data.frame used to provide to frontend services object
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualAnalysisBlock"

setMethod(".setOutputFrontendData",  
          signature(object = "VirtualAnalysisBlock", frontend_data = "data.frame"),
          function(object, frontend_data){
            object@frontend_data <- frontend_data
            return(object)
          }
)

if (!isGenericS4("Process")) {
  setGeneric("Process", function(object, ...){standardGeneric("Process")})
}
# Trigger Computation of analysis
#
# Args:
#   object : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualAnalysisBlock"
