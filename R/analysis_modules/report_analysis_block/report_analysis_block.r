sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)

####################################
#
# VirtualReportAnalysisBlock Class
#
####################################

setClass(
  Class           = "VirtualReportAnalysisBlock",
  slots = c(
    ggplot_list        = "list",
    ggplot_data_list   = "list",
    frontend_data_list = "list",
    output_list        = "list"
  ),
  prototype       = list(
    ggplot_list        = list(),
    ggplot_data_list   = list(),
    frontend_data_list = list(),
    output_list        = list()
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)



if (!isGenericS4(".checkSlotClass")) {
  setGeneric(".checkSlotClass", function(object,slot, class, req_class, ...){standardGeneric(".checkSlotClass")})
}
# Private method to check if given value has correct class for the slot 
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"

setMethod(".checkSlotClass",  
          signature(object = "VirtualReportAnalysisBlock", slot = "character", class = "character", req_class = "character"),
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


if (!isGenericS4("getOutputObjectList")) {
  setGeneric("getOutputObjectList", function(object,...){standardGeneric("getOutputObjectList")})
}
# Returns generated Output Reference Data objects list
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   output_list : output object list

setMethod("getOutputObjectList",  
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@output_list)
          }
)

if (!isGenericS4(".setOutputObjectList")) {
  setGeneric(".setOutputObjectList", function(object, list, ...){standardGeneric(".setOutputObjectList")})
}
# Returns generated GGPlot object
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   ggplot : ggplot object

setMethod(".setOutputObjectList",  
          signature(object = "VirtualReportAnalysisBlock", list = "list" ),
          function(object, list) {
            
            for (rd_obj in list) {
              object <- .checkSlotClass(object, "OutputObjectList", extends(class(rd_obj)), "NullableReferenceData")
            }
            
            object@output_list <- list
            return(object)
          }
)


if (!isGenericS4("getOutputGGPlotList")) {
  setGeneric("getOutputGGPlotList", function(object,...){standardGeneric("getOutputGGPlotList")})
}
# Returns generated GGPlot object list
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   ggplot : ggplot object list

setMethod("getOutputGGPlotList",  
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@ggplot_list)
          }
)

if (!isGenericS4(".setOutputGGPlotList")) {
  setGeneric(".setOutputGGPlotList", function(object, list, ...){standardGeneric(".setOutputGGPlotList")})
}
# Private method to set ggplot slot with generated GGPlot object list
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"

setMethod(".setOutputGGPlotList",  
          signature(object = "VirtualReportAnalysisBlock", list = "list"),
          function(object, list){
            
            for (ggplot in list) {
              object <- .checkSlotClass(object, "OutputGGPlotList", class(ggplot), c("ggplot","grob"))
            }
            object@ggplot_list <- list
            return(object)
          }
)

if (!isGenericS4("getOutputGGPlotDataList")) {
  setGeneric("getOutputGGPlotDataList", function(object,...){standardGeneric("getOutputGGPlotDataList")})
}
# Returns data for generated GGPlot object
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   ggplot_data : ggplot_data data.frame

setMethod("getOutputGGPlotDataList",  
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@ggplot_data_list)
          }
)

if (!isGenericS4(".setOutputGGPlotDataList")) {
  setGeneric(".setOutputGGPlotDataList", function(object, list,...){standardGeneric(".setOutputGGPlotDataList")})
}
# Private method to set ggplot_data slot with data.frame used to generate GGPlot object
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"

setMethod(".setOutputGGPlotDataList",  
          signature(object = "VirtualReportAnalysisBlock", list = "list"),
          function(object, list){
            for (df in list) {
              object <- .checkSlotClass(object, "OutputGGPlotDataList", class(df), "data.frame")
            }
            object@ggplot_data_list <- list
            return(object)
          }
)

if (!isGenericS4("getOutputFrontendDataList")) {
  setGeneric("getOutputFrontendDataList", function(object,...){standardGeneric("getOutputFrontendDataList")})
}
# Returns data for generated front end services
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   frontend_data : frontend_data data.frame

setMethod("getOutputFrontendDataList",  
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@frontend_data_list)
          }
)

if (!isGenericS4(".setOutputFrontendDataList")) {
  setGeneric(".setOutputFrontendDataList", function(object, list, ...){standardGeneric(".setOutputFrontendDataList")})
}
# Private method to set frontend_data slot with list of  data.frames used to provide to frontend services object
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"

setMethod(".setOutputFrontendDataList",  
          signature(object = "VirtualReportAnalysisBlock", list = "list"),
          function(object, list){
            
            for (df in list) {
              object <- .checkSlotClass(object, "OutputFrontendDataList", class(df), "data.frame")
            }
            
            object@frontend_data_list <- list
            return(object)
          }
)


if (!isGenericS4(".copyAnalyzerOutputData")) {
  setGeneric(".copyAnalyzerOutputData", function(object, analyzer, ...){standardGeneric(".copyAnalyzerOutputData")})
}
# Private method to copy analyzer output and deal to internal lists
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
#   analyzer : object of type "VirtualAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"

setMethod(".copyAnalyzerOutputData",  
          signature(object = "VirtualReportAnalysisBlock", analyzer = "VirtualAnalysisBlock"),
          function(object, analyzer){
            
            ggplot_list <- getOutputGGPlotList(object)
            ggplot_data_list <- getOutputGGPlotDataList(object)
            frontend_data_list <- getOutputFrontendDataList(object)
            output_list <- getOutputObjectList(object)
            
            name <- gsub("AnalysisBlock", "", class(analyzer))
            
            ggplot_list[[name]] <- getOutputGGPlot(analyzer)
            output_list[[name]] <- getOutputObject(analyzer)
            ggplot_data_list[[name]] <- getOutputGGPlotData(analyzer)
            frontend_data_list[[name]] <- getOutputFrontendData(analyzer)
            
            object <- .setOutputGGPlotList(object, ggplot_list)
            object <- .setOutputObjectList(object, output_list)
            object <- .setOutputGGPlotDataList(object, ggplot_data_list)
            object <- .setOutputFrontendDataList(object, frontend_data_list)
            
            return(object)
          }
)



if (!isGenericS4("Process")) {
  setGeneric("Process", function(object, ...){standardGeneric("Process")})
}
# Trigger Computation of analysis
#
# Args:
#   object : object of type "VirtualReportAnalysisBlock"
# Returns:
#   object : object of type "VirtualReportAnalysisBlock"
