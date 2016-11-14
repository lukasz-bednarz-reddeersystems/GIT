#' @include TE.Report.r
NULL

####################################
#
# VirtualReportAnalysisBlock Class
#
####################################

#' Virtual S4 class implementing handling of analysis objects.
#'
#' Class that is to be inherited by any objects
#' that will encapsulate specific analysis computations.
#'
#' Each analysis module will generate an entry to the list
#' with respective class name.
#'
#' Inherits from "VirtualDataSourceClient"
#'
#' @slot ggplot_list list of objects of class "ggplot" or "grob"
#' @slot ggplot_data_list list of "data.frame"'s with data for ggplots
#' @slot frontend_data list of "data.frame"'s with data for ggplots
#' @slot output list of output from analysis classes of class derived from "VirtualReferenceData"

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



#' check if slot class is as required
#'
#' Private method to check if given value has correct class for the slot
#'
#' @rdname private_checkSlotClass
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param slot "character" slot name which class is beeing checked
#' @param class "character" class name of the slot
#' @param req_class "character" vector of allowed slot classes
#' @return \code{object} object of class "VirtualReportAnalysisBlock"

setGeneric(".checkSlotClass", function(object,slot, class, req_class){standardGeneric(".checkSlotClass")})

setMethod(".checkSlotClass",
          signature(object = "VirtualReportAnalysisBlock", slot = "character", class = "character", req_class = "character"),
          function(object,slot, class, req_class){
            if (!any(class %in% req_class))  {
              message(paste("Invalid class", class, "."))
              message(paste("in attempt to set", sQuote(slot), "slot in object of class", class(object) ))
              stop(paste("assignment of an object of class",
                         dQuote(head(class, 1)),
                                "is not valid for @", sQuote(slot), " in an object of class",
                                dQuote(class(object))
                         )
                   )
            } else {
              return(object)
            }


          }
)


#' Returns generated Output Reference Data objects list
#'
#' Returns Reference Data Objects list that possibly was
#' computed by analysis objects. If embeded Analysis modules
#' don't compute any output this slot will be NULL
#'
#' @param object object of class "VirtualReportAnalysisBlock"
#'
#' @export

setGeneric("getOutputObjectList", function(object){standardGeneric("getOutputObjectList")})

#' @describeIn getOutputObjectList
#' Returns generated output object
#'
#' Returns Reference Data Objects list that possibly was
#' computed by analysis objects. If embeded Analysis modules
#' don't compute any output this slot will be NULL
#'
#' @inheritParams getOutputObjectList
#' @return \code{output_list} list of objects of class derived from "VirtualReferenceData"
#'
#' @export

setMethod("getOutputObjectList",
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@output_list)
          }
)


#' Set internaly output object list
#'
#' Private method to set output object list slot
#'
#' @rdname private_setOutputObjectList
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param list list of objects of class derived from "VirtualReferenceData"
#' @return \code{object} object object of class "VirtualReportAnalysisBlock"

setGeneric(".setOutputObjectList", function(object, list){standardGeneric(".setOutputObjectList")})

setMethod(".setOutputObjectList",
          signature(object = "VirtualReportAnalysisBlock", list = "list" ),
          function(object, list) {

            for (rd_obj in list) {
              object <- .checkSlotClass(object,
                                        "OutputObjectList",
                                        extends(class(rd_obj)),
                                        c("NULL", "VirtualReferenceData"))
            }

            object@output_list <- list
            return(object)
          }
)


#' Returns generated output ggplot list
#'
#' Returns list of ggplot or grob objects generated
#' as a result of analysis computation.
#'
#' @param object object of class "VirtualReportAnalysisBlock"
#'
#' @export

setGeneric("getOutputGGPlotList", function(object){standardGeneric("getOutputGGPlotList")})

#' @describeIn getOutputGGPlotList
#' Returns generated output ggplot list
#'
#' Returns list of ggplot or grob objects generated as a result of analysis computation.
#'
#' @inheritParams getOutputGGPlotList
#' @return \code{ggplot_list} list of objects of class "ggplot" or "grob"
#'
#' @export

setMethod("getOutputGGPlotList",
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@ggplot_list)
          }
)


#' Set internaly output ggplot object list
#'
#' Private method to set ggplot slot with generated GGPlot object list
#'
#' @rdname private_setOutputGGPlotList
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param list list with objects of class "ggplot"
#' @return \code{object} object object of class "VirtualReportAnalysisBlock"

setGeneric(".setOutputGGPlotList", function(object, list){standardGeneric(".setOutputGGPlotList")})

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



#' Returns list of generated output ggplot data.frame's
#'
#' Returns list of "data.frame"'s with data used to generate ggplots
#'
#' @param object object of class "VirtualReportAnalysisBlock"
#'
#' @export

setGeneric("getOutputGGPlotDataList", function(object){standardGeneric("getOutputGGPlotDataList")})

#' @describeIn getOutputGGPlotDataList
#' Returns list of generated output ggplot data.frame's
#'
#' Returns list of "data.frame"'s with data used to generate ggplots
#'
#' @inheritParams getOutputGGPlotDataList
#' @return \code{ggplot_data_list} list of objects of class "data.frame"
#'
#' @export

setMethod("getOutputGGPlotDataList",
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@ggplot_data_list)
          }
)


#' Set internaly list of data for ggplot objects
#'
#' Private method to set slot with list of data
#' used to generate ggplot object.
#'
#' @rdname private_setOutputGGPlotDataList
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param list list of objects of class "data.frame"
#' @return object object of class "VirtualReportAnalysisBlock"
setGeneric(".setOutputGGPlotDataList", function(object, list){standardGeneric(".setOutputGGPlotDataList")})

setMethod(".setOutputGGPlotDataList",
          signature(object = "VirtualReportAnalysisBlock", list = "list"),
          function(object, list){
            for (df in list) {
              object <- .checkSlotClass(object, "OutputGGPlotDataList", class(df), c("data.frame", "list"))
            }
            object@ggplot_data_list <- list
            return(object)
          }
)



#' Returns generated output frontend data list
#'
#' Returns list of data to be used by any frontend using the module
#'
#' @param object object of class "VirtualReportAnalysisBlock"
#'
#' @export

setGeneric("getOutputFrontendDataList", function(object){standardGeneric("getOutputFrontendDataList")})

#' @describeIn getOutputFrontendDataList
#' Returns generated output frontend data list
#'
#' Returns list of data to be used by any frontend using the module
#'
#' @inheritParams getOutputFrontendDataList
#' @return \code{frontend_data_list} list of objects of class "data.frame"
#'
#' @export

setMethod("getOutputFrontendDataList",
          signature(object = "VirtualReportAnalysisBlock"),
          function(object){
            return(object@frontend_data_list)
          }
)


#' Set internaly list of data for ggplot objects
#'
#' Private method to set frontend_data_list slot
#' with list of data.frame's used to provide to
#' frontend services object
#'
#' @rdname private_setOutputFrontendData
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param list list of objects of class "data.frame"
#' @return \code{object} object object of class "VirtualReportAnalysisBlock"
setGeneric(".setOutputFrontendDataList", function(object, list){standardGeneric(".setOutputFrontendDataList")})

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



#' Copy output of analyzer to internal list
#'
#' Private method to copy analyzer output and deal to internal lists
#'
#' @rdname private_copyAnalyzerOutputData
#' @param object object of class "VirtualReportAnalysisBlock"
#' @param analyzer object of type "VirtualAnalysisBlock"
#' @return \code{object} object object of class "VirtualReportAnalysisBlock"
setGeneric(".copyAnalyzerOutputData", function(object, analyzer){standardGeneric(".copyAnalyzerOutputData")})

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

