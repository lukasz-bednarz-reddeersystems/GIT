#' @include analysis_block.r
NULL


################################################################################
#
# VirtualAggregateAnalysisBlock Class
#
# Virtual generic block to hold data that is constructed by the analysis block client
# on the fly and to handle how that data should be aggregated.
#
###############################################################################

#' Block that holds only ggplot data and ggplot that has been gathered from
#' other blocks and aggregated
#'
#' Inherits from "VirtualAnalysisBlock",
#' @slot ggplot_data_aggregator object of class "function"
#' @slot ggplot_object_modifier object of class "function"
#' @slot ggplot_format_modifier object of class "function"
#' @slot ui_options_modifier object of class "function"
#' @export

setClass(
  Class             = "VirtualAggregateAnalysisBlock",
  slots = c(ggplot_data_aggregator = "function",
            ggplot_object_modifier = "function",
            ggplot_format_modifier = "function",
            ui_options_modifier = "function"),
  contains          = c("VirtualAnalysisBlock","VIRTUAL")
)

#' Request data from data source: Overriden to have no function because
#' this does not make sense in the context of aggregate blocks
#'
#' @param object object of class "VirtualAggregateAnalysisBlock".
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualAggregateAnalysisBlock'.
#' @rdname dataRequest-VirtualAggregate-method
#' @export
setMethod("dataRequest",
          signature(object = "VirtualAggregateAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            key_values
            message("dataRequest is not implemented for AggregateAnalysis blocks")
            return(object)
          }
)

#' Trigger computation of analysis data: Overridden to have not function because
#' this does not make sense in the context of aggregate blocks.
#'
#' @param object object of class "VirtualAggregateAnalysisBlock"
#' @return \code{object} object object of class "VirtualAggregateAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "VirtualAggregateAnalysisBlock"),
          function(object){
            message("Process is not implemented for AggregateAnalysis blocks")
            return(object)
          }
)


#' Aggregate data into the block
#'
#' @param object object of class "VirtualAggregateAnalysisBlock"
#' @param data data of class "data.frame"
#' @param with_id with_id of class "data.frame"
#' @export
setGeneric("aggregateAnalysisData", function(object,data,with_id){standardGeneric("aggregateAnalysisData")})

#' @describeIn aggregateAnalysisData
#' Aggregate data into the block
#' @inheritParams aggregateAnalysisData
#'
#' @return \code{object} object of class "VirtualAggregateAnalysisBlock"
#' @export
setMethod("aggregateAnalysisData",
          signature(object = "VirtualAggregateAnalysisBlock", data = "data.frame", with_id = "data.frame"),
          function(object,data,with_id){
            if(length(object@ggplot_data)==0){
              object@ggplot_data <- cbind(with_id,data)
            } else {
              object <- tryCatch({
                              object@ggplot_data_aggregator(object,cbind(with_id,data))
                         }, error = function(cond){
                              message(paste("Aggregate analysis data failed:",cond))
                              return(object)
                         })
            }
            return(object)
          }
)

#' Modify the plot object: i.e. reassign to fields internal to
#' the ggplot
#'
#' @param object object of class "VirtualAggregateAnalysisBlock"
#' @export

setGeneric("modifyGGPlotObject", function(object){standardGeneric("modifyGGPlotObject")})

#' @describeIn modifyGGPlotObject
#' Modify the plot object: i.e. reassign to fields internal to
#' the ggplot
#'
#' @inheritParams modifyGGPlotObject
#' @return \code{object} object of class "VirtualAggregateAnalysisBlock"
#' @export
setMethod("modifyGGPlotObject",
          signature(object = "VirtualAggregateAnalysisBlock"),
          function(object){
            if(length(object@ggplot)>0){
              object <- tryCatch({
                object@ggplot_object_modifier(object)
              }, error = function(cond){
                message(paste("ggplot object modification failed:",cond))
                return(object)
              })
            }
            return(object)
          }
)

#' Modify the plot format: i.e. add gg functions the plot stream
#'
#' @param object object of class "VirtualAggregateAnalysisBlock"
#' @export

setGeneric("modifyGGPlotFormat", function(object){standardGeneric("modifyGGPlotFormat")})

#' @describeIn modifyGGPlotFormat
#' Modify the plot format: i.e. add gg functions the plot stream
#'
#' @inheritParams modifyGGPlotFormat
#' @return \code{object} object of class "VirtualAggregateAnalysisBlock"
#' @export
setMethod("modifyGGPlotFormat",
          signature(object = "VirtualAggregateAnalysisBlock"),
          function(object){
            if(length(object@ggplot)>0){
              object <- tryCatch({
                object@ggplot_format_modifier(object)
              }, error = function(cond){
                message(paste("ggplot format modification failed:",cond))
                return(object)
              })
            }
            return(object)
          }
)

#' Modify the ui options list
#'
#' @param object object of class "VirtualAggregateAnalysisBlock"
#' @export

setGeneric("modifyUIOptions", function(object){standardGeneric("modifyUIOptions")})

#' @describeIn modifyUIOptions
#' Modify the ui options list
#'
#' @inheritParams modifyUIOptions
#' @return \code{object} object of class "VirtualAggregateAnalysisBlock"
#' @export
setMethod("modifyUIOptions",
          signature(object = "VirtualAggregateAnalysisBlock"),
          function(object){
            if(length(object@frontend_data)>0){
              object <- tryCatch({
                object@ui_options_modifier(object)
              }, error = function(cond){
                message(paste("ui_options modification failed:",cond))
                return(object)
              })
            }
            return(object)
          }
)

################################################################################
#
# AggregateAnalysisBlock Class
#
# Concrete generic block to hold data that is constructed by the analysis block client
# on the fly and to handle how that data should be aggregated.
#
###############################################################################

#' Block that holds only ggplot data and ggplot that has been gathered from
#' other blocks and aggregated
#' This is the defaul concrete version that can be envoked if there is no
#' specific aggregator for a given analysis block class.
#'
#' Inherits from "VirtualAggregateAnalysisBlock",

#' @export

setClass(
  Class             = "AggregateAnalysisBlock",
  prototype         = list(
    ggplot_data_aggregator = function(object,data){
      object@ggplot_data <- rbind(object@ggplot_data,data)
      return(object)},
    ggplot_object_modifier = function(object){
      object@ggplot$data <- object@ggplot_data
      return(object)},
    ggplot_format_modifier = function(object){
      return(object)},
    ui_options_modifier    = function(object){
      return(object)}
  ),
  contains                 = c("VirtualAggregateAnalysisBlock")
)

