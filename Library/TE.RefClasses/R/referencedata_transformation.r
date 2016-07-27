#' @include referenceobject.r
#' @include virtual_transformation.r
#' @include transformation_functions.r
NULL

setClassUnion("ReferenceDataTransformationData",c("VirtualReferenceData","NULL"))

################################################
#
# VirtualReferenceDataTransformation Class
#
################################################

#' Virtual S4 class implementing reference data transformations
#'
#' This is base class handling Tranformation
#' on VirtualReferenceData object
#'
#' @slot ref_data      "VirtualReferenceData"

setClass(
  Class          = "VirtualReferenceDataTransformation",
  slots = c(
    ref_data     = "ReferenceDataTransformationData"
  ),
  prototype = prototype(
    required_colnms = c('Date','InstrumentID','Weight')
  ),
  contains = c("VirtualTransformation", "VIRTUAL")
)

setMethod("initialize",
          signature(.Object = "VirtualReferenceDataTransformation"),
          function(.Object, ref_data ){
          if (!is(ref_data, "VirtualReferenceData") && !is(ref_data,"NULL")){
            message(paste("Error when initializing", class(.Object)[[1]], "class."))
            message(paste("Missing or invalid ref_data argument."))
            message(paste("VirtualReferenceDataTransformation and derived classes only accept \"ref_data\""))
            message(paste("argument extending VirtualReferenceData class."))
            stop("Invalid Class initialize() argument \"ref_data\"")
          }
          if (is(ref_data, "VirtualReferenceData")) {
            .Object <- setReferenceData(.Object, ref_data)
          }

          return(.Object)
})


#' Get stored VirtualReferenceData Object
#'
#' Returns VirtualReferenceData object on which transformation is beeing done
#'
#' @param object object of class 'VirtualReferenceDataTransformation'.
#' @return \code{ref_data} object of class 'VirtualReferenceData'.

setGeneric("getReferenceData",function(object,...){standardGeneric("getReferenceData")})
setMethod("getReferenceData",
          signature(object = "VirtualReferenceDataTransformation"),
          function(object){

            return(object@ref_data)
          }
)


#' Set VirtualReferenceData Object
#'
#' Sets VirtualReferenceData object on which transformation is beeing done
#'
#' @param object object of class 'VirtualReferenceDataTransformation'.
#' @param ref_data object of class 'VirtualReferenceData'.
#' @return \code{ref_data} object of class 'VirtualReferenceDataTransformation'.

setGeneric("setReferenceData",function(object, ref_data, ...){standardGeneric("setReferenceData")})
setMethod("setReferenceData",
          signature(object = "VirtualReferenceDataTransformation",
                    ref_data = "VirtualReferenceData"),
          function(object, ref_data){
            if (!is(ref_data, "VirtualReferenceData")){
              message(paste("Error when calling setReferenceData() on ", class(object)[[1]], "class."))
              message(paste("Missing or invalid ref_data argument."))
              message(paste("VirtualReferenceDataTransformation and derived classes only accept \"ref_data\""))
              message(paste("argument of class extending VirtualReferenceData class."))
              stop("Invalid setReferenceData(object, ref_data) argument \"ref_data\"")
            }

            object <- setComputationInput(object, ref_data)
            object@ref_data <- ref_data

            return(object)
          }
)

################################################
#
# DaysSinceLastFlatTransformationn Class
#
################################################

#' computes number of days since last flat
#'
#' Adds column with number of days since last flat
#' for each position.
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "DaysSinceLastFlatTransformationComputation",
  prototype      = list(
    required_colnms= c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    compute = days_since_last_flat

  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes number of days since last flat
#'
#' Adds column with number of days since last flat
#' for each position
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "DaysSinceLastFlatTransformationComputation"
#' @export
setClass(
  Class          = "DaysSinceLastFlatTransformation",
  prototype = prototype(
    required_colnms = c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    computation = new("DaysSinceLastFlatTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
)
