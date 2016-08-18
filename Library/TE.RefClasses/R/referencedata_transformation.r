#' @include referenceobject.r
#' @include virtual_transformation.r
#' @include transformation_functions.r
NULL

#' @exportClass ReferenceDataTransformationData
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
            .Object <- setReferenceDataObject(.Object, ref_data)
          }

          return(.Object)
})


#' Get stored VirtualReferenceData Object
#'
#' Returns VirtualReferenceData object on which transformation is beeing done
#'
#' @param object object of class 'VirtualReferenceDataTransformation'.
#' @return \code{ref_data} object of class 'VirtualReferenceData'.
#' @export

setGeneric("getReferenceDataObject",function(object,...){standardGeneric("getReferenceDataObject")})
setMethod("getReferenceDataObject",
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
#' @export

setGeneric("setReferenceDataObject",function(object, ref_data, ...){standardGeneric("setReferenceDataObject")})
setMethod("setReferenceDataObject",
          signature(object = "VirtualReferenceDataTransformation",
                    ref_data = "VirtualReferenceData"),
          function(object, ref_data){
            if (!is(ref_data, "VirtualReferenceData")){
              message(paste("Error when calling setReferenceDataObject() on ", class(object)[[1]], "class."))
              message(paste("Missing or invalid ref_data argument."))
              message(paste("VirtualReferenceDataTransformation and derived classes only accept \"ref_data\""))
              message(paste("argument of class extending VirtualReferenceData class."))
              stop("Invalid setReferenceDataObject(object, ref_data) argument \"ref_data\"")
            }

            object <- setComputationInput(object, ref_data)
            object@ref_data <- ref_data

            return(object)
          }
)

