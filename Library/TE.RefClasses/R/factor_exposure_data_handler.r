#' @include factor_exposure_data.r
NULL


########################################
#
# VirtualFactorExposureDataHandler Class
#
########################################

#' Virtual S4 class handling VirtualFactorExposureData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualFactorExposureData or derived classes
#'
#' @slot factor_exposure_data object of class "VirtualFactorExposureData"

setClass(
  Class          = "VirtualFactorExposureDataHandler",
  slots = c(
    factor_exposure_data = "VirtualFactorExposureData"
  ),
  prototype = list(
    factor_exposure_data = new("FactorExposureData")
  ),
  contains = c("VIRTUAL")
)


#' Get factor_exposure_data stored in object
#'
#' Returns factor_exposure_data object of class "VirtualFactorExposureData"
#'
#' @param object object of class "VirtualFactorExposureDataHandler"
#' @return \code{factor_exposure_data} object of class "VirtualFactorExposureData"
#' @export

setGeneric("getFactorExposureDataObject", function(object,...){standardGeneric("getFactorExposureDataObject")})

setMethod("getFactorExposureDataObject",
          signature(object = "VirtualFactorExposureDataHandler"),
          function(object){
            return(object@factor_exposure_data)
          }
)


#' Set event data object in object slot
#'
#' Public method to set factor_exposure_data slot with "VirtualFactorExposureData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualFactorExposureDataHandler"
#' @param factor_exposure_data object of class "VirtualFactorExposureData"
#' @return \code{object} object of class "VirtualFactorExposureDataHandler"
#' @export

setGeneric("setFactorExposureDataObject", function(object,factor_exposure_data, ...){standardGeneric("setFactorExposureDataObject")})


#' Set event data object in object slot
#'
#' Private method to set factor_exposure_data slot with "VirtualFactorExposureData"
#'
#' @param object object of class "VirtualFactorExposureDataHandler"
#' @param factor_exposure_data object of class "VirtualFactorExposureData"
#' @return \code{object} object of class "VirtualFactorExposureDataHandler"

setGeneric(".setFactorExposureDataObject", function(object,factor_exposure_data, ...){standardGeneric(".setFactorExposureDataObject")})

setMethod(".setFactorExposureDataObject",
          signature(object = "VirtualFactorExposureDataHandler", factor_exposure_data = "VirtualFactorExposureData"),
          function(object, factor_exposure_data){
              object@factor_exposure_data <- factor_exposure_data
            return(object)
          }
)
