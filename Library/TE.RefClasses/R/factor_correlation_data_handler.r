#' @include factor_correlation_data.r
NULL


###########################################
#
# VirtualFactorCorrelationDataHandler Class
#
###########################################

#' Virtual S4 class handling VirtualFactorCorrelationData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualFactorCorrelationData or derived classes
#'
#' @slot factor_correlation object of class "VirtualFactorCorrelationData"

setClass(
  Class          = "VirtualFactorCorrelationDataHandler",
  slots = c(
    factor_correlation = "VirtualFactorCorrelationData"
  ),
  contains = c("VIRTUAL")
)

#' Get factor_correlation data stored in object
#'
#' Returns factor_correlation object of class "VirtualFactorCorrelationData"
#'
#' @param object object of class "VirtualFactorCorrelationDataHandler"
#' @export

setGeneric("getFactorCorrelationDataObject", function(object){standardGeneric("getFactorCorrelationDataObject")})

#' @describeIn getFactorCorrelationDataObject
#' Get factor_correlation data stored in object
#'
#' Returns factor_correlation object of class "VirtualFactorCorrelationData"
#'
#' @inheritParams getFactorCorrelationDataObject
#' @return \code{factor_correlation} object of class "VirtualFactorCorrelationData"
#' @export
setMethod("getFactorCorrelationDataObject",
          signature(object = "VirtualFactorCorrelationDataHandler"),
          function(object){
            return(object@factor_correlation)
          }
)

#' Set factor_correlation data object in object slot
#'
#' Public method to set factor_correlation slot with "VirtualFactorCorrelationData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualFactorCorrelationDataHandler"
#' @param factor_correlation object of class "VirtualFactorCorrelationData"
#' @export

setGeneric("setFactorCorrelationDataObject", function(object,factor_correlation){standardGeneric("setFactorCorrelationDataObject")})


#' Set factor_correlation data object in object slot
#'
#' Private method to set factor_correlation slot with "VirtualFactorCorrelationData"
#'
#' @rdname private_setFactorCorrelationDataObject
#' @param object object of class "VirtualFactorCorrelationDataHandler"
#' @param factor_correlation object of class "VirtualFactorCorrelationData"
#' @return \code{object} object of class "VirtualFactorCorrelationDataHandler"

setGeneric(".setFactorCorrelationDataObject", function(object,factor_correlation){standardGeneric(".setFactorCorrelationDataObject")})

setMethod(".setFactorCorrelationDataObject",
          signature(object = "VirtualFactorCorrelationDataHandler", factor_correlation = "VirtualFactorCorrelationData"),
          function(object, factor_correlation){
            object@factor_correlation <- factor_correlation
            return(object)
          }
)

#' Update factor_correlation risk model
#'
#' Trigger update of risk model of factor_correlation object
#'
#' @param object object of class "VirtualFactorCorrelationDataHandler"
#' @param risk_model object of class "VirtualRiskModel"
#' @export

setGeneric("updateFactorCorrelationDataRiskModel",
           function(object, risk_model){standardGeneric("updateFactorCorrelationDataRiskModel")})


#' @describeIn updateFactorCorrelationDataRiskModel
#' Update factor_correlation risk model
#'
#' Trigger update of risk model of factor_correlation object
#'
#' @inheritParams updateFactorCorrelationDataRiskModel
#' @return \code{factor_correlation} object of class "VirtualFactorCorrelationData"
#' @export
setMethod("updateFactorCorrelationDataRiskModel",
          signature(object = "VirtualFactorCorrelationDataHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            factor_correlation <- getFactorCorrelationDataObject(object)
            factor_correlation <- setRiskModelObject(factor_correlation, risk_model)
            object <- .setFactorCorrelationDataObject(object, factor_correlation)
            return(object)
          }
)

