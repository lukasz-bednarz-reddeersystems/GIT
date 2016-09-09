#' @include implied_factor_returns_data.r
NULL

###############################################
#
# VirtualImpliedFactorReturnsDataHandler Class
#
##############################################

#' Virtual S4 class handling VirtualImpliedFactorReturnsData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualImpliedFactorReturnsData or derived classes
#'
#' @slot implied_factor_returns object of class "VirtualImpliedFactorReturnsData"

setClass(
  Class          = "VirtualImpliedFactorReturnsDataHandler",
  slots = c(
    implied_factor_returns = "VirtualImpliedFactorReturnsData"
  ),
  contains = c("VIRTUAL")
)


#' Get implied_factor_returns stored in object
#'
#' Returns implied_factor_returns object of class "VirtualImpliedFactorReturnsData"
#'
#' @param object object of class "VirtualImpliedFactorReturnsDataHandler"
#' @return \code{implied_factor_returns} object of class "VirtualImpliedFactorReturnsData"
#' @export

setGeneric("getImpliedFactorReturnsDataObject", function(object){standardGeneric("getImpliedFactorReturnsDataObject")})

#' @describeIn getImpliedFactorReturnsDataObject
#' Get implied_factor_returns stored in object
#'
#' Returns implied_factor_returns object of class "VirtualImpliedFactorReturnsData"
#'
#' @inheritParams getImpliedFactorReturnsDataObject
#' @return \code{implied_factor_returns} object of class "VirtualImpliedFactorReturnsData"
#' @export
setMethod("getImpliedFactorReturnsDataObject",
          signature(object = "VirtualImpliedFactorReturnsDataHandler"),
          function(object){
            return(object@implied_factor_returns)
          }
)

#' Set implied_factor_returns object in object slot
#'
#' Public method to set implied_factor_returns slot with "VirtualImpliedFactorReturnsData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualImpliedFactorReturnsDataHandler"
#' @param implied_factor_returns object of class "VirtualImpliedFactorReturnsData"
#' @return \code{object} object of class "VirtualImpliedFactorReturnsDataHandler"
#' @export

setGeneric("setImpliedFactorReturnsDataObject", function(object,implied_factor_returns){standardGeneric("setImpliedFactorReturnsDataObject")})


#' Set implied_factor_returns object in object slot
#'
#' Private method to set implied_factor_returns slot with "VirtualImpliedFactorReturnsData"
#'
#' @param object object of class "VirtualImpliedFactorReturnsDataHandler"
#' @param implied_factor_returns object of class "VirtualImpliedFactorReturnsData"
#' @return \code{object} object of class "VirtualImpliedFactorReturnsDataHandler"

setGeneric(".setImpliedFactorReturnsDataObject", function(object,implied_factor_returns){standardGeneric(".setImpliedFactorReturnsDataObject")})

setMethod(".setImpliedFactorReturnsDataObject",
          signature(object = "VirtualImpliedFactorReturnsDataHandler", implied_factor_returns = "VirtualImpliedFactorReturnsData"),
          function(object, implied_factor_returns){
            object@implied_factor_returns <- implied_factor_returns
            return(object)
          }
)


#' Update implied_factor_returns risk model
#'
#' Trigger update of risk model of implied_factor_returns object
#'
#' @param object object of class "VirtualImpliedFactorReturnsDataHandler"
#' @param risk_model object of class "VirtualRiskModel"
#' @export

setGeneric("updateImpliedFactorReturnsDataRiskModel",
           function(object, risk_model){standardGeneric("updateImpliedFactorReturnsDataRiskModel")})


#' @describeIn updateImpliedFactorReturnsDataRiskModel
#' Update implied_factor_returns risk model
#'
#' Trigger update of risk model of implied_factor_returns object
#'
#' @inheritParams updateImpliedFactorReturnsDataRiskModel
#' @return \code{implied_factor_returns} object of class "VirtualImpliedFactorReturnsData"
#' @export
setMethod("updateImpliedFactorReturnsDataRiskModel",
          signature(object = "VirtualImpliedFactorReturnsDataHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            implied_factor_returns <- getImpliedFactorReturnsDataObject(object)
            implied_factor_returns <- setRiskModelObject(implied_factor_returns, risk_model)
            object <- .setImpliedFactorReturnsDataObject(object, implied_factor_returns)
            return(object)
          }
)
