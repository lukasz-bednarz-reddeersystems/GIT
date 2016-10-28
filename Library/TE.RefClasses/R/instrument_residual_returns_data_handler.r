#' @include instrument_residual_returns_data.r
NULL

#####################################
#
# VirtualInstrumentResidualReturnsDataHandler Class
#
#####################################

#' Virtual S4 class handling VirtualInstrumentResidualReturnsData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualInstrumentResidualReturnsData or derived classes
#'
#' @slot instrument_residual_returns object of class "VirtualInstrumentResidualReturnsData"

setClass(
  Class          = "VirtualInstrumentResidualReturnsDataHandler",
  slots = c(
    instrument_residual_returns = "VirtualInstrumentResidualReturnsData"
  ),
  contains = c("VIRTUAL")
)


#' Get instrument_residual_returns stored in object
#'
#' Returns instrument_residual_returns object of class "VirtualInstrumentResidualReturnsData"
#'
#' @param object object of class "VirtualInstrumentResidualReturnsDataHandler"
#' @return \code{instrument_residual_returns} object of class "VirtualInstrumentResidualReturnsData"
#' @export

setGeneric("getInstrumentResidualReturnsDataObject", function(object){standardGeneric("getInstrumentResidualReturnsDataObject")})

#' @describeIn getInstrumentResidualReturnsDataObject
#' Get instrument_residual_returns stored in object
#'
#' Returns instrument_residual_returns object of class "VirtualInstrumentResidualReturnsData"
#'
#' @inheritParams getInstrumentResidualReturnsDataObject
#' @return \code{instrument_residual_returns} object of class "VirtualInstrumentResidualReturnsData"
#' @export
setMethod("getInstrumentResidualReturnsDataObject",
          signature(object = "VirtualInstrumentResidualReturnsDataHandler"),
          function(object){
            return(object@instrument_residual_returns)
          }
)


#' Set instrument_residual_returns object in object slot
#'
#' Public method to set instrument_residual_returns slot with "VirtualInstrumentResidualReturnsData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualInstrumentResidualReturnsDataHandler"
#' @param instrument_residual_returns object of class "VirtualInstrumentResidualReturnsData"
#' @return \code{object} object of class "VirtualInstrumentResidualReturnsDataHandler"
#' @export

setGeneric("setInstrumentResidualReturnsDataObject", function(object,instrument_residual_returns){standardGeneric("setInstrumentResidualReturnsDataObject")})



#' Set instrument_residual_returns object in object slot
#'
#' Private method to set instrument_residual_returns slot with "VirtualInstrumentResidualReturnsData"
#'
#' @rdname private_setInstrumentResidualReturnsDataObject
#' @param object object of class "VirtualInstrumentResidualReturnsDataHandler"
#' @param instrument_residual_returns object of class "VirtualInstrumentResidualReturnsData"
#' @return \code{object} object of class "VirtualInstrumentResidualReturnsDataHandler"

setGeneric(".setInstrumentResidualReturnsDataObject", function(object,instrument_residual_returns){standardGeneric(".setInstrumentResidualReturnsDataObject")})

setMethod(".setInstrumentResidualReturnsDataObject",
          signature(object = "VirtualInstrumentResidualReturnsDataHandler",
                    instrument_residual_returns = "VirtualInstrumentResidualReturnsData"),
          function(object, instrument_residual_returns){
              object@instrument_residual_returns <- instrument_residual_returns
            return(object)
          }
)


#' Update instrument_residual_returns risk model
#'
#' Trigger update of risk model of instrument_residual_returns object
#'
#' @param object object of class "VirtualInstrumentResidualReturnsDataHandler"
#' @param risk_model object of class "VirtualRiskModel"
#' @export

setGeneric("updateInstrumentResidualReturnsDataRiskModel",
           function(object, risk_model){standardGeneric("updateInstrumentResidualReturnsDataRiskModel")})


#' @describeIn updateInstrumentResidualReturnsDataRiskModel
#' Update instrument_residual_returns risk model
#'
#' Trigger update of risk model of instrument_residual_returns object
#'
#' @inheritParams updateInstrumentResidualReturnsDataRiskModel
#' @return \code{instrument_residual_returns} object of class "VirtualInstrumentResidualReturnsData"
#' @export
setMethod("updateInstrumentResidualReturnsDataRiskModel",
          signature(object = "VirtualInstrumentResidualReturnsDataHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            instrument_residual_returns <- getInstrumentResidualReturnsDataObject(object)
            instrument_residual_returns <- setRiskModelObject(instrument_residual_returns, risk_model)
            object <- .setInstrumentResidualReturnsDataObject(object, instrument_residual_returns)
            return(object)
          }
)
