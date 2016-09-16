#' @include instrument_betas_data.r
NULL

#####################################
#
# VirtualInstrumentBetasDataHandler Class
#
#####################################

#' Virtual S4 class handling VirtualInstrumentBetasData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualInstrumentBetasData or derived classes
#'
#' @slot instrument_betas object of class "VirtualInstrumentBetasData"

setClass(
  Class          = "VirtualInstrumentBetasDataHandler",
  slots = c(
    instrument_betas = "VirtualInstrumentBetasData"
  ),
  contains = c("VIRTUAL")
)


#' Get instrument_betas stored in object
#'
#' Returns instrument_betas object of class "VirtualInstrumentBetasData"
#'
#' @param object object of class "VirtualInstrumentBetasDataHandler"
#' @return \code{instrument_betas} object of class "VirtualInstrumentBetasData"
#' @export

setGeneric("getInstrumentBetasDataObject", function(object){standardGeneric("getInstrumentBetasDataObject")})

#' @describeIn getInstrumentBetasDataObject
#' Get instrument_betas stored in object
#'
#' Returns instrument_betas object of class "VirtualInstrumentBetasData"
#'
#' @inheritParams getInstrumentBetasDataObject
#' @return \code{instrument_betas} object of class "VirtualInstrumentBetasData"
#' @export
setMethod("getInstrumentBetasDataObject",
          signature(object = "VirtualInstrumentBetasDataHandler"),
          function(object){
            return(object@instrument_betas)
          }
)


#' Set instrument_betas object in object slot
#'
#' Public method to set instrument_betas slot with "VirtualInstrumentBetasData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualInstrumentBetasDataHandler"
#' @param instrument_betas object of class "VirtualInstrumentBetasData"
#' @return \code{object} object of class "VirtualInstrumentBetasDataHandler"
#' @export

setGeneric("setInstrumentBetasDataObject", function(object,instrument_betas){standardGeneric("setInstrumentBetasDataObject")})



#' Set instrument_betas object in object slot
#'
#' Private method to set instrument_betas slot with "VirtualInstrumentBetasData"
#'
#' @rdname private_setInstrumentBetasDataObject
#' @param object object of class "VirtualInstrumentBetasDataHandler"
#' @param instrument_betas object of class "VirtualInstrumentBetasData"
#' @return \code{object} object of class "VirtualInstrumentBetasDataHandler"

setGeneric(".setInstrumentBetasDataObject", function(object,instrument_betas){standardGeneric(".setInstrumentBetasDataObject")})

setMethod(".setInstrumentBetasDataObject",
          signature(object = "VirtualInstrumentBetasDataHandler", instrument_betas = "VirtualInstrumentBetasData"),
          function(object, instrument_betas){
              object@instrument_betas <- instrument_betas
            return(object)
          }
)


#' Update instrument_betas risk model
#'
#' Trigger update of risk model of instrument_betas object
#'
#' @param object object of class "VirtualInstrumentBetasDataHandler"
#' @param risk_model object of class "VirtualRiskModel"
#' @export

setGeneric("updateInstrumentBetasDataRiskModel",
           function(object, risk_model){standardGeneric("updateInstrumentBetasDataRiskModel")})


#' @describeIn updateInstrumentBetasDataRiskModel
#' Update instrument_betas risk model
#'
#' Trigger update of risk model of instrument_betas object
#'
#' @inheritParams updateInstrumentBetasDataRiskModel
#' @return \code{instrument_betas} object of class "VirtualInstrumentBetasData"
#' @export
setMethod("updateInstrumentBetasDataRiskModel",
          signature(object = "VirtualInstrumentBetasDataHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            instrument_betas <- getInstrumentBetasDataObject(object)
            instrument_betas <- setRiskModelObject(instrument_betas, risk_model)
            object <- .setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)
