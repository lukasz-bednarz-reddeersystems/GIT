#' @include trade_data.r
NULL

#######################################
#
# VirtualExtendedTradeDataHandler Class
#
#######################################

#' Virtual S4 class handling VirtualExtendedTradeData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualExtendedTradeData or derived classes
#'
#' @slot ex_trade_data object of class "VirtualExtendedTradeData"

setClass(
  Class          = "VirtualExtendedTradeDataHandler",
  slots = c(
    ex_trade_data = "VirtualTradeData"
  ),
  prototype = list(
    ex_trade_data = new("ExtendedTradeData")
  ),
  contains = c("VIRTUAL")
)

#' Get ex_trade data stored in object
#'
#' Returns ex_trade_data object of class "VirtualExtendedTradeData"
#'
#' @param object object of class "VirtualExtendedTradeDataHandler"
#' @return \code{ex_trade_data} object of class "VirtualExtendedTradeData"
#' @export

setGeneric("getExtendedTradeDataObject", function(object){standardGeneric("getExtendedTradeDataObject")})

#' @describeIn  getExtendedTradeDataObject
#' Get ex_trade data stored in object
#'
#' Returns ex_trade_data object of class "VirtualExtendedTradeData"
#'
#' @inheritParams getExtendedTradeDataObject
#' @return \code{ex_trade_data} object of class "VirtualExtendedTradeData"
#' @export
setMethod("getExtendedTradeDataObject",
          signature(object = "VirtualExtendedTradeDataHandler"),
          function(object){
            return(object@ex_trade_data)
          }
)

#' Set ex_trade data object in object slot
#'
#' Public method to set ex_trade_data slot with "VirtualExtendedTradeData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualExtendedTradeDataHandler"
#' @param ex_trade_data object of class "VirtualExtendedTradeData"
#' @return \code{object} object of class "VirtualExtendedTradeDataHandler"
#' @export

setGeneric("setExtendedTradeDataObject", function(object,ex_trade_data){standardGeneric("setExtendedTradeDataObject")})


#' Set ex_trade data object in object slot
#'
#' Private method to set ex_trade_data slot with "VirtualExtendedTradeData"
#'
#' @rdname private_setExtendedTradeDataObject
#' @param object object of class "VirtualExtendedTradeDataHandler"
#' @param ex_trade_data object of class "VirtualExtendedTradeData"
#' @return \code{object} object of class "VirtualExtendedTradeDataHandler"

setGeneric(".setExtendedTradeDataObject", function(object,ex_trade_data){standardGeneric(".setExtendedTradeDataObject")})

setMethod(".setExtendedTradeDataObject",
          signature(object = "VirtualExtendedTradeDataHandler", ex_trade_data = "VirtualTradeData"),
          function(object, ex_trade_data){
              object@ex_trade_data <- ex_trade_data
            return(object)
          }
)
