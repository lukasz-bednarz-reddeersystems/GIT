#' @include trade_data.r
NULL


####################################
#
# VirtualTradeDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualTradeData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualTradeData or derived classes
#'
#' @slot trade_data object of class "VirtualTradeData"

setClass(
  Class          = "VirtualTradeDataHandler",
  slots = c(
    trade_data = "VirtualTradeData"
  ),
  prototype = list(
    trade_data = new("TradeData")
  ),
  contains = c("VIRTUAL")
)


#' Get trade_data stored in object
#'
#' Returns trade_data object of class "VirtualTradeData"
#'
#' @param object object of class "VirtualTradeDataHandler"
#' @return \code{trade_data} object of class "VirtualTradeData"
#' @export

setGeneric("getTradeDataObject", function(object){standardGeneric("getTradeDataObject")})

#' @describeIn getTradeDataObject
#' Get trade_data stored in object
#'
#' Returns trade_data object of class "VirtualTradeData"
#'
#' @inheritParams getTradeDataObject
#' @return \code{trade_data} object of class "VirtualTradeData"
#' @export
setMethod("getTradeDataObject",
          signature(object = "VirtualTradeDataHandler"),
          function(object){
            return(object@trade_data)
          }
)

#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "VirtualTradeData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualTradeDataHandler"
#' @param trade_data object of class "VirtualTradeData"
#' @return \code{object} object of class "VirtualTradeDataHandler"
#' @export

setGeneric("setTradeDataObject", function(object,trade_data){standardGeneric("setTradeDataObject")})


#' Set trade_data object in object slot
#'
#' Private method to set trade_data slot with "VirtualTradeData"
#'
#' @param object object of class "VirtualTradeDataHandler"
#' @param trade_data object of class "VirtualTradeData"
#' @return \code{object} object of class "VirtualTradeDataHandler"

setGeneric(".setTradeDataObject", function(object,trade_data){standardGeneric(".setTradeDataObject")})

setMethod(".setTradeDataObject",
          signature(object = "VirtualTradeDataHandler", trade_data = "VirtualTradeData"),
          function(object, trade_data){
              object@trade_data <- trade_data
            return(object)
          }
)
