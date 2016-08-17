#' @include market_data.r
NULL

####################################
#
# VirtualMarketDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualMarketData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualMarketData or derived classes
#'
#' @slot market_data object of class "VirtualMarketData"

setClass(
  Class          = "VirtualMarketDataHandler",
  slots = c(
    market_data = "NullableMarketData"
  ),
  contains = c("VIRTUAL")
)


#' Get market_data stored in object
#'
#' Returns market_data object of class "VirtualMarketData"
#'
#' @param object object of class "VirtualMarketDataHandler"
#' @return \code{market_data} object of class "VirtualMarketData"
#' @export

setGeneric("getMarketDataObject", function(object,...){standardGeneric("getMarketDataObject")})

setMethod("getMarketDataObject",
          signature(object = "VirtualMarketDataHandler"),
          function(object){
            return(object@market_data)
          }
)


#' Set market_data object in object slot
#'
#' Public method to set market_data slot with "VirtualMarketData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualMarketDataHandler"
#' @param market_data object of class "VirtualMarketData"
#' @return \code{object} object of class "VirtualMarketDataHandler"
#' @export

setGeneric("setMarketDataObject", function(object,market_data, ...){standardGeneric("setMarketDataObject")})



#' Set market_data object in object slot
#'
#' Private method to set market_data slot with "VirtualMarketData"
#'
#' @param object object of class "VirtualMarketDataHandler"
#' @param market_data object of class "VirtualMarketData"
#' @return \code{object} object of class "VirtualMarketDataHandler"

setGeneric(".setMarketDataObject", function(object,market_data, ...){standardGeneric(".setMarketDataObject")})

setMethod(".setMarketDataObject",
          signature(object = "VirtualMarketDataHandler", market_data = "MarketData"),
          function(object, market_data){
              object@market_data <- market_data
            return(object)
          }
)
