#' @include market_style_data.r
NULL

#####################################
#
# VirtualMarketStyleDataHandler Class
#
#####################################

#' Virtual S4 class handling VirtualMarketStyleData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualMarketStyleData or derived classes
#'
#' @slot market_style object of class "VirtualMarketStyleData"

setClass(
  Class          = "VirtualMarketStyleDataHandler",
  slots = c(
    market_style = "VirtualMarketStyleData"
  ),
  contains = c("VIRTUAL")
)


#' Get market_style stored in object
#'
#' Returns market_style object of class "VirtualMarketStyleData"
#'
#' @param object object of class "VirtualMarketStyleDataHandler"
#' @return \code{market_style} object of class "VirtualMarketStyleData"
#' @export

setGeneric("getMarketStyleDataObject", function(object,...){standardGeneric("getMarketStyleDataObject")})

setMethod("getMarketStyleDataObject",
          signature(object = "VirtualMarketStyleDataHandler"),
          function(object){
            return(object@market_style)
          }
)



#' Set market_style object in object slot
#'
#' Public method to set market_style slot with "VirtualMarketStyleData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualMarketStyleDataHandler"
#' @param market_style object of class "VirtualMarketStyleData"
#' @return \code{object} object of class "VirtualMarketStyleDataHandler"
#' @export

setGeneric("setMarketStyleDataObject", function(object,market_style, ...){standardGeneric("setMarketStyleDataObject")})



#' Set market_style object in object slot
#'
#' Private method to set market_style slot with "VirtualMarketStyleData"
#'
#' @param object object of class "VirtualMarketStyleDataHandler"
#' @param market_style object of class "VirtualMarketStyleData"
#' @return \code{object} object of class "VirtualMarketStyleDataHandler"

setGeneric(".setMarketStyleDataObject", function(object,market_style, ...){standardGeneric(".setMarketStyleDataObject")})

setMethod(".setMarketStyleDataObject",
          signature(object = "VirtualMarketStyleDataHandler", market_style = "VirtualMarketStyleData"),
          function(object, market_style){
            object@market_style <- market_style
            return(object)
          }
)
