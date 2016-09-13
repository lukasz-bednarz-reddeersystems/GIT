#' @include strategy_data.r
NULL

####################################
#
# VirtualStrategyDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualStrategyData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualStrategyData or derived classes
#'
#' @slot strategy_data object of class "VirtualStrategyData"

setClass(
  Class          = "VirtualStrategyDataHandler",
  slots = c(
    strategy_data = "VirtualStrategyData"
  ),
  contains = c("VIRTUAL")
)


#' Get strategy_data stored in object
#'
#' Returns strategy_data object of class "VirtualStrategyData"
#'
#' @param object object of class "VirtualStrategyDataHandler"
#' @return \code{strategy_data} object of class "VirtualStrategyData"
#' @export

setGeneric("getStrategyDataObject", function(object){standardGeneric("getStrategyDataObject")})

#' @describeIn getStrategyDataObject
#' Get strategy_data stored in object
#'
#' Returns strategy_data object of class "VirtualStrategyData"
#'
#' @inheritParams getStrategyDataObject
#' @return \code{strategy_data} object of class "VirtualStrategyData"
#' @export
setMethod("getStrategyDataObject",
          signature(object = "VirtualStrategyDataHandler"),
          function(object){
            return(object@strategy_data)
          }
)


#' Set strategy_data object in object slot
#'
#' Public method to set strategy_data slot with "VirtualStrategyData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualStrategyDataHandler"
#' @param strategy_data object of class "VirtualStrategyData"
#' @return \code{object} object of class "VirtualStrategyDataHandler"
#' @export

setGeneric("setStrategyDataObject", function(object,strategy_data){standardGeneric("setStrategyDataObject")})



#' Set strategy_data object in object slot
#'
#' Private method to set strategy_data slot with "VirtualStrategyData"
#'
#' @rdname private_setStrategyDataObject
#' @param object object of class "VirtualStrategyDataHandler"
#' @param strategy_data object of class "VirtualStrategyData"
#' @return \code{object} object of class "VirtualStrategyDataHandler"

setGeneric(".setStrategyDataObject", function(object,strategy_data){standardGeneric(".setStrategyDataObject")})

setMethod(".setStrategyDataObject",
          signature(object = "VirtualStrategyDataHandler", strategy_data = "VirtualStrategyData"),
          function(object, strategy_data){
              object@strategy_data <- strategy_data
            return(object)
          }
)
