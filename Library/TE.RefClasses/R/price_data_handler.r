#' @include price_data.r
NULL


####################################
#
# VirtualPriceDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualPriceData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualPriceData or derived classes
#'
#' @slot price_data object of class "VirtualPriceData"

setClass(
  Class          = "VirtualPriceDataHandler",
  slots = c(
    price_data = "VirtualPriceData"
  ),
  prototype = list(
    price_data = new("PriceData")
  ),
  contains = c("VIRTUAL")
)



#' Get price_data stored in object
#'
#' Returns price_data object of class "VirtualPriceData"
#'
#' @param object object of class "VirtualPriceDataHandler"
#' @return \code{price_data} object of class "VirtualPriceData"
#' @export

setGeneric("getPriceDataObject", function(object){standardGeneric("getPriceDataObject")})

#' @describeIn getPriceDataObject
#' Get price_data stored in object
#'
#' Returns price_data object of class "VirtualPriceData"
#'
#' @inheritParams getPriceDataObject
#' @return \code{price_data} object of class "VirtualPriceData"
#' @export
setMethod("getPriceDataObject",
          signature(object = "VirtualPriceDataHandler"),
          function(object){
            return(object@price_data)
          }
)


#' Set price_data object in object slot
#'
#' Public method to set price_data slot with "VirtualPriceData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualPriceDataHandler"
#' @param price_data object of class "VirtualPriceData"
#' @return \code{object} object of class "VirtualPriceDataHandler"
#' @export

setGeneric("setPriceDataObject", function(object,price_data){standardGeneric("setPriceDataObject")})




#' Set price_data object in object slot
#'
#' Private method to set price_data slot with "VirtualPriceData"
#'
#' @param object object of class "VirtualPriceDataHandler"
#' @param price_data object of class "VirtualPriceData"
#' @return \code{object} object of class "VirtualPriceDataHandler"

setGeneric(".setPriceDataObject", function(object,price_data){standardGeneric(".setPriceDataObject")})

setMethod(".setPriceDataObject",
          signature(object = "VirtualPriceDataHandler", price_data = "VirtualPriceData"),
          function(object, price_data){
              object@price_data <- price_data
            return(object)
          }
)
