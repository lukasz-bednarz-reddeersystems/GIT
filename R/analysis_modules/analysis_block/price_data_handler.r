sourceTo("../common/price_data/price_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualPriceDataHandler Class
#
####################################

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

setGeneric("getPriceDataObject", function(object,...){standardGeneric("getPriceDataObject")})
# Returns price_data object of class "PriceData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualPriceDataHandler"
# Returns:
#   price_data : "PriceData" object

setMethod("getPriceDataObject",  
          signature(object = "VirtualPriceDataHandler"),
          function(object){
            return(object@price_data)
          }
)


setGeneric(".setPriceDataObject", function(object,price_data, ...){standardGeneric(".setPriceDataObject")})
# Private method to set price_data slot with "PriceData" class object
#
# Args:
#   object : object of type "VirtualPriceDataHandler"
# Returns:
#   object : object of type "VirtualPriceDataHandler"

setMethod(".setPriceDataObject",  
          signature(object = "VirtualPriceDataHandler", price_data = "VirtualPriceData"),
          function(object, price_data){
              object@price_data <- price_data
            return(object)
          }
)
