sourceTo("../common/market_data/market_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualMarketDataHandler Class
#
####################################

setClass(
  Class          = "VirtualMarketDataHandler",
  slots = c(
    market_data = "NullableMarketData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getMarketDataObject", function(object,...){standardGeneric("getMarketDataObject")})
# Returns market_data object of class "MarketData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualMarketDataHandler"
# Returns:
#   market_data : "MarketData" object

setMethod("getMarketDataObject",  
          signature(object = "VirtualMarketDataHandler"),
          function(object){
            return(object@market_data)
          }
)

if (!isGenericS4("setMarketDataObject")){
  setGeneric("setMarketDataObject", function(object,market_data, ...){standardGeneric("setMarketDataObject")})
}
# Public method to set market_data slot with "MarketData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualMarketDataHandler"
#   market_data : object of class "MarketData" 
# Returns:
#   object : object of type "VirtualMarketDataHandler"




setGeneric(".setMarketDataObject", function(object,market_data, ...){standardGeneric(".setMarketDataObject")})
# Private method to set market_data slot with "MarketData" class object
#
# Args:
#   object : object of type "VirtualMarketDataHandler"
#   market_data : object of class "MarketData" 
# Returns:
#   object : object of type "VirtualMarketDataHandler"

setMethod(".setMarketDataObject",  
          signature(object = "VirtualMarketDataHandler", market_data = "MarketData"),
          function(object, market_data){
              object@market_data <- market_data
            return(object)
          }
)
