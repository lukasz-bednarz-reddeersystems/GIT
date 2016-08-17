sourceTo("../common/market_style_data/market_style_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#####################################
#
# VirtualMarketStyleDataHandler Class
#
#####################################


setClass(
  Class          = "VirtualMarketStyleDataHandler",
  slots = c(
    market_style = "VirtualMarketStyleData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getMarketStyleDataObject", function(object,...){standardGeneric("getMarketStyleDataObject")})
# Returns market_style object of class "VirtualMarketStyleData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualMarketStyleDataHandler"
# Returns:
#   market_style : "VirtualMarketStyleData" object

setMethod("getMarketStyleDataObject",  
          signature(object = "VirtualMarketStyleDataHandler"),
          function(object){
            return(object@market_style)
          }
)

if (!isGenericS4("setMarketStyleDataObject")){
  setGeneric("setMarketStyleDataObject", function(object,market_style, ...){standardGeneric("setMarketStyleDataObject")})
}
# Public method to set market_style slot with "VirtualMarketStyleData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualMarketStyleDataHandler"
#   market_style : object of class "VirtualMarketStyleData" 
# Returns:
#   object : object of type "VirtualMarketStyleDataHandler"




setGeneric(".setMarketStyleDataObject", function(object,market_style, ...){standardGeneric(".setMarketStyleDataObject")})
# Private method to set market_style slot with "VirtualMarketStyleData" class object
#
# Args:
#   object : object of type "VirtualMarketStyleDataHandler"
#   market_style : object of class "VirtualMarketStyleData" 
# Returns:
#   object : object of type "VirtualMarketStyleDataHandler"

setMethod(".setMarketStyleDataObject",  
          signature(object = "VirtualMarketStyleDataHandler", market_style = "VirtualMarketStyleData"),
          function(object, market_style){
            object@market_style <- market_style
            return(object)
          }
)
