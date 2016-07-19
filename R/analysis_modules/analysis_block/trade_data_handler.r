sourceTo("../common/trade_data/trade_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualTradeDataHandler Class
#
####################################

setClassUnion("TradeDataClassUnion", c("TradeData", "TradesExtendedReturnPerMonth"))

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


setGeneric("getTradeDataObject", function(object,...){standardGeneric("getTradeDataObject")})
# Returns trade_data object of class "TradeData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualTradeDataHandler"
# Returns:
#   trade_data : "TradeData" object

setMethod("getTradeDataObject",  
          signature(object = "VirtualTradeDataHandler"),
          function(object){
            return(object@trade_data)
          }
)

if (!isGenericS4("setTradeDataObject")){
  setGeneric("setTradeDataObject", function(object,trade_data, ...){standardGeneric("setTradeDataObject")})
}
# Public method to set trade_Data slot with "TradeData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualTradeDataHandler"
#   trade_data : object of class "TradeData" 
# Returns:
#   object : object of type "VirtualTradeDataHandler"




setGeneric(".setTradeDataObject", function(object,trade_data, ...){standardGeneric(".setTradeDataObject")})
# Private method to set trade_Data slot with "TradeData" class object
#
# Args:
#   object : object of type "VirtualTradeDataHandler"
#   trade_data : object of class "TradeData" 
# Returns:
#   object : object of type "VirtualTradeDataHandler"

setMethod(".setTradeDataObject",  
          signature(object = "VirtualTradeDataHandler", trade_data = "VirtualTradeData"),
          function(object, trade_data){
              object@trade_data <- trade_data
            return(object)
          }
)
