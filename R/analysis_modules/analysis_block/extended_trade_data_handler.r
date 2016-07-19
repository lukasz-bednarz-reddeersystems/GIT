sourceTo("../common/trade_data/trade_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualExtendedTradeDataHandler Class
#
####################################

setClassUnion("ExtendedTradeDataClassUnion", c("ExtendedTradeData", 
                                               "TradesExtendedReturnPerMonth"))


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


setGeneric("getExtendedTradeDataObject", function(object,...){standardGeneric("getExtendedTradeDataObject")})
# Returns ex_trade_data object of class "ExtendedTradeData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualExtendedTradeDataHandler"
# Returns:
#   ex_trade_data : "ExtendedTradeData" object

setMethod("getExtendedTradeDataObject",  
          signature(object = "VirtualExtendedTradeDataHandler"),
          function(object){
            return(object@ex_trade_data)
          }
)

if (!isGenericS4("setExtendedTradeDataObject")){
  setGeneric("setExtendedTradeDataObject", function(object,ex_trade_data, ...){standardGeneric("setExtendedTradeDataObject")})
}
# Public method to set ex_trade_data slot with "ExtendedTradeData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualExtendedTradeDataHandler"
#   ex_trade_data : object of class "ExtendedTradeData" 
# Returns:
#   object : object of type "VirtualExtendedTradeDataHandler"



if (!isGenericS4(".setExtendedTradeDataObject")){
  setGeneric(".setExtendedTradeDataObject", function(object,ex_trade_data, ...){standardGeneric(".setExtendedTradeDataObject")})
}
# Private method to set ex_trade_data slot with "ExtendedTradeData" class object
#
# Args:
#   object : object of type "VirtualExtendedTradeDataHandler"
#   ex_trade_data : object of class "ExtendedTradeData" 
# Returns:
#   object : object of type "VirtualExtendedTradeDataHandler"

setMethod(".setExtendedTradeDataObject",  
          signature(object = "VirtualExtendedTradeDataHandler", ex_trade_data = "VirtualTradeData"),
          function(object, ex_trade_data){
              object@ex_trade_data <- ex_trade_data
            return(object)
          }
)
