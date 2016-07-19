sourceTo("../common/strategy_data/strategy_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualStrategyDataHandler Class
#
####################################

setClass(
  Class          = "VirtualStrategyDataHandler",
  slots = c(
    strategy_data = "VirtualStrategyData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getStrategyDataObject", function(object,...){standardGeneric("getStrategyDataObject")})
# Returns strategy_data object of class "StrategyData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualStrategyDataHandler"
# Returns:
#   strategy_data : "VirtualStrategyData" object

setMethod("getStrategyDataObject",  
          signature(object = "VirtualStrategyDataHandler"),
          function(object){
            return(object@strategy_data)
          }
)

if (!isGenericS4("setStrategyDataObject")){
  setGeneric("setStrategyDataObject", function(object,strategy_data, ...){standardGeneric("setStrategyDataObject")})
}
# Public method to set strategy_data slot with "StrategyData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualStrategyDataHandler"
#   strategy_data : object of class "StrategyData" 
# Returns:
#   object : object of type "VirtualStrategyDataHandler"




setGeneric(".setStrategyDataObject", function(object,strategy_data, ...){standardGeneric(".setStrategyDataObject")})
# Private method to set strategy_data slot with "StrategyData" class object
#
# Args:
#   object : object of type "VirtualStrategyDataHandler"
#   strategy_data : object of class "StrategyData" 
# Returns:
#   object : object of type "VirtualStrategyDataHandler"

setMethod(".setStrategyDataObject",  
          signature(object = "VirtualStrategyDataHandler", strategy_data = "VirtualStrategyData"),
          function(object, strategy_data){
              object@strategy_data <- strategy_data
            return(object)
          }
)
