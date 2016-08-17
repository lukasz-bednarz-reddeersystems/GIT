sourceTo("../common/implied_factor_returns_data/implied_factor_returns_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

###############################################
#
# VirtualImpliedFactorReturnsDataHandler Class
#
##############################################


setClass(
  Class          = "VirtualImpliedFactorReturnsDataHandler",
  slots = c(
    implied_factor_returns = "VirtualImpliedFactorReturnsData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getImpliedFactorReturnsDataObject", function(object,...){standardGeneric("getImpliedFactorReturnsDataObject")})
# Returns implied_factor_returns object of class "VirtualImpliedFactorReturnsData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualImpliedFactorReturnsDataHandler"
# Returns:
#   implied_factor_returns : "VirtualImpliedFactorReturnsData" object

setMethod("getImpliedFactorReturnsDataObject",  
          signature(object = "VirtualImpliedFactorReturnsDataHandler"),
          function(object){
            return(object@implied_factor_returns)
          }
)

if (!isGenericS4("setImpliedFactorReturnsDataObject")){
  setGeneric("setImpliedFactorReturnsDataObject", function(object,implied_factor_returns, ...){standardGeneric("setImpliedFactorReturnsDataObject")})
}
# Public method to set implied_factor_returns slot with "VirtualImpliedFactorReturnsData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualImpliedFactorReturnsDataHandler"
#   implied_factor_returns : object of class "VirtualImpliedFactorReturnsData" 
# Returns:
#   object : object of type "VirtualImpliedFactorReturnsDataHandler"




setGeneric(".setImpliedFactorReturnsDataObject", function(object,implied_factor_returns, ...){standardGeneric(".setImpliedFactorReturnsDataObject")})
# Private method to set implied_factor_returns slot with "VirtualImpliedFactorReturnsData" class object
#
# Args:
#   object : object of type "VirtualImpliedFactorReturnsDataHandler"
#   implied_factor_returns : object of class "VirtualImpliedFactorReturnsData" 
# Returns:
#   object : object of type "VirtualImpliedFactorReturnsDataHandler"

setMethod(".setImpliedFactorReturnsDataObject",  
          signature(object = "VirtualImpliedFactorReturnsDataHandler", implied_factor_returns = "VirtualImpliedFactorReturnsData"),
          function(object, implied_factor_returns){
            object@implied_factor_returns <- implied_factor_returns
            return(object)
          }
)
