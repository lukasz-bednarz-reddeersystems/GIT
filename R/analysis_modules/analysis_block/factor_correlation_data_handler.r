sourceTo("../common/factor_correlation_data/factor_correlation_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#####################################
#
# VirtualFactorCorrelationDataHandler Class
#
#####################################


setClass(
  Class          = "VirtualFactorCorrelationDataHandler",
  slots = c(
    factor_correlation = "VirtualFactorCorrelationData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getFactorCorrelationDataObject", function(object,...){standardGeneric("getFactorCorrelationDataObject")})
# Returns factor_correlation object of class "VirtualFactorCorrelationData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualFactorCorrelationDataHandler"
# Returns:
#   factor_correlation : "VirtualFactorCorrelationData" object

setMethod("getFactorCorrelationDataObject",  
          signature(object = "VirtualFactorCorrelationDataHandler"),
          function(object){
            return(object@factor_correlation)
          }
)

if (!isGenericS4("setFactorCorrelationDataObject")){
  setGeneric("setFactorCorrelationDataObject", function(object,factor_correlation, ...){standardGeneric("setFactorCorrelationDataObject")})
}
# Public method to set factor_correlation slot with "VirtualFactorCorrelationData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualFactorCorrelationDataHandler"
#   factor_correlation : object of class "VirtualFactorCorrelationData" 
# Returns:
#   object : object of type "VirtualFactorCorrelationDataHandler"




setGeneric(".setFactorCorrelationDataObject", function(object,factor_correlation, ...){standardGeneric(".setFactorCorrelationDataObject")})
# Private method to set factor_correlation slot with "VirtualFactorCorrelationData" class object
#
# Args:
#   object : object of type "VirtualFactorCorrelationDataHandler"
#   factor_correlation : object of class "VirtualFactorCorrelationData" 
# Returns:
#   object : object of type "VirtualFactorCorrelationDataHandler"

setMethod(".setFactorCorrelationDataObject",  
          signature(object = "VirtualFactorCorrelationDataHandler", factor_correlation = "VirtualFactorCorrelationData"),
          function(object, factor_correlation){
            object@factor_correlation <- factor_correlation
            return(object)
          }
)
