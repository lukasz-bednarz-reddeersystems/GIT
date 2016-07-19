sourceTo("../common/factor_variance_data/factor_variance_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#####################################
#
# VirtualFactorVarianceDataHandler Class
#
#####################################


setClass(
  Class          = "VirtualFactorVarianceDataHandler",
  slots = c(
    factor_variance = "VirtualFactorVarianceData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getFactorVarianceDataObject", function(object,...){standardGeneric("getFactorVarianceDataObject")})
# Returns factor_variance object of class "VirtualFactorVarianceData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualFactorVarianceDataHandler"
# Returns:
#   factor_variance : "VirtualFactorVarianceData" object

setMethod("getFactorVarianceDataObject",  
          signature(object = "VirtualFactorVarianceDataHandler"),
          function(object){
            return(object@factor_variance)
          }
)

if (!isGenericS4("setFactorVarianceDataObject")){
  setGeneric("setFactorVarianceDataObject", function(object,factor_variance, ...){standardGeneric("setFactorVarianceDataObject")})
}
# Public method to set factor_variance slot with "VirtualFactorVarianceData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualFactorVarianceDataHandler"
#   factor_variance : object of class "VirtualFactorVarianceData" 
# Returns:
#   object : object of type "VirtualFactorVarianceDataHandler"




setGeneric(".setFactorVarianceDataObject", function(object,factor_variance, ...){standardGeneric(".setFactorVarianceDataObject")})
# Private method to set factor_variance slot with "VirtualFactorVarianceData" class object
#
# Args:
#   object : object of type "VirtualFactorVarianceDataHandler"
#   factor_variance : object of class "VirtualFactorVarianceData" 
# Returns:
#   object : object of type "VirtualFactorVarianceDataHandler"

setMethod(".setFactorVarianceDataObject",  
          signature(object = "VirtualFactorVarianceDataHandler", factor_variance = "VirtualFactorVarianceData"),
          function(object, factor_variance){
            object@factor_variance <- factor_variance
            return(object)
          }
)
