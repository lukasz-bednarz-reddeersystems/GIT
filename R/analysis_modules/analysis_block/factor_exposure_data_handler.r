sourceTo("../common/factor_exposure_data/factor_exposure_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualFactorExposureDataHandler Class
#
####################################

setClass(
  Class          = "VirtualFactorExposureDataHandler",
  slots = c(
    factor_exposure_data = "VirtualFactorExposureData"
  ),
  prototype = list(
    factor_exposure_data = new("FactorExposureData")
  ),
  contains = c("VIRTUAL")
)

setGeneric("getFactorExposureDataObject", function(object,...){standardGeneric("getFactorExposureDataObject")})
# Returns factor_exposure_data object of class "FactorExposureData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualFactorExposureDataHandler"
# Returns:
#   factor_exposure_data : "FactorExposureData" object

setMethod("getFactorExposureDataObject",  
          signature(object = "VirtualFactorExposureDataHandler"),
          function(object){
            return(object@factor_exposure_data)
          }
)


if (!isGenericS4("setFactorExposureDataObject")){
  setGeneric("setFactorExposureDataObject", function(object,factor_exposure_data, ...){standardGeneric("setFactorExposureDataObject")})
}
# Public method to set trade_Data slot with "FactorExposureData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualFactorExposureDataHandler"
#   trade_data : object of class "FactorExposureData" 
# Returns:
#   object : object of type "VirtualFactorExposureDataHandler"



setGeneric(".setFactorExposureDataObject", function(object,factor_exposure_data, ...){standardGeneric(".setFactorExposureDataObject")})
# Private method to set factor_exposure_data slot with "FactorExposureData" class object
#
# Args:
#   object : object of type "VirtualFactorExposureDataHandler"
# Returns:
#   object : object of type "VirtualFactorExposureDataHandler"

setMethod(".setFactorExposureDataObject",  
          signature(object = "VirtualFactorExposureDataHandler", factor_exposure_data = "VirtualFactorExposureData"),
          function(object, factor_exposure_data){
              object@factor_exposure_data <- factor_exposure_data
            return(object)
          }
)
