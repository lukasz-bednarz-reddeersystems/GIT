sourceTo("../common/instrument_betas_data/instrument_betas_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#####################################
#
# VirtualInstrumentBetasDataHandler Class
#
#####################################


setClass(
  Class          = "VirtualInstrumentBetasDataHandler",
  slots = c(
    instrument_betas = "VirtualInstrumentBetasData"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getInstrumentBetasDataObject", function(object,...){standardGeneric("getInstrumentBetasDataObject")})
# Returns instrument_betas object of class "VirtualInstrumentBetasData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualInstrumentBetasDataHandler"
# Returns:
#   instrument_betas : "VirtualInstrumentBetasData" object

setMethod("getInstrumentBetasDataObject",  
          signature(object = "VirtualInstrumentBetasDataHandler"),
          function(object){
            return(object@instrument_betas)
          }
)

if (!isGenericS4("setInstrumentBetasDataObject")){
  setGeneric("setInstrumentBetasDataObject", function(object,instrument_betas, ...){standardGeneric("setInstrumentBetasDataObject")})
}
# Public method to set instrument_betas slot with "VirtualInstrumentBetasData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualInstrumentBetasDataHandler"
#   instrument_betas : object of class "VirtualInstrumentBetasData" 
# Returns:
#   object : object of type "VirtualInstrumentBetasDataHandler"




setGeneric(".setInstrumentBetasDataObject", function(object,instrument_betas, ...){standardGeneric(".setInstrumentBetasDataObject")})
# Private method to set instrument_betas slot with "VirtualInstrumentBetasData" class object
#
# Args:
#   object : object of type "VirtualInstrumentBetasDataHandler"
#   instrument_betas : object of class "VirtualInstrumentBetasData" 
# Returns:
#   object : object of type "VirtualInstrumentBetasDataHandler"

setMethod(".setInstrumentBetasDataObject",  
          signature(object = "VirtualInstrumentBetasDataHandler", instrument_betas = "VirtualInstrumentBetasData"),
          function(object, instrument_betas){
              object@instrument_betas <- instrument_betas
            return(object)
          }
)
