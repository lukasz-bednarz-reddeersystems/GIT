sourceTo("../common/position_data/position_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualPositionDataHandler Class
#
####################################

setClass(
  Class          = "VirtualPositionDataHandler",
  slots = c(
    position_data = "VirtualPositionData"
  ),
  prototype = list(
    position_data = new("PositionData")
  ),
  contains = c("VIRTUAL")
)

setGeneric("getPositionDataObject", function(object,...){standardGeneric("getPositionDataObject")})
# Returns position_data object of class "PositionData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualPositionDataHandler"
# Returns:
#   position_data : "PositionData" object

setMethod("getPositionDataObject",  
          signature(object = "VirtualPositionDataHandler"),
          function(object){
            return(object@position_data)
          }
)


if (!isGenericS4("setPositionDataObject")){
  setGeneric("setPositionDataObject", function(object,position_data, ...){standardGeneric("setPositionDataObject")})
}
# Public method to set position_data slot with "PositionData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualPositionDataHandler"
#   position_data : object of class "VirtualPositionData" 
# Returns:
#   object : object of type "VirtualPositionDataHandler"





setGeneric(".setPositionDataObject", function(object,position_data, ...){standardGeneric(".setPositionDataObject")})
# Private method to set position_data slot with "PositionData" class object
#
# Args:
#   object : object of type "VirtualPositionDataHandler"
# Returns:
#   object : object of type "VirtualPositionDataHandler"

setMethod(".setPositionDataObject",  
          signature(object = "VirtualPositionDataHandler", position_data = "VirtualPositionData"),
          function(object, position_data){
              object@position_data <- position_data
            return(object)
          }
)
