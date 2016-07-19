sourceTo("../common/event_data/event_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualEventDataHandler Class
#
####################################

setClass(
  Class          = "VirtualEventDataHandler",
  slots = c(
    event_data = "VirtualEventData"
  ),
  prototype = list(
    event_data = new("EventData")
  ),
  contains = c("VIRTUAL")
)

setGeneric("getEventDataObject", function(object,...){standardGeneric("getEventDataObject")})
# Returns event_data object of class "EventData" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualEventDataHandler"
# Returns:
#   event_data : "EventData" object

setMethod("getEventDataObject",  
          signature(object = "VirtualEventDataHandler"),
          function(object){
            return(object@event_data)
          }
)


if (!isGenericS4("setEventDataObject")){
  setGeneric("setEventDataObject", function(object,event_data, ...){standardGeneric("setEventDataObject")})
}
# Public method to set trade_Data slot with "EventData" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualEventDataHandler"
#   trade_data : object of class "EventData" 
# Returns:
#   object : object of type "VirtualEventDataHandler"



setGeneric(".setEventDataObject", function(object,event_data, ...){standardGeneric(".setEventDataObject")})
# Private method to set event_data slot with "EventData" class object
#
# Args:
#   object : object of type "VirtualEventDataHandler"
# Returns:
#   object : object of type "VirtualEventDataHandler"

setMethod(".setEventDataObject",  
          signature(object = "VirtualEventDataHandler", event_data = "VirtualEventData"),
          function(object, event_data){
              object@event_data <- event_data
            return(object)
          }
)
