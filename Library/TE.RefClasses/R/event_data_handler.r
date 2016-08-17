#' @include event_data.r
NULL

####################################
#
# VirtualEventDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualEventData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualEventData or derived classes
#'
#' @slot event_data object of class "VirtualEventData"

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

#' Get event_data stored in object
#'
#' Returns event_data object of class "VirtualEventData"
#'
#' @param object object of class "VirtualEventDataHandler"
#' @return \code{event_data} object of class "VirtualEventData"
#' @export

setGeneric("getEventDataObject", function(object,...){standardGeneric("getEventDataObject")})

setMethod("getEventDataObject",
          signature(object = "VirtualEventDataHandler"),
          function(object){
            return(object@event_data)
          }
)


#' Set event_data object in object slot
#'
#' Public method to set event_data slot with "VirtualEventData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualEventDataHandler"
#' @param event_data object of class "VirtualEventData"
#' @return \code{object} object of class "VirtualEventDataHandler"
#' @export

setGeneric("setEventDataObject", function(object,event_data, ...){standardGeneric("setEventDataObject")})



#' Set event_data object in object slot
#'
#' Private method to set event_data slot with "VirtualEventData"
#'
#' @param object object of class "VirtualEventDataHandler"
#' @param event_data object of class "VirtualEventData"
#' @return \code{object} object of class "VirtualEventDataHandler"

setGeneric(".setEventDataObject", function(object,event_data, ...){standardGeneric(".setEventDataObject")})

setMethod(".setEventDataObject",
          signature(object = "VirtualEventDataHandler", event_data = "VirtualEventData"),
          function(object, event_data){
              object@event_data <- event_data
            return(object)
          }
)
