#' @include position_data.r
NULL

####################################
#
# VirtualPositionDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualPositionData objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualPositionData or derived classes
#'
#' @slot position_data object of class "VirtualPositionData"

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


#' Get position_data stored in object
#'
#' Returns position_data object of class "VirtualPositionData"
#'
#' @param object object of class "VirtualPositionDataHandler"
#' @return \code{position_data} object of class "VirtualPositionData"
#' @export

setGeneric("getPositionDataObject", function(object){standardGeneric("getPositionDataObject")})

#' @describeIn getPositionDataObject
#' Get position_data stored in object
#'
#' Returns position_data object of class "VirtualPositionData"
#'
#' @inheritParams getPositionDataObject
#' @return \code{position_data} object of class "VirtualPositionData"
#' @export
setMethod("getPositionDataObject",
          signature(object = "VirtualPositionDataHandler"),
          function(object){
            return(object@position_data)
          }
)



#' Set position_data object in object slot
#'
#' Public method to set position_data slot with "VirtualPositionData"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualPositionDataHandler"
#' @param position_data object of class "VirtualPositionData"
#' @return \code{object} object of class "VirtualPositionDataHandler"
#' @export

setGeneric("setPositionDataObject", function(object,position_data){standardGeneric("setPositionDataObject")})




#' Set position_data object in object slot
#'
#' Private method to set position_data slot with "VirtualPositionData"
#'
#' @rdname private_setPositionDataObject
#' @param object object of class "VirtualPositionDataHandler"
#' @param position_data object of class "VirtualPositionData"
#' @return \code{object} object of class "VirtualPositionDataHandler"

setGeneric(".setPositionDataObject", function(object,position_data){standardGeneric(".setPositionDataObject")})


setMethod(".setPositionDataObject",
          signature(object = "VirtualPositionDataHandler", position_data = "VirtualPositionData"),
          function(object, position_data){
              object@position_data <- position_data
            return(object)
          }
)
