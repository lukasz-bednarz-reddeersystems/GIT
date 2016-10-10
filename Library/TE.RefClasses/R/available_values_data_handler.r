#' @include available_values.r
NULL

####################################
#
# VirtualAvailableValuesDataHandler Class
#
####################################

#' Virtual S4 class handling VirtualAvailableValues objects
#'
#' Class that is to be inherited by any objects
#' that will contain AvailableValues
#'
#' @slot Values object of class "VirtualAvailableValues"

setClass(
  Class          = "VirtualAvailableValuesDataHandler",
  slots = c(
    available_values = "VirtualAvailableValues"
  ),
  contains = c("VIRTUAL")
)

#' Get available_Values stored in object
#'
#' Returns available_Values object of class "VirtualAvailableValues"
#'
#' @param object object of class "VirtualAvailableValuesDataHandler"
#' @return \code{available_Values} object of class "VirtualAVailableValues"
#' @export

setGeneric("getAvailableValuesObject", function(object){standardGeneric("getAvailableValuesObject")})

#' @describeIn getAvailableValuesObject
#' Get available_Values stored in object
#'
#' Returns available_Values object of class "VirtualAvailableValues"
#'
#' @inheritParams getAvailableValuesObject
#' @return \code{available_Values} object of class "VirtualAvailableValues"
#' @export
setMethod("getAvailableValuesObject",
          signature(object = "VirtualAvailableValuesDataHandler"),
          function(object){
            return(object@available_values)
          }
)

#' Set available_Values object in object slot
#'
#' Public method to set available_Values slot with "VirtualAvailableValues"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualAvailableValuesDataHandler"
#' @param available_Values object of class "VirtualAvailableValues"
#' @return \code{object} object of class "VirtualAvailableValuesDataHandler"
#' @export
setGeneric("setAvailableValuesObject", function(object,available_Values){standardGeneric("setAvailableValuesObject")})


#' Set available_Values object in object slot
#'
#' Private method to set available_Values slot with "VirtualAvailableValues"
#'
#' @rdname private_setAvailableValuesObject
#' @param object object of class "VirtualAvailableValuesDataHandler"
#' @param available_Values object of class "VirtualAvailableValues"
#' @return \code{object} object of class "VirtualAvailableValuesDataHandler"

setGeneric(".setAvailableValuesObject", function(object,available_Values){standardGeneric(".setAvailableValuesObject")})

setMethod(".setAvailableValuesObject",
          signature(object = "VirtualAvailableValuesDataHandler", available_Values = "VirtualAvailableValues"),
          function(object, available_Values){
              object@available_Values <- available_Values
            return(object)
          }
)

#' Getter for the value column name
#'
#' @param object object of class 'VirtualAvailableValuesDataHandler'.
#' @return \code{name} "character"
#' @export
setGeneric("getValueName", function(object){standardGeneric("getValueName")})
setMethod("getValueName","VirtualAvailableValuesDataHandler",
          function(object){
            return(object@available_values@column_name)
          }
)

