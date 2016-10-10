#' @include available_values.r
#' @include available_values_data_handler.r
NULL

####################################
#
# VirtualKeyExpander Class
#
####################################

#' Virtual S4 class that expands a key column over
#' the available values from the relevant handler.
#'
#'
#' @slot traders object of class "VirtualAvailableValues"

setClass(
  Class    = "VirtualKeyExpander",
  slots    = c(has_been_expanded = 'logical',
               key_values = 'data.frame'),
  prototype= list(has_been_expanded = FALSE),
  contains = c("VIRTUAL",
               "VirtualAvailableValuesDataHandler")
)

#' Expand key frame over the available_values column name
#' if that column is not already present.
#'
#' @param object object of class "VirtualKeyExpander"
#' @param object key_values of class "data.frame"
#' @return \code{object} object of class "VirtualKeyExpander"
#' @export
setGeneric("expandKeys", function(object,key_values){standardGeneric("expandKeys")})

#' @describeIn expandKeys
#'
#' Expand key frame over the available_values column name
#' if that column is not already present.
#'
#' @inheritParams expandKeys
#' @return \code{object} object of class "VirtualKeyExpander"
#' @export
setMethod("expandKeys",
          signature(object = "VirtualKeyExpander",key_values = "data.frame"),
          function(object,key_values){
            available_values <- getAvailableValuesObject(object)
            column_name <- getValueName(object)
            if(!(column_name%in%colnames(key_values))){
              message(paste("Expanding key on missing column",column_name))
              av <- dataRequest(available_values,key_values)
              key_values <- merge(av,key_values,all=TRUE)
              object@has_been_expanded <- TRUE
            }
            object@key_values <- key_values
            return(object)
          }
)

#' Get key_values
#'
#' @param object object of class "VirtualKeyExpander"
#' @return \code{key_values} object of class "data.frame"
#' @export
setGeneric("getExpanderKeys", function(object){standardGeneric("getExpanderKeys")})

#' @describeIn getExpanderKeys
#'
#' Get key_values
#'
#' @inheritParams getExpanderKeys
#' @return \code{key_values} object of class "data.frame"
#' @export
setMethod("getExpanderKeys",
          signature(object = "VirtualKeyExpander"),
          function(object){
            return(object@key_values)
          }
)

#' Get has_been_expanded
#'
#' @param object object of class "VirtualKeyExpander"
#' @return \code{has_been_expanded} object of class "logical"
#' @export
setGeneric("hasQueryBeenExpanded", function(object){standardGeneric("hasQueryBeenExpanded")})

#' @describeIn hasQueryBeenExpanded
#'
#' Get has_been_expanded
#'
#' @inheritParams hasQueryBeenExpanded
#' @return \code{has_been_expanded} object of class "logical"
#' @export
setMethod("hasQueryBeenExpanded",
          signature(object = "VirtualKeyExpander"),
          function(object){
            return(object@has_been_expanded)
          }
)
