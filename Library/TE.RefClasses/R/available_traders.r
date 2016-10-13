#' @include available_values.r
#' @include available_values_data_handler.r
NULL

####################################
#
# AvailableTraders Class
#
####################################

#' Contrete S4 class providing a dummy mechanism for
#' analysis blocks to query the available
#' trader ids.
#'
#' dataRequest implementation supplies the
#' values from a static .obtainValues
#' implementation.
#'
#' Inherits from "VirtualAvailableValues

setClass(
  Class = "AvailableTraders",
  prototype = list(column_name="TraderID"),
  contains = c("VirtualAvailableValues")
)

#' Dummy method returns hard coded trader IDs
setMethod(".obtainValues",
          signature(object="AvailableTraders"),
          function(object,key_values){
            key_values
            df <- data.frame(c(11,101,70))
            colnames(df) <- object@column_name
            return(df)
          }
)

#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'TestDataStoreClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'TestDataStoreClient'.
#' @export
#'
setMethod("dataRequest",
          signature(object="AvailableTraders",key_values="data.frame"),
          function(object,key_values){
            return(.obtainValues(object,key_values))
          }
)




