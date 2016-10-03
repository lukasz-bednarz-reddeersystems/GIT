#' @include sql_query.r
NULL

#######################################
#
# VirtualSQLInsertHandler Class
#
#######################################

#' Virtual S4 class handling VirtualSQLQuery objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualSQLQuery or derived classes
#'
#' @slot sql_insert object of class "VirtualSQLQuery"

setClass(
  Class          = "VirtualSQLInsertHandler",
  slots = c(
    sql_insert = "VirtualSQLQuery"
  ),
  contains = c("VIRTUAL")
)

#' Get sql_insert stored in object
#'
#' Returns sql_insert object of class "VirtualSQLQuery"
#'
#' @param object object of class "VirtualSQLInsertHandler"
#' @return \code{sql_insert} object of class "VirtualSQLQuery"
#' @export

setGeneric("getSQLInsertObject", function(object){standardGeneric("getSQLInsertObject")})

#' @describeIn  getSQLInsertObject
#' Get sql_insert stored in object
#'
#' Returns sql_insert object of class "VirtualSQLQuery"
#'
#' @inheritParams getSQLInsertObject
#' @return \code{sql_insert} object of class "VirtualSQLQuery"
#' @export
setMethod("getSQLInsertObject",
          signature(object = "VirtualSQLInsertHandler"),
          function(object){
            return(object@sql_insert)
          }
)

#' Set sql_insert object in object slot
#'
#' Public method to set sql_insert slot with "VirtualSQLQuery"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualSQLInsertHandler"
#' @param sql_insert object of class "VirtualSQLQuery"
#' @return \code{object} object of class "VirtualSQLInsertHandler"
#' @export

setGeneric("setSQLInsertObject", function(object,sql_insert){standardGeneric("setSQLInsertObject")})


#' Set sql_insert object in object slot
#'
#' Private method to set sql_insert slot with "VirtualSQLQuery"
#'
#' @param object object of class "VirtualSQLInsertHandler"
#' @param sql_insert object of class "VirtualSQLQuery"
#' @return \code{object} object of class "VirtualSQLInsertHandler"

setGeneric(".setSQLInsertObject", function(object,sql_insert){standardGeneric(".setSQLInsertObject")})

setMethod(".setSQLInsertObject",
          signature(object = "VirtualSQLInsertHandler", sql_insert = "VirtualSQLQuery"),
          function(object, sql_insert){
            object@sql_insert <- sql_insert
            return(object)
          }
)
