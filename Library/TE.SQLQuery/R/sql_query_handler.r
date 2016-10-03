#' @include sql_query.r
NULL

#######################################
#
# VirtualSQLQueryHandler Class
#
#######################################

#' Virtual S4 class handling VirtualSQLQuery objects
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualSQLQuery or derived classes
#'
#' @slot sql_query object of class "VirtualSQLQuery"

setClass(
  Class          = "VirtualSQLQueryHandler",
  slots = c(
    sql_query = "VirtualSQLQuery"
  ),
  contains = c("VIRTUAL")
)

#' Get sql_query stored in object
#'
#' Returns sql_query object of class "VirtualSQLQuery"
#'
#' @param object object of class "VirtualSQLQueryHandler"
#' @return \code{sql_query} object of class "VirtualSQLQuery"
#' @export

setGeneric("getSQLQueryObject", function(object){standardGeneric("getSQLQueryObject")})

#' @describeIn  getSQLQueryObject
#' Get sql_query stored in object
#'
#' Returns sql_query object of class "VirtualSQLQuery"
#'
#' @inheritParams getSQLQueryObject
#' @return \code{sql_query} object of class "VirtualSQLQuery"
#' @export
setMethod("getSQLQueryObject",
          signature(object = "VirtualSQLQueryHandler"),
          function(object){
            return(object@sql_query)
          }
)

#' Set sql_query object in object slot
#'
#' Public method to set sql_query slot with "VirtualSQLQuery"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualSQLQueryHandler"
#' @param sql_query object of class "VirtualSQLQuery"
#' @return \code{object} object of class "VirtualSQLQueryHandler"
#' @export

setGeneric("setSQLQueryObject", function(object,sql_query){standardGeneric("setSQLQueryObject")})


#' Set sql_query object in object slot
#'
#' Private method to set sql_query slot with "VirtualSQLQuery"
#'
#' @param object object of class "VirtualSQLQueryHandler"
#' @param sql_query object of class "VirtualSQLQuery"
#' @return \code{object} object of class "VirtualSQLQueryHandler"

setGeneric(".setSQLQueryObject", function(object,sql_query){standardGeneric(".setSQLQueryObject")})

setMethod(".setSQLQueryObject",
          signature(object = "VirtualSQLQueryHandler", sql_query = "VirtualSQLQuery"),
          function(object, sql_query){
            object@sql_query <- sql_query
            return(object)
          }
)
