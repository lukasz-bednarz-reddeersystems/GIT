#' @include sql_procedure_call_functions.r
#' @include sql_query.r
NULL

####################################
#
# VirtualSQLProcedureCall Class
#
####################################


#' Virtual S4 class handling sql procedures calls.
#'
#' Implements handling of access to data via sql stored procedures calls.
#' Inherits from "VirtualSQLQuery"
#'
#' @slot arguments  = "character",
#' @slot procedure  = "character",
#' @export
setClass(
  Class    = "VirtualSQLProcedureCall",
  slots    = c(
    arguments    = "character",
    procedure    = "character"
  ),
  contains = c("VirtualSQLQuery", "VIRTUAL")
)


#' Get stored procedure arg names
#'
#' Returns names of arguments used by procedure(s).
#'
#' @param object object of class 'VirtualSQLProcedureCall'.
#' @return \code{arguments} character vector of names of procedure arguments

setGeneric(".getSQLProcedureArgumentNames", function(object){standardGeneric(".getSQLProcedureArgumentNames")})
setMethod(".getSQLProcedureArgumentNames",
          signature(object = "VirtualSQLProcedureCall"),
          function(object){
            return(object@arguments)
          }
)


#' Get stored procedure name
#'
#' Returns name of the stored procedure that needs to be executed.
#'
#' @param object object of class 'VirtualSQLProcedureCall'.
#' @return \code{procedure} character procedure name

setGeneric(".getSQLProcedureName", function(object){standardGeneric(".getSQLProcedureName")})
setMethod(".getSQLProcedureName",
          signature(object = "VirtualSQLProcedureCall"),
          function(object){
            return(object@procedure)
          }
)


#' Prepare SQL query strings
#'
#' Parses key_values to vector of SQL query strings
#'
#' @param object object of class 'VirtualSQLProcedureCall'.
#' @param key_values "data.frame" with query keys
#' @return \code{object} object of class 'VirtualSQLProcedureCall'.
#' @export
setMethod("prepareSQLQuery",
          signature(object = "VirtualSQLProcedureCall", key_values = "data.frame"),
          function(object, key_values){

            object <- .setSQLQueryKeyValues(object, key_values)
            key_values <- getSQLQueryKeyValues(object)

            parser <- .getSQLQueryKeyValuesParser(object)
            proc_n <- .getSQLProcedureName(object)
            args   <- .getSQLProcedureArgumentNames(object)

            parsed_keys <- parser(key_values)

            sql_strings <- generate_procedure_call_strings(proc_n, args, parsed_keys)

            object <- .setSQLQueryStrings(object, sql_strings)

            return(object)
          }
)



