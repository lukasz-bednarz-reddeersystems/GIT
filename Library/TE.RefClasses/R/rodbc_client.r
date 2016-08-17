#' @include datasource_client.r
#' @include rodbc_client_functions.r
NULL

####################################
#
# VirtualSQLQuery Class
#
####################################

#' Virtual S4 class sql queries handling.
#'
#' Implements handling of access to data via SQL queries.
#'
#' @slot db_schema  = "character",
#' @slot sql_query  = "character",
#' @slot key_cols   = "character",
#' @slot key_values = "data.frame",
#' @slot query_parser = "function",
#' @slot results_parser = "function"

setClass(
  Class     = "VirtualSQLQuery",
  slots     = c(
    db_schema  = "character",
    sql_query  = "character",
    key_cols   = "character",
    key_values = "data.frame",
    query_parser = "function",
    results_parser = "function"
  ),
  prototype = list(
    results_parser = pass_thru_parser
  ),
  contains  = c("VIRTUAL")
)

#' Get schema name
#'
#' Returns character name of dB schema to be used for queries.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{db_schema} character

setGeneric(".getSQLQuerySchemaName", function(object,...){standardGeneric(".getSQLQuerySchemaName")})
setMethod(".getSQLQuerySchemaName",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@db_schema)
          }
)


#' Get sql queries
#'
#' Returns character vector of constructed SQL query strings.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{sql_query} character vector of constructed SQL query strings.

setGeneric(".getSQLQueryStrings", function(object,...){standardGeneric(".getSQLQueryStrings")})
setMethod(".getSQLQueryStrings",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@sql_query)
          }
)


#' Set sql query strings
#'
#' Private method to set character vector of constructed SQL query strings.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param queries character vector of constructed SQL query strings.
#' @return \code{object} object of class 'VirtualSQLQuery'.

setGeneric(".setSQLQueryStrings", function(object, queries, ...){standardGeneric(".setSQLQueryStrings")})
setMethod(".setSQLQueryStrings",
          signature(object = "VirtualSQLQuery", queries = "character"),
          function(object, queries){
            object@sql_query <- queries
            return(object)
          }
)


#' Get sql query column names
#'
#' Returns key column names that are used to construct sql querries.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{key_cols} character vector of constructed SQL query strings.

setGeneric("getSQLQueryKeyColumnNames", function(object,...){standardGeneric("getSQLQueryKeyColumnNames")})
setMethod("getSQLQueryKeyColumnNames",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@key_cols)
          }
)


#' set sql query key values
#'
#' Private method to set values of key values datatable
#' with keys that will be matched in datastore query
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param key_values  data.frame with columns matching key column names()
#' @return \code{object} object of class 'VirtualSQLQuery'.

setGeneric(".setSQLQueryKeyValues", function(object, key_values, ...){standardGeneric(".setSQLQueryKeyValues")})
setMethod(".setSQLQueryKeyValues",
          signature(object = "VirtualSQLQuery", key_values = "data.frame"),
          function(object, key_values) {

            if (! has_required_columns(key_values, getSQLQueryKeyColumnNames(object))) {
              message(paste("Object", class(object), "in .setSQLQueryKeyValues()"))
              message(paste("Query key_values set do not contain required column names"))
              message(paste("Required columns are: ", paste0(getSQLQueryKeyColumnNames(object), collapse = ", ")))
              stop("Invalid column names of query keys passed to .setSQLQueryKeyValues().")
            } else if (0 == nrow(key_values)) {
              message(paste("Object", class(object), "in .setSQLQueryKeyValues()"))
              message(paste("Query key_values has zero rows"))
              stop("Zero row query keys data.frame passed to .setSQLQueryKeyValues().")
            }
            else {
              object@key_values <- key_values
              return(object)
            }
          }
)


#' Get sql query column names
#'
#' Returns key values datatable
#' with keys that will be matched in datastore query
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{key_values} data.frame with columns matching key column names()

setGeneric("getSQLQueryKeyValues", function(object,...){standardGeneric("getSQLQueryKeyValues")})
setMethod("getSQLQueryKeyValues",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@key_values)
          }
)

#' Get sql query column names
#'
#' Returns function that will be used to parse
#' query key values to SQL querry string(s).
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{parser} function used to parse the query keys

setGeneric(".getSQLQueryKeyValuesParser", function(object,...){standardGeneric(".getSQLQueryKeyValuesParser")})
setMethod(".getSQLQueryKeyValuesParser",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@query_parser)
          }
)


#' Get sql query column names
#'
#' Returns function that will be used to parse results of SQL query.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{results_parser} function used to parse the data

setGeneric(".getSQLQueryResultsParser", function(object,...){standardGeneric(".getSQLQueryResultsParser")})
setMethod(".getSQLQueryResultsParser",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@results_parser)
          }
)


#' Prepare SQL query strings
#'
#' Parses key_values to vector of SQL query strings
#'
#' @rdname VirtualSQLQuery-class
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{object} object of class 'VirtualSQLQuery'.

setGeneric("prepareSQLQuery", function(object, key_values, ...){standardGeneric("prepareSQLQuery")})


#' Executes SQL query strings
#'
#' Executes previously prepared SQL queries and returns result
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param key_values data_frame with query keys. If missing will use
#' previously set keys or returns an error.
#' @return \code{ret_val} data.frame with result of the query

setGeneric("executeSQLQuery", function(object, key_values, ...){standardGeneric("executeSQLQuery")})
setMethod("executeSQLQuery",
          signature(object = "VirtualSQLQuery", key_values = "missing"),
          function(object){

            queries <- .getSQLQueryStrings(object)

            schema <- .getSQLQuerySchemaName(object)

            if (length(schema) ==0) {
              schema = NULL
            }

            if(length(queries) == 0) (
              stop(sprintf("Tried to executeSQLQuery without valid queries set in class %s", class(object)))
            )

            ret_df <- NULL

            first = TRUE
            for(query in queries){

              ret <- tryCatch({
                execute_sql_query(query, schema)
              }, error = function(cond){
                stop(sprintf("Error ocured when executing query of class %s, \n %s", class(object), cond) )
              })

              if (is(ret, "data.frame")) {
                ret_df <- ret
                first <- FALSE
              } else {
                ret_df <- rbind(ret_df, ret)
              }

            }

            parser <- .getSQLQueryResultsParser(object)

            ret_df <- parser(ret_df)

            return(ret_df)
          }
)


setMethod("executeSQLQuery",
          signature(object = "VirtualSQLQuery", key_values = "data.frame"),
          function(object, key_values){

            object <- prepareSQLQuery(object, key_values)

            ret_df <- executeSQLQuery(object)

            return(ret_df)
          }
)


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

setClass(
  Class    = "VirtualSQLProcedureCall",
  slots    = c(
    arguments    = "character",
    procedure    = "character"
  ),
  contains = c("VirtualSQLQuery")
)


#' Get stored procedure arg names
#'
#' Returns names of arguments used by procedure(s).
#'
#' @param object object of class 'VirtualSQLProcedureCall'.
#' @return \code{arguments} character vector of names of procedure arguments

setGeneric(".getSQLProcedureArgumentNames", function(object,...){standardGeneric(".getSQLProcedureArgumentNames")})
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

setGeneric(".getSQLProcedureName", function(object,...){standardGeneric(".getSQLProcedureName")})
setMethod(".getSQLProcedureName",
          signature(object = "VirtualSQLProcedureCall"),
          function(object){
            return(object@procedure)
          }
)



setMethod("prepareSQLQuery",
          signature(object = "VirtualSQLProcedureCall", key_values = "data.frame"),
          function(object, key_values){

            object <- .setSQLQueryKeyValues(object, key_values)
            parser <- .getSQLQueryKeyValuesParser(object)
            proc_n <- .getSQLProcedureName(object)
            args   <- .getSQLProcedureArgumentNames(object)

            parsed_keys <- parser(key_values)

            sql_strings <- generate_procedure_call_strings(proc_n, args, parsed_keys)

            object <- .setSQLQueryStrings(object, sql_strings)

            return(object)
          }
)




####################################
#
# VirtualRODBCClient Class
#
####################################


#' Virtual S4 class wrapping VirtualSQLQuery.
#'
#' Implements handling of access to data via sql stored procedures calls.
#' Provides SQL data access interface to Reference Classes
#'
#' @slot sql_query  = "VirtualRODBCClient",

setClass(
  Class    = "VirtualRODBCClient",
  slots    = c(
    sql_query    = "VirtualSQLQuery"
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

#' Get SQL query Object
#'
#' Returns stored SQL query object that encapsulates query to DB
#'
#' @param object object of class 'VirtualRODBCClient'.
#' @return \code{sql_query} object of type "VirtualSQLQuery"

setGeneric(".getSQLQueryObject", function(object,...){standardGeneric(".getSQLQueryObject")})
setMethod(".getSQLQueryObject",
          signature(object = "VirtualRODBCClient"),
          function(object){
            return(object@sql_query)
          }
)


#' Set SQL query Object
#'
#' Sets stored SQL query object that encapsulates query to DB
#'
#' @param object object of class 'VirtualRODBCClient'.
#' @param sql_query bject of type "VirtualSQLQuery"
#' @return \code{object} object of class 'VirtualRODBCClient'.

setGeneric(".setSQLQueryObject", function(object, sql_query,...){standardGeneric(".setSQLQueryObject")})
setMethod(".setSQLQueryObject",
          signature(object = "VirtualRODBCClient", sql_query = "VirtualSQLQuery"),
          function(object, sql_query){

            object@sql_query <- sql_query

            return(object)
          }
)


setMethod(".generateDataFilledWithNA",
          signature(object = "VirtualRODBCClient"),
          function(object){

            ret_vars <- getDataSourceReturnColumnNames(object)
            ret_vars <- .translateDataSourceColumnNames(object, ret_vars)
            key_vals <- getDataSourceQueryKeyValues(object)

            diff <- setdiff(ret_vars, colnames(key_vals))

            ret_data <- cbind(key_vals, data.frame(t(rep(NA,length(diff)))))

            colnames(ret_data) <- c(ret_vars)

            return(ret_data)
          }
)


setMethod("dataRequest",
          signature(object = "VirtualRODBCClient", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object,key_values)
            values <- getDataSourceReturnColumnNames(object)
            sql_query <- .getSQLQueryObject(object)
            sql_query <- prepareSQLQuery(sql_query, key_values)

            # data request sent to dataplex

            query_data <- tryCatch({
              executeSQLQuery(sql_query)
            }, error = function(cond) {
              stop(sprintf("Error in call to executeSQLQuery() of class %s in dataRequest() of class %s : \n%s",
                           class(sql_query), class(object), cond))
            })

            query_data <- query_data[values]

            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent via", class(sql_query), "returned zero row data.frame"))
              query_data <- .generateDataFilledWithNA(object)
              # stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }

            # translating column names
            colnames(query_data) <- .translateDataSourceColumnNames(object, values)

            # storing Reference data internaly
            object <- setReferenceData(object, query_data)

            if(hasNonNAColumnNames(object) > 0){
              object <- .removeNAReferenceData(object)

            }

            if(hasFactorColumnNames(object) > 0){
              object <- .transformReferenceData(object)

            }


            return(object)
          }
)

