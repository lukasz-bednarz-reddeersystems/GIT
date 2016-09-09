#' @include sql_query_functions.r
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
#' @slot db_name "character" ODBC name of the database
#' @slot db_schema "character" schema name
#' @slot sql_query "character" sql query string
#' @slot key_cols   "character" key column names of the query
#' @slot key_values "data.frame" key values
#' @slot query_parser "function" query parsing function
#' @slot results_parser "function" result parsing function
#' @slot column_name_map "hash" slot storing map hash for column names
#'
#' @export
setClass(
  Class     = "VirtualSQLQuery",
  slots     = c(
    db_name    = "character",
    db_schema  = "character",
    sql_query  = "character",
    key_cols   = "character",
    key_values = "data.frame",
    query_parser = "function",
    results_parser = "function",
    column_name_map = "hash"

  ),
  prototype = list(
    query_parser   = pass_thru_parser,
    results_parser = pass_thru_parser
  ),
  contains  = c("VIRTUAL")
)


#' Get db name
#'
#' Returns character name of dB to be used for queries.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{db_schema} character

setGeneric(".getSQLQueryDBName", function(object){standardGeneric(".getSQLQueryDBName")})
setMethod(".getSQLQueryDBName",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@db_name)
          }
)

#' Get schema name
#'
#' Returns character name of dB schema to be used for queries.
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{db_schema} character

setGeneric(".getSQLQuerySchemaName", function(object){standardGeneric(".getSQLQuerySchemaName")})
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

setGeneric(".getSQLQueryStrings", function(object){standardGeneric(".getSQLQueryStrings")})
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

setGeneric(".setSQLQueryStrings", function(object, queries){standardGeneric(".setSQLQueryStrings")})
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
#' @export
setGeneric("getSQLQueryKeyColumnNames", function(object){standardGeneric("getSQLQueryKeyColumnNames")})

#' @describeIn getSQLQueryKeyColumnNames
#' Get sql query column names
#'
#' Returns key column names that are used to construct sql querries.
#'
#' @inheritParams getSQLQueryKeyColumnNames
#' @return \code{key_cols} character vector of constructed SQL query strings.
#' @export
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

setGeneric(".setSQLQueryKeyValues", function(object, key_values){standardGeneric(".setSQLQueryKeyValues")})
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
#' @export

setGeneric("getSQLQueryKeyValues", function(object){standardGeneric("getSQLQueryKeyValues")})

#' @describeIn getSQLQueryKeyValues
#' Get sql query column names
#'
#' Returns key values datatable
#' with keys that will be matched in datastore query
#'
#' @inheritParams getSQLQueryKeyValues
#' @return \code{key_values} data.frame with columns matching key column names()
#' @export
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

setGeneric(".getSQLQueryKeyValuesParser", function(object){standardGeneric(".getSQLQueryKeyValuesParser")})
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

setGeneric(".getSQLQueryResultsParser", function(object){standardGeneric(".getSQLQueryResultsParser")})
setMethod(".getSQLQueryResultsParser",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@results_parser)
          }
)



#' Retreive SQL query column name map
#'
#' Returns hash with mapping of query column names
#' to return column names
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{column_name_map} hash column name map of the datasource to output names
#' @export

setGeneric("getSQLQueryColumnNameMap", function(object){standardGeneric("getSQLQueryColumnNameMap")})

#' @describeIn getSQLQueryColumnNameMap
#' Retreive SQL query column name map
#'
#' Returns hash with mapping of query column names
#' to return column names
#'
#' @inheritParams getSQLQueryColumnNameMap
#' @return \code{column_name_map} hash column name map of the datasource to output names
#' @export
setMethod("getSQLQueryColumnNameMap",
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@column_name_map)
          }
)


#' Translate datasource column names
#'
#' Pubic method to translate raw data column names
#' to required output names
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param colnames character vector with list of raw data column names
#' @return \code{ret_colnames} data source column names translated to
#' output column names

setGeneric(".translateSQLQueryColumnNames", function(object, colnames){standardGeneric(".translateSQLQueryColumnNames")})

setMethod(".translateSQLQueryColumnNames",
          signature(object = "VirtualSQLQuery", colnames = "character"),
          function(object, colnames){

            colnames_map <- getSQLQueryColumnNameMap(object)

            names_to_translate <- intersect(colnames, names(colnames_map))
            idx <- (colnames %in% names_to_translate)

            ret_colnames <- colnames
            ret_colnames[idx] <- values(colnames_map[names_to_translate])[names_to_translate]

            return(ret_colnames)
          }
)



#' Prepare SQL query strings
#'
#' Parses key_values to vector of SQL query strings
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param key_values "data.frame" with query keys
#' @export

setGeneric("prepareSQLQuery", function(object, key_values){standardGeneric("prepareSQLQuery")})


#' @describeIn prepareSQLQuery
#' Prepare SQL query strings
#'
#' Parses key_values to vector of SQL query strings
#'
#' @inheritParams prepareSQLQuery
#' @return \code{object} object of class 'VirtualSQLQuery'.
#' @export
setMethod("prepareSQLQuery",
          signature(object = "VirtualSQLQuery", key_values = "data.frame"),
          function(object, key_values){

            object <- .setSQLQueryKeyValues(object, key_values)
            parser <- .getSQLQueryKeyValuesParser(object)

            sql_strings <- parser(key_values)

            object <- .setSQLQueryStrings(object, sql_strings)

            return(object)
          }
)

#' Executes SQL query strings
#'
#' Executes previously prepared SQL queries and returns result
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param key_values data_frame with query keys. If missing will use
#' previously set keys or returns an error.
#' @return \code{ret_val} data.frame with result of the query
#' @export

setGeneric("executeSQLQuery", function(object, key_values){standardGeneric("executeSQLQuery")})

#' Executes SQL query strings
#'
#' Executes previously prepared SQL queries and returns result
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @return \code{ret_val} data.frame with result of the query
#' @export
setMethod("executeSQLQuery",
          signature(object = "VirtualSQLQuery", key_values = "missing"),
          function(object){

            queries <- .getSQLQueryStrings(object)
            db      <- .getSQLQueryDBName(object)
            schema  <- .getSQLQuerySchemaName(object)

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
                execute_sql_query(query, db, schema)
              }, error = function(cond){
                stop(sprintf("Error ocured when executing query of class %s, \n %s", class(object), cond) )
              })

              if (is(ret, "data.frame")) {
                if (first) {
                  ret_df <- ret
                  first <- FALSE
                } else {
                  ret_df <- rbind(ret_df, ret)
                }
              }

            }

            parser <- .getSQLQueryResultsParser(object)

            ret_df <- parser(ret_df)

            return(ret_df)
          }
)

#' Executes SQL query strings
#'
#' Executes previously prepared SQL queries and returns result
#'
#' @param object object of class 'VirtualSQLQuery'.
#' @param key_values data_frame with query keys. If missing will use
#' previously set keys or returns an error.
#' @return \code{ret_val} data.frame with result of the query
#' @export
setMethod("executeSQLQuery",
          signature(object = "VirtualSQLQuery", key_values = "data.frame"),
          function(object, key_values){

            object <- prepareSQLQuery(object, key_values)

            ret_df <- executeSQLQuery(object)

            return(ret_df)
          }
)


