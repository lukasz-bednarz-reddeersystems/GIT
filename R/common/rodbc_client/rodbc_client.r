sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/rodbc_client/rodbc_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(hash)
library(plyr)


####################################
#
# VirtualSQLQuery Class
#
####################################

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


if (!isGenericS4(".getSQLQuerySchemaName")){
  setGeneric(".getSQLQuerySchemaName", function(object,...){standardGeneric(".getSQLQuerySchemaName")})
}
# Returns character name of dB schema to be used for queries.
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   queries : character vector of constructed SQL query strings.

setMethod(".getSQLQuerySchemaName",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@db_schema)
          }
)

if (!isGenericS4(".getSQLQueryStrings")){
  setGeneric(".getSQLQueryStrings", function(object,...){standardGeneric(".getSQLQueryStrings")})
}
# Returns character vector of constructed SQL query strings.
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   queries : character vector of constructed SQL query strings.

setMethod(".getSQLQueryStrings",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@sql_query)
          }
)


if (!isGenericS4(".setSQLQueryStrings")){
  setGeneric(".setSQLQueryStrings", function(object, queries, ...){standardGeneric(".setSQLQueryStrings")})
}
# Returns character vector of constructed SQL query strings.
#
# Args:
#   object : object of type "VirtualSQLQuery"
#   queries : character vector of constructed SQL query strings.
# Returns:
#   object : object of type "VirtualSQLQuery"

setMethod(".setSQLQueryStrings",  
          signature(object = "VirtualSQLQuery", queries = "character"),
          function(object, queries){
            object@sql_query <- queries
            return(object)
          }
)


if (!isGenericS4("getSQLQueryKeyColumnNames")){
setGeneric("getSQLQueryKeyColumnNames", function(object,...){standardGeneric("getSQLQueryKeyColumnNames")})
}
# Returns key column names that are used to construct sql querries.
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   list of key column names

setMethod("getSQLQueryKeyColumnNames",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@key_cols)
          }
)

if (!isGenericS4(".setSQLQueryKeyValues")){
setGeneric(".setSQLQueryKeyValues", function(object, key_values, ...){standardGeneric(".setSQLQueryKeyValues")})
}
# Set values of key values datatable with keys that will be matched in datastore query
#
# Args:
#   object : object of type "VirtualSQLQuery"
#   key_values : data.frame with columns matching key column names()
# Returns:
#   object : object of type "VirtualSQLQuery"

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

if (!isGenericS4("getSQLQueryKeyValues")){
setGeneric("getSQLQueryKeyValues", function(object,...){standardGeneric("getSQLQueryKeyValues")})
}
# Returns name of the datastore which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   datastore_name

setMethod("getSQLQueryKeyValues",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@key_values)
          }
)

if (!isGenericS4(".getSQLQueryKeyValuesParser")){
setGeneric(".getSQLQueryKeyValuesParser", function(object,...){standardGeneric(".getSQLQueryKeyValuesParser")})
}
# Returns function that will be used to parse query key values to SQL querry string(s).
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   parser : function used to parse the query keys

setMethod(".getSQLQueryKeyValuesParser",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@query_parser)
          }
)

if (!isGenericS4(".getSQLQueryResultsParser")){
setGeneric(".getSQLQueryResultsParser", function(object,...){standardGeneric(".getSQLQueryResultsParser")})
}
# Returns function that will be used to parse results ofq SQL query.
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   parser : function used to parse the data

setMethod(".getSQLQueryResultsParser",  
          signature(object = "VirtualSQLQuery"),
          function(object){
            return(object@results_parser)
          }
)



if (!isGenericS4("prepareSQLQuery")){
setGeneric("prepareSQLQuery", function(object, key_values, ...){standardGeneric("prepareSQLQuery")})
}
# parses key_values to vector of SQL query strings
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   object : object of type "VirtualSQLQuery"


if (!isGenericS4("executeSQLQuery")){
  setGeneric("executeSQLQuery", function(object, key_values, ...){standardGeneric("executeSQLQuery")})
}
# executes previously prepared SQL queries and returns result
#
# Args:
#   object : object of type "VirtualSQLQuery"
# Returns:
#   ret_val : typicaly data.frame with result of the query

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
setClass(
  Class    = "VirtualSQLProcedureCall",
  slots    = c(
    arguments    = "character",
    procedure    = "character"
  ),
  contains = c("VirtualSQLQuery")
)

if (!isGenericS4(".getSQLProcedureArgumentNames")){
setGeneric(".getSQLProcedureArgumentNames", function(object,...){standardGeneric(".getSQLProcedureArgumentNames")})
}
# Returns names of arguments used by procedure(s).
#
# Args:
#   object : object of type "VirtualSQLProcedureCall"
# Returns:
#   arguments : names of procedure arguments

setMethod(".getSQLProcedureArgumentNames",  
          signature(object = "VirtualSQLProcedureCall"),
          function(object){
            return(object@arguments)
          }
)

if (!isGenericS4(".getSQLProcedureName")){
setGeneric(".getSQLProcedureName", function(object,...){standardGeneric(".getSQLProcedureName")})
}
# Returns name of the stored procedure that needs to be executed.
#
# Args:
#   object : object of type "VirtualSQLProcedureCall"
# Returns:
#   procedure : procedure name

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

setClass(
  Class    = "VirtualRODBCClient",
  slots    = c(
    sql_query    = "VirtualSQLQuery"
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)


if (!isGenericS4(".getSQLQueryObject")){
  setGeneric(".getSQLQueryObject", function(object,...){standardGeneric(".getSQLQueryObject")})
}
# Returns name of the stored SQL query object that encapsulates query to DB
#
# Args:
#   object : object of type "VirtualRODBCClient"
# Returns:
#   sql_query : object of type "VirtualSQLQuery"

setMethod(".getSQLQueryObject",  
          signature(object = "VirtualRODBCClient"),
          function(object){
            return(object@sql_query)
          }
)


if (!isGenericS4(".setSQLQueryObject")){
  setGeneric(".setSQLQueryObject", function(object, sql_query,...){standardGeneric(".setSQLQueryObject")})
}
# Sets SQL Query object to new value
#
# Args:
#   object : object of type "VirtualRODBCClient"
# Returns:
#   object : object of type "VirtualRODBCClient"

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

