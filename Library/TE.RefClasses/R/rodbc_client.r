#' @include datasource_client.r
#' @include rodbc_client_functions.r
NULL

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
  contains = c("VirtualDataSourceClient",
               "VirtualSQLQueryHandler",
                "VIRTUAL")
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


#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualRODBCClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualRODBCClient'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualRODBCClient", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object,key_values)
            values <- getDataSourceReturnColumnNames(object)
            sql_query <- getSQLQueryObject(object)

            sql_query <- prepareSQLQuery(sql_query, key_values)

            # data request sent to dataplex

            query_data <- tryCatch({
              executeSQLQuery(sql_query)
            }, error = function(cond) {
              stop(sprintf("Error in call to executeSQLQuery() of class %s in dataRequest() of class %s : \n%s",
                           class(sql_query), class(object), cond))
            })

            if (is.null(query_data) || 0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent via", class(sql_query), "returned zero row data.frame"))
              query_data <- .generateDataFilledWithNA(object)
              # stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }

            # translating column names
            colnames(query_data) <- .translateDataSourceColumnNames(object, colnames(query_data))


            if (!is.null(query_data)){
              query_data <- query_data[values]
            }

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

