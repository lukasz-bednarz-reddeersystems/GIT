#' @include datasource_client.r
#' @include datastore_client_functions.r
NULL


####################################
#
# VirtualDataStoreClient Class
#
####################################

#' Virtual S4 class implementing handling of data access.
#'
#' Implements handling of access to data via DataStore class.
#' Inherits from "VirtualDataSourceClient"
#'
#' @slot datastore_name           = "character",  name of datastore
#' @export

setClass(
  Class                = "VirtualDataStoreClient",
  slots = c(
    datastore_name     = "character" # name of datastore which will be querried
    ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

#' Get source datastore name
#'
#' Returns name of the datastore which will be querried when asked to fill data.
#'
#' @param object object of class 'VirtualDataStoreClient'.
#' @return \code{datastore_name} character
#' @export

setGeneric("getDataStoreName", function(object){standardGeneric("getDataStoreName")})

#' @describeIn getDataStoreName
#' Get source datastore name
#'
#' Returns name of the datastore which will be querried when asked to fill data.
#'
#' @inheritParams getDataStoreName
#' @return \code{datastore_name} character
#' @export
setMethod("getDataStoreName",
          signature(object = "VirtualDataStoreClient"),
          function(object){
            return(object@datastore_name)
          }
)

#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualDataStoreClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualDataStoreClient'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualDataStoreClient", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object,key_values)
            datastore <- getDataStoreName(object)
            values <- getDataSourceReturnColumnNames(object)

            colnames(key_values) <- .translateDataSourceColumnNames(object,
                                                                    colnames(key_values))

            # data request sent to dataplex
            query_data <- data_request(datastore ,key_values,values)
            query_data <- getData(query_data)


            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }
            query_data <- query_data[values]

            # translating column names
            colnames(query_data) <- .translateDataSourceColumnNames(object,
                                                                    colnames(query_data))


            # storing Reference data internaly
            object <- setReferenceData(object, query_data)


            if(hasNonNAColumnNames(object)){
              object <- .removeNAReferenceData(object)

            }

            if(hasFactorColumnNames(object)){
              object <- .transformReferenceData(object)

            }

            return(object)
          }
)

