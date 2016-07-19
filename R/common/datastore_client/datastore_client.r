sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(hash)
library(plyr)


####################################
#
# VirtualDataStoreClient Class
#
####################################

setClass(
  Class                = "VirtualDataStoreClient",
  slots = c(
    datastore_name     = "character" # name of datastore which will be querried
    ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)



setGeneric("getDataStoreName", function(object,...){standardGeneric("getDataStoreName")})
# Returns name of the datastore which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualDataStoreClient"
# Returns:
#   datastore_name

setMethod("getDataStoreName", 
          signature(object = "VirtualDataStoreClient"),
          function(object){
            return(object@datastore_name)
          }
)

setMethod("dataRequest",  
          signature(object = "VirtualDataStoreClient", key_values = "data.frame"),
          function(object, key_values){
            non_na_cols <- getNonNAColumnNames(object)
            colnames_map <- getDataSourceClientColumnNameMap(object)
            object <- .setDataSourceQueryKeyValues(object,key_values)
            datastore <- getDataStoreName(object)
            values <- getDataSourceReturnColumnNames(object)
            factor_cols <- getFactorColumnNames(object)
            
            
            # data request sent to dataplex
            query_data <- data_request(datastore ,key_values,values)
            query_data <- getData(query_data)
            query_data <- query_data[values]
            
            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }
            
            # translating column names
            colnames(query_data) <- values(colnames_map[values])[values]
            
            # storing Reference data internaly
            object <- setReferenceData(object, query_data)
            
            
            if(length(non_na_cols) > 0){
              object <- .removeNAReferenceData(object)
              
            }
            
            if(length(factor_cols) > 0){
              object <- .transformReferenceData(object)
              
            } 
            
            return(object)
          }
)

