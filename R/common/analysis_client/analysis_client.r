sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_objectstore/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(lubridate)
library(hash)


####################################
#
# AnalysisClient Class
# Stores instances of the AnalysisBlock class
#
####################################

setClass(
  Class                = "VirtualAnalysisClient",
  slots                = c(analysis_class = "character"),
  prototype = list(
    key_cols = c("analysis_class", "id", "start", "end")

  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

setGeneric("getAnalysisClass", function(object,...){standardGeneric("getAnalysisClass")})
# Returns name of the underlying analysis block class.
#
# Args:
#   object : object of type "VirtualAnalysisClient"
# Returns:
#   analysis name

setMethod("getAnalysisClass",  
          signature(object = "VirtualAnalysisClient"),
          function(object){
            return(object@analysis_class)
          }
)

setMethod("dataRequest",  
          signature(object = "VirtualAnalysisClient", key = "data.frame", force=logical),
          function(object, key, force=FALSE){
            
            object <- .setDataSourceQueryKeyValues(object,key)
            
            non_na_cols <- getNonNAColumnNames(object)
            analysis <- getAnalysisClass(object)
            values <- getDataSourceReturnColumnNames(object)
            colnames_map <- getDataSourceClientColumnNameMap(object)
            
            key_with_class <- cbind(data.frame(analysis_class = analysis), key_with_class)
            store_id <- get_analysis_objectstore_name(key_with_class)
            
            analysis_store <- analysis_objectstore_factory(store_id)
            analysis_block <- queryAnalysisStore(analysis_store, key_with_class)
            
            if (is.null(analysis_block)) {
              if(force){
                analysis_block <- new(analysis)
                analysis_block <- dataRequest(analysis_block,key)
                analysis_store <- updateAnalysisStore(analysis_store,analysis_block,key)
                analysis_store <- commitAnalysisStore(analysis_store)
                query_data     <- getOutputlGGPlotData(analysis_block)
              }
              else{
                stop(message(paste("No instance of",analysis,"found in store, either build it, check the key, or run with force=TRUE.")))
              }
            } else {
              query_data <- getOutputGGPlotData(analysis_block)
            }
            
            if (nrow(query_data) == 0) {
              query_data <- .generateDataFilledWithNA(object, trader, start, end)
            }
            
            query_data[setdiff(values, colnames(query_data))] <- NA 
            query_data <- query_data[values]
            
            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }
            
            # translating column names
            colnames(query_data) <- .translateDataSourceColumnNames(object, values)
            
            # forcing new variables set
            object <- .setRequiredVariablesNames(object, colnames(query_data))
            object <- .setStoredVariablesNames(object, colnames(query_data))
            
            # storing Reference data internaly
            object <- setReferenceData(object, query_data)
            
            # remove rows undefined rows
            if(length(non_na_cols) > 0 ){
              object <- .removeNAReferenceData(object)
            }

            return(object)
          }
)

