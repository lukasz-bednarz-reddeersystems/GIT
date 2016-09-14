library(lubridate)
library(hash)
library(digest)
library(hashFunction)

####################################
#
# AnalysisClient Class
# Stores instances of the AnalysisBlock class
#
####################################

setClass(
  Class                = "VirtualAnalysisClient",
  slots                = c(analysis_class = "character", analysis_block = "VirtualAnalysisBlock"),
  prototype = list(
    key_cols = c("analysis_class", "TraderID", "start", "end")
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)
#To instantiate the object the analysis class slot should be set

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

setGeneric("getAnalysisBlock", function(object,...){standardGeneric("getAnalysisBlock")})
# Returns name of the underlying analysis block class.
#
# Args:
#   object : object of type "VirtualAnalysisClient"
# Returns:
#   analysis block
setMethod("getAnalysisBlock",  
          signature(object = "VirtualAnalysisClient"),
          function(object){
            return(object@analysis_block)
          }
)

setMethod("dataRequest",
          signature(object = "VirtualAnalysisClient", key_values = "data.frame"),
          function(object, key_values, force = FALSE){

            key <- key_values
            analysis <- getAnalysisClass(object)
            key_with_class <- cbind(data.frame(analysis_class = analysis), key_values)
            colnames(key_with_class) <- getDataSourceQueryKeyColumnNames(object)
            object <- .setDataSourceQueryKeyValues(object,key_with_class[getDataSourceQueryKeyColumnNames(object)])

            store_id <- get_analysis_objectstore_name(key_with_class)
            
            analysis_store <- analysis_objectstore_factory(store_id)
            kh <- as.character(murmur3.32(as.character(key_values)))
            analysis_block <- queryAnalysisStore(analysis_store,data.frame(key_hash=kh,analysis_module=object@analysis_class))
            
            if (is.null(analysis_block)) { 
              if(force){
                analysis_block <- new(analysis)
                analysis_block <- dataRequest(analysis_block,key_with_class[colnames(key_with_class)!='analysis_class'])
                analysis_block <- Process(analysis_block)
                analysis_store <- updateAnalysisStore(analysis_store,analysis_block,data.frame(key_hash=kh,analysis_module=object@analysis_class),force=force)
                analysis_store <- commitAnalysisStore(analysis_store)
                query_data     <- getOutputGGPlotData(analysis_block)
                object@analysis_block <- analysis_block
              }
              else{
                stop(message(paste("No instance of",analysis,"found in store, either build it, check the key, or run with force=TRUE.")))
              }
            } else {
              query_data <- getOutputGGPlotData(analysis_block)
              object@analysis_block <- analysis_block
            }
            
            if (nrow(query_data) == 0) {
              query_data <- .generateDataFilledWithNA(object, trader, start, end)
            }
            
            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }
            
            # forcing new variables set
            object <- .setRequiredVariablesNames(object, colnames(query_data))
            object <- .setStoredVariablesNames(object, colnames(query_data))
            
            # storing Reference data internaly
            object <- setReferenceData(object, query_data)
            
            return(object)
          }
)

#blockRequest