sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/ppmodel_client/ppmodel_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/ppmodel_objectstore/ppmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(lubridate)
library(hash)


####################################
#
# VirtualPPModelClient Class
#
####################################

setClass(
  Class                = "VirtualPPModelClient",
  slots                = c(model_class = "character"),
  prototype = list(
    key_cols = c("model_class", "id", "start", "end")

  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

setGeneric("getPPModelClass", function(object,...){standardGeneric("getPPModelClass")})
# Returns post processing model used to assembly data.
#
# Args:
#   object : object of type "VirtualPPModelClient"
# Returns:
#   model name

setMethod("getPPModelClass",  
          signature(object = "VirtualPPModelClient"),
          function(object){
            return(object@model_class)
          }
)

setMethod("dataRequest",  
          signature(object = "VirtualPPModelClient", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            non_na_cols <- getNonNAColumnNames(object)
            model <- getPPModelClass(object)
            values <- getDataSourceReturnColumnNames(object)
            colnames_map <- getDataSourceClientColumnNameMap(object)
            
            key_values <- cbind(data.frame(model_class = model), key_values)
            
            store_ids <- get_ppmodel_objectstore_name(key_values)
            
            first <- TRUE
            
            for(i_row in seq(length(store_ids))) {
              trader <- as.integer(key_values$id[i_row])
              start <- as.Date(key_values$start[i_row])
              end <- as.Date(key_values$end[i_row])
              ppm_store_name <- store_ids[i_row]
              
              ppm_store <- ppmodel_objectstore_factory(store_ids[i_row])
              
              pp_model <- queryPPModelStore(ppm_store, key_values[i_row,])
              
              if (is.null(pp_model)) {
                
                pp_model <- new(model, keys = data.frame(id = trader, start = start, end = end) )
                
                pp_model <- tryCatch({
                  pp_model <- runPreProcessorModel(pp_model)
                  pp_model
                }, error = function(cond){
                  message(paste("Error occured during update PP model in dataRequest() of class" , class(object)))
                  message(sprintf("during query for trader: %s, start: %s, end: %s" , trader, start, end))
                  message(cond)
                  pp_model
                })
                
                ppm_store <- updatePPModelStore(ppm_store, pp_model,  key_values[i_row,])
                ppm_store <- commitPPModelStore(ppm_store)
                
                query_data <- getData(pp_model@modeldata)
                

              } else {
                query_data <- getData(pp_model@modeldata)
              }
              
              if (nrow(query_data) == 0) {
                query_data <- .generateDataFilledWithNA(object, trader, start, end)
              }
              
              if (first) {
                ret_data <- query_data
                first <- FALSE
              } else {
                
                
                if (!setequal(colnames(ret_data), colnames(query_data))) {
                  query_data[setdiff(colnames(ret_data), colnames(query_data))] <- NA
                }
                
                ret_data <- rbind(ret_data, query_data)
              }
              
            }
            
            ret_data[setdiff(values, colnames(ret_data))] <- NA 
            ret_data <- ret_data[values]
            
            if (0 == nrow(ret_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }
            
            # translating column names
            colnames(ret_data) <- .translateDataSourceColumnNames(object, values)
            
            # forcing new variables set
            object <- .setRequiredVariablesNames(object, colnames(ret_data))
            object <- .setStoredVariablesNames(object, colnames(ret_data))
            
            # storing Reference data internaly
            object <- setReferenceData(object, ret_data)
            
            
            # remove rows undefined rows
            if(length(non_na_cols) > 0 ){
              object <- .removeNAReferenceData(object)
              
            }

            return(object)
          }
)

