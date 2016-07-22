sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model/risk_model_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/daily_riskmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(hash)

#########################################
#
# VirtualRiskModelObjectstoreClient Class
#
#########################################

risk_model_objecstore_keys <-  c("Date")

setClass(
  Class                = "VirtualRiskModelObjectstoreClient",
  slots = c(
    component          = "character" # name of datastore which will be querried
    ),
  prototype = list(
    key_cols        = risk_model_objecstore_keys,
    key_values      = data.frame(Date = as.Date(character()))
  ),
  contains = c("VirtualDataSourceClient","VirtualRiskModelHandler", "VIRTUAL")
)


if (!isGenericS4("getRiskModelObjectstoreComponentName")) {
  setGeneric("getRiskModelObjectstoreComponentName", function(object,...){standardGeneric("getRiskModelObjectstoreComponentName")})
}
# Returns name of the objectstore component which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualRiskModelObjectstoreClient"
# Returns:
#   component_name

setMethod("getRiskModelObjectstoreComponentName", 
          signature(object = "VirtualRiskModelObjectstoreClient"),
          function(object){
            return(object@component)
          }
)


if (!isGenericS4(".generateQueryKeyValues")) {
  setGeneric(".generateQueryKeyValues", function(object, key_values,...){standardGeneric(".generateQueryKeyValues")})
}
# Transforms original query key values to objectstore specific keys
#
# Args:
#   object : object of type "VirtualRiskModelObjectstoreClient"
# Returns:
#   query_key_values

setMethod(".generateQueryKeyValues", 
          signature(object = "VirtualRiskModelObjectstoreClient",
                    key_values = "data.frame"),
          function(object, key_values){
            
            query_key_values <- unique(key_values["Date"])
            query_key_values$start <- query_key_values$Date - day(query_key_values$Date )
            query_key_values$end <- (query_key_values$Date - day(query_key_values$Date ) + 
                                    days_in_month(query_key_values$Date))
            query_key_values <- unique(query_key_values[c("start", "end")])
            
            return(query_key_values)
          }
)



setMethod("dataRequest",  
          signature(object = "VirtualRiskModelObjectstoreClient", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object, key_values)
            
            key_values <- getDataSourceQueryKeyValues(object)
            
            colnames(key_values) <- .translateDataSourceColumnNames(object, colnames(key_values))
            
            model_prefix <- getRiskModelName(object)
            lookback     <- getRiskModelLookback(object)
            component    <- getRiskModelObjectstoreComponentName(object)
            
            query_key_vals <- .generateQueryKeyValues(object, key_values)
            merge_cols   <- colnames(key_values)
            
            first <- TRUE
            
            for(key_idx in seq(nrow(query_key_vals))) {
              
              start        <- as.Date(query_key_vals$start[key_idx])
              end          <- as.Date(query_key_vals$end[key_idx])
              
              rm_str       <- get_most_recent_model_objectstore(model_prefix, end, lookback)
              
              if (is.null(rm_str)) next()
              
              name         <- getID(rm_str)
              
              query_data   <- queryDailyRiskModelObjectStore(rm_str,name,lookback,component)
              
              query_data   <- getData(query_data) 
                
              query_data   <- query_data[query_data$Date >= start & query_data$Date <= end, ]
              
              if (first) {
                ret_data <- query_data
                first <- FALSE
              } 
              else {
                
                ret_data <- rbind(ret_data, query_data[!(query_data$Date %in% unique(ret_data$Date)), ])
              }
              
            }
            
            ret_data$Index <- seq(nrow(ret_data))
            
            ret_data   <- merge(key_values, 
                                  ret_data, 
                                  all.x = TRUE, sort = FALSE)
            
            ret_data <- ret_data[order(ret_data$Index), setdiff(colnames(ret_data), "Index")]
            
            
            # translating column names
            colnames(ret_data) <- .translateDataSourceColumnNames(object, colnames(ret_data))
            
            # storing Reference data internaly
            object <- setReferenceData(object, ret_data)
            
            return(object)
          }
)

