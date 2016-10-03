#' @include common_factor_datastore.r
#' @include common_event_datastore.r
#' @include common_ext_pos_datastore.r
#' @include common_dealing_datastore.r
#' @include common_warehouse_objectstore.r
#' @include common_analysis_objectstore.r
#' @include common_trade_levels_datastore.r
#' @include common_instrument_datastore.r
#' @include common_instrument_history_datastore.r
#' @include common_instrument_sector.r
#' @include common_instrument_country.r
#' @include common_instrument_price.r
#' @include common_risk_instrument_exposure_datastore.r
#' @include common_risk_factor_returns_datastore.r
#' @include common_allocation_datastore.r
#' @include common_watchlist_datastore.r
NULL


setClass(
  Class          = "DataPlex",
  slots = c(
    warehouse        = "environment"
  ),
  prototype = list(
    warehouse    = new.env()
  )
)

setGeneric("resetMemory",function(object){standardGeneric("resetMemory")})
setMethod("resetMemory","DataPlex",
          function(object){
            rm(list = ls(object@warehouse))
            object@warehouse <- new.env()
            return(object)
          }
)

setGeneric("getDataPlexWarehouse",function(object){standardGeneric("getDataPlexWarehouse")})
setMethod("getDataPlexWarehouse","DataPlex",
          function(object){
            return(object@warehouse)
          }
)

setGeneric("getDataPlexStore",function(object, store){standardGeneric("getDataPlexStore")})
setMethod("getDataPlexStore","DataPlex",
          function(object, store){
            wh <- getDataPlexWarehouse(object)

            return(wh[[store]])
          }
)

setGeneric("setDataPlexStoreValue",function(object, store, value){standardGeneric("setDataPlexStoreValue")})
setMethod("setDataPlexStoreValue","DataPlex",
          function(object, store, value){
            wh <- getDataPlexWarehouse(object)
            wh[[store]] <- value
            return(object)
          }
)

setGeneric("isDataPlexInitialized",function(object){standardGeneric("isDataPlexInitialized")})
setMethod("isDataPlexInitialized","DataPlex",
          function(object){
            wh <- getDataPlexWarehouse(object)

            is_initialized = FALSE

            if (exists("is_initialized", envir = wh)) {
              is_initialized <- wh$is_initialized
            }
            return(is_initialized)
          }
)

# setGeneric(".setDataPlexInitialized",function(object, store, value){standardGeneric(".setDataPlexInitialized")})
# setMethod(".setDataPlexInitialized",
#           signature(object = "DataPlex", is_initialized = "logical"),
#           function(object, is_initialized){
#             wh <- getDataPlexWarehouse(object)
#             wh$is_initialized <- is_initialized
#
#             return(object)
#           }
# )


setGeneric("initializeDataPlex",function(object){standardGeneric("initializeDataPlex")})
setMethod("initializeDataPlex","DataPlex",
          function(object){
            data_map <- getDataPlexWarehouse(object)

            data_map[["factor_datastore"]] <- new("StaticFactorDataStore")

            data_map[["dynamic_factor_datastore"]] <- new("DynamicFactorDataStore")

            data_map[["event_datastore"]] <- new("EventDataStore")

            data_map[["ext_pos_datastore"]] <- new("ExtPosDataStore")

            data_map[["dealing_datastore"]] <- new("DealingDataStore")

            data_map[["trade_levels"]] <- new("TradeLevelsDataStore")

            data_map[["instrument_details"]] <- new("InstrumentDataStore")

            data_map[["instrument_history"]] <- new("InstrumentHistoryDataStore")

            data_map[["instrument_country"]] <- new("InstrumentCountryDataStore")

            data_map[["instrument_price"]] <- new("InstrumentPriceDataStore")

            data_map[["instrument_sector"]] <- new("InstrumentSectorDataStore")

            data_map[["risk_instrument_exposure"]] <- new("RiskInstrumentExposuresDataStore")

            data_map[["risk_factor_returns"]] <- new("RiskFactorReturnsDataStore")

            data_map[["trader_allocation"]] <- new("TraderAllocationDataStore")

            data_map[["watchlist"]] <- new("WatchListDataStore")

            data_map[["is_initialized"]] <- TRUE

            return(object)
          }
)

#' Initialization method for DataPlex
#' Populates DataPlex with respective DataStores
#'
#' @param .Object object of class "DataPlex"
#' @return \code{.Object} object of class "DataPlex"
setMethod("initialize","DataPlex",
          function(.Object){
            if(!isDataPlexInitialized(.Object)){
              .Object <- initializeDataPlex(.Object)
            }
            return(.Object)
          }
)

#' function to reset DataPlex
initialise_data_store <- function(){
  dataplex <- new("DataPlex")
  dataplex <- resetMemory(dataplex)
  dataplex <- initializeDataPlex(dataplex)
}




#' Request data from datastore
#'
#' Global data access function
#'
#' @param store character, store name.
#' @param keys data.frame store query keys
#' @param variables character vector of variables to pull from store
#' @return \code{rval} data.frame, result of query
#' @export

data_request <- function(store,keys,variables){
  rval <- NULL
  dataplex <- new("DataPlex")
  str_obj <- getDataPlexStore(dataplex, store)

  str_obj <- queryStore(str_obj,keys,variables)
  rval  <- getLastResult(str_obj)

  dataplex <- setDataPlexStoreValue(dataplex, store, str_obj)

  return (rval)
}




setClass(
  Class          = "WarehouseCache",
  slots = c(
    cache    = "environment"
  ),
  prototype = list(
    cache    = new.env()
  )
)

setGeneric("getWarehouseObjectstoreCache",
           function(object){standardGeneric("getWarehouseObjectstoreCache")})

setMethod("getWarehouseObjectstoreCache","WarehouseCache",
          function(object){
            return(object@cache)
          }
)

setGeneric("getWarehouseObjectstoreCacheItem",
           function(object, name){standardGeneric("getWarehouseObjectstoreCacheItem")})

setMethod("getWarehouseObjectstoreCacheItem",
          signature(object = "WarehouseCache",
                    name = "character"),
          function(object, name){
            cache <- getWarehouseObjectstoreCache(object)

            return(cache[[name]])
          }
)


setGeneric("isWarehouseObjectstoreCacheItemPresent",
           function(object, name){standardGeneric("isWarehouseObjectstoreCacheItemPresent")})

setMethod("isWarehouseObjectstoreCacheItemPresent",
          signature(object = "WarehouseCache",
                    name = "character"),
          function(object, name){
            cache <- getWarehouseObjectstoreCache(object)

            return(name %in% ls(cache))
          }
)


setGeneric("setWarehouseObjectstoreCacheItemValue",
           function(object, name, value){standardGeneric("setWarehouseObjectstoreCacheItemValue")})

setMethod("setWarehouseObjectstoreCacheItemValue",
          signature(object = "WarehouseCache",
                    name = "character",
                    value = "ANY"),
          function(object, name, value){
            cache <- getWarehouseObjectstoreCache(object)
            cache[[name]] <- value
            return(object)
          }
)


#' Refresh Warehouse Cache
#'
#' Creates new instance of WarehouseObjectstore and
#' stores it in cache
#'
#' @param name warehouse objectstore name

refresh <- function(name) {

  wh_cache <- new("WarehouseCache")

  if(isWarehouseObjectstoreCacheItemPresent(wh_cache, name)==FALSE){
    message("Warehouse not stored, creating new store")
  }
  wh_cache <- setWarehouseObjectstoreCacheItemValue(wh_cache,
                                                    name,
                                                    warehouse_objectstore_factory(name))
}

##############################
#
# Global trade data store
#
##############################

#' Query Warehouse for given keys
#'
#' Queries Warehouse for data for given keys.
#' Query goes via warehouse objectstore cache.
#'
#' @param name "character" warehouse objectstore name
#' @param trader "integer" trader ID
#' @param start "Date" start date of warehouse
#' @param end "Date" end date of warehouse
#' @return \code{warehouse} object of type TradeWarehouse if present NULL otherwise
#'
#' @export

warehouse_request <- function(name,trader,start,end){
  refresh(name)

  wh_cache <- new("WarehouseCache")
  wh_str <- getWarehouseObjectstoreCacheItem(wh_cache, name)

  wh_str <- queryWarehouseStore(wh_str,trader,start,end)
  warehouse <- getWarehouseFromStore(wh_str,trader,start,end)

  wh_cache <-  setWarehouseObjectstoreCacheItemValue(wh_cache,
                                                     name,
                                                     NULL)
  return(warehouse)
}

warehouse_push_features <- function(name,warehouse,replace_features=FALSE){
  refresh(name)

  wh_cache <- new("WarehouseCache")
  wh_str <- getWarehouseObjectstoreCacheItem(wh_cache, name)

  wh_str <- pushFeatures(wh_str, warehouse, keep_old=!replace_features)
  commitWarehouseStore(wh_str)

  wh_cache <-  setWarehouseObjectstoreCacheItemValue(wh_cache,
                                                     name,
                                                     NULL)
}

warehouse_push_summary <- function(name,warehouse){
  refresh(name)

  wh_cache <- new("WarehouseCache")
  wh_str <- getWarehouseObjectstoreCacheItem(wh_cache, name)

  wh_str <- pushSummary(wh_str,warehouse)
  commitWarehouseStore(wh_str)

  wh_cache <-  setWarehouseObjectstoreCacheItemValue(wh_cache,
                                                     name,
                                                     NULL)
}


#' get analysis store for given key function
#'
#' @param key_function "function" generating key
#' @return \code{an_store} object of class "AnalysisObjectStore"
#' @export
analysis_store_request <- function(key_function){
  key_hash <- as.character(murmur3.32(as.character(key_function())))
  kv <- key_function()
  hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
  name <- paste("analysis_store_",hrname,sep="")
  return(analysis_objectstore_factory(name))
}


#' query for analysis module for specific set of parameters
#'
#' @param key_function "function" generating key
#' @param name_or_builder "character" name of the objectstore or "function" builder of the objectstore
#' @param force "logical" should Analysis be create if not existing in store
#' @return \code{an_store} object of class "AnalysisObjectStore"
#' @export
analysis_module_request <- function(key_function,name_or_builder,force=FALSE){
  if(class(name_or_builder)=='character'){
    analysis_name <- name_or_builder
  }
  else{
    module_class <- tryCatch({createAnalysisModule(name_or_builder,key_function)},error=function(cond){stop(paste("Error getting analysis object:",cond[['message']]))})
    analysis_name <- class(module_class)[[1]]
  }
  store <- analysis_store_request(key_function)
  key_hash <- as.character(murmur3.32(as.character(key_function())))
  key <- data.frame(key_hash=key_hash,analysis_module=analysis_name)
  query <- getObjectStoreQuery(store)
  already_stored <- isAnalysisStored(query,key)
  error_free <- tryCatch({
                            message("Query:")
                            analysis <- queryAnalysisStore(store,key)
                            if(length(analysis)>0){
                              message("Update:")
                              store <- updateAnalysisStore(store,analysis,key,force=force)
                              TRUE
                            }
                            else{
                              message("Analysis query returned NULL object, no commit made.")
                              FALSE
                            }
                         },error=function(cond){
                            message(paste('Analysis store: Error during query/update:',cond[['message']]))
                            return(FALSE)
                         })
  message("Commit:")
  if(!already_stored){
    if(error_free){
      commitAnalysisStore(store)
    }
    else{
      message("Error during analysis store query, no commit made.")
    }
  }
  else{
    message("No new data built, no commit made.")
  }
  return(analysis)
}


#' Load and bind analysis output from multiple stores
#'
#' Queries analysis store objects for requested dates.
#' Retrieves or updates analysis and binds results together.
#'
#' @param traders integer, trader ID's.
#' @param dates Date vector of dates
#' @param name_or_builder function or function name of builder used
#' to generate analysis module
#' @param key_fn function, key generator
#' @return \code{posn_comp_obj} object of class 'PositionComposite'.
#' @export

analysis_module_load_multiple <- function(traders,dates,name_or_builder,key_fn){
  first <- TRUE
  for(trader in traders){
    for(date in dates){
      key_func <- function(){key_fn(trader,date)}
      analysis <- analysis_module_request(key_func,name_or_builder)
      if(length(analysis)>0){
        if(first){
          all_data <- analysis@ppmdl@modeldata@data
          first <- FALSE
        }
        else{
          tryCatch({
                all_data <- unique(rbind.fill(all_data,analysis@ppmdl@modeldata@data))
            },error=function(cond){
                stop(paste("Error binding history data:",cond))
                #Could introduce ability to rebuild data here
            })
        }
      } else {
        message(paste("No object for trader",trader,"on",date))
      }
    }
  }
  return(all_data)
}

report_memory <- function(){
  return(sort(sapply(ls(pos = ".GlobalEnv"),function(x){object.size(get(x))})))
}

report_memory_object <- function(object_name){
  slts <- eval(parse(text=paste("slotNames(",object_name,")",sep="")))
  return(sort(sapply(paste(object_name,"@",slts,sep=""),function(x){eval(parse(text=paste("object.size(",x,")")))})))
}

#' Get trader allocation value
#'
#' Returns trader allocation value in USD.
#' Reads from "trader_allocation" datastore
#'
#' @param trader integer, trader ID.
#' @param start Date start date
#' @param end Date end date
#' @return \code{allocation} numeric, allocation value.
#' @export

get_trader_allocation <- function(trader,start,end){
  dts <- unique(format(seq(as.Date(start),as.Date(end),by='1 day'),'%Y-%m'))
  first <- TRUE
  for(d in dts){
    cd <- paste(as.character(d),'-01',sep="")
    a <- data_request("trader_allocation",data.frame(lUserID=trader,dtDateFrom=c(as.Date(cd),as.Date(cd))),c('dblValue'))
    if(first){
      allocation <- a@data
      first <- FALSE
    }
    else{
      allocation <- rbind(allocation,a@data)
    }
  }
  colnames(allocation) <- c('TraderID','Date','Allocation')
  return(allocation)
}
