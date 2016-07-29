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
    warehouse    = "environment"
  ),
  prototype = list(
    datastore    = new.env()
  )
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


#Global singleton datastores
if(exists("dataplex_created")==FALSE){
  initialise_data_store <- function(){

    dataplex <- new("DataPlex")

    data_map <- getDataPlexWarehouse(dataplex)

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

    watchlist <- new("WatchListDataStore")
    data_map[["watchlist"]] <- "watchlist"
  }
  initialise_data_store()
  #ToDo: Put datastore operations/acces into singleton class

  dataplex_created <- TRUE
  devtools::use_data(dataplex_created, overwrite = TRUE)

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


if(exists("warehouse_store_created")==FALSE){

  all_stores <- new.env(parent=emptyenv())

  refresh <- function(name){
    if(name%in%ls(all_stores)==FALSE){
      message("Warehouse not stored, creating new store")
    }
    eval_str <- paste("all_stores[['",name,"']]<<-warehouse_objectstore_factory('",name,"')",sep="")
    eval(parse(text=eval_str))
  }

  #Global trade data store
  warehouse_request <- function(name,trader,start,end){
    refresh(name)
    all_stores[[name]] <- queryWarehouseStore(all_stores[[name]],trader,start,end)
    warehouse <- getWarehouseFromStore(all_stores[[name]],trader,start,end)
    all_stores[[name]] <- NULL
    return(warehouse)
  }

  warehouse_push_features <- function(name,warehouse,replace_features=FALSE){
    refresh(name)
    all_stores[[name]] <- pushFeatures(all_stores[[name]],warehouse,keep_old=!replace_features)
    commitWarehouseStore(all_stores[[name]])
    all_stores[[name]] <- NULL
  }

  warehouse_push_summary <- function(name,warehouse){
    refresh(name)
    all_stores[[name]] <- pushSummary(all_stores[[name]],warehouse)
    commitWarehouseStore(all_stores[[name]])
    all_stores[[name]] <- NULL
  }

  warehouse_store_created <- TRUE
}

if(exists("analysis_store_created")==FALSE){

  analysis_store_request <- function(key_function){
    key_hash <- as.character(murmur3.32(as.character(key_function())))
    kv <- key_function()
    hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
    name <- paste("analysis_store_",hrname,sep="")
    return(analysis_objectstore_factory(name))
  }

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
    already_stored <- isAnalysisStored(store@warehouse_q,key)
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
