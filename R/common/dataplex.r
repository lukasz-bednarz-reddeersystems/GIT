sourceTo("../common/factor_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/event_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/ext_pos_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dealing_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_levels_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/instrument_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/instrument_history_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/instrument_sector.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/instrument_country.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/instrument_price.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_instrument_exposure_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_factor_returns_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/allocation_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/watchlist_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Global singleton datastores
if(exists("dataplex_created")==FALSE){
  initialise_data_store <- function(){

    data_map <<- list()

    static_factors_1 <<- new("StaticFactorDataStore")
    data_map[[1]] <<- "static_factors_1"

    dynamic <<- new("DynamicFactorDataStore")
    data_map[[2]] <<- "dynamic"
    
    events <<- new("EventDataStore")
    data_map[[3]] <<- "events"
    
    ext_pos <<- new("ExtPosDataStore")
    data_map[[4]] <<- "ext_pos"

    dealing <<- new("DealingDataStore")
    data_map[[5]] <<- "dealing"

    trade_levels <<- new("TradeLevelsDataStore")
    data_map[[6]] <<- "trade_levels"

    instrument_details <<- new("InstrumentDataStore")
    data_map[[7]] <<- "instrument_details"

    instrument_history <<- new("InstrumentHistoryDataStore")
    data_map[[8]] <<- "instrument_history"

    instrument_country <<- new("InstrumentCountryDataStore")
    data_map[[9]] <<- "instrument_country"

    instrument_price <<- new("InstrumentPriceDataStore")
    data_map[[10]] <<- "instrument_price"

    instrument_sector <<- new("InstrumentSectorDataStore")
    data_map[[11]] <<- "instrument_sector"

    risk_instrument_exposure <<- new("RiskInstrumentExposuresDataStore")
    data_map[[12]] <<- "risk_instrument_exposure"

    risk_factor_returns <<- new("RiskFactorReturnsDataStore")
    data_map[[13]] <<- "risk_factor_returns"

    trader_allocation <<- new("TraderAllocationDataStore")
    data_map[[14]] <<- "trader_allocation"

    watchlist <<- new("WatchListDataStore")
    data_map[[15]] <<- "watchlist"
 
    names(data_map) <<-  c("factor_datastore","dynamic_factor_datastore",
                           "event_datastore",
                           "ext_pos_datastore",
                           "dealing_datastore",
                           "trade_levels",
                           "instrument_details","instrument_history","instrument_country","instrument_price","instrument_sector",
                           "risk_instrument_exposure","risk_factor_returns",
                           "trader_allocation",
                           "watchlist")
  }
  initialise_data_store()
  #ToDo: Put datastore operations/acces into singleton class
  
  #Global data access function
  data_request <- function(store,keys,variables){
    rval <- NULL
    eval_str <- paste(data_map[[store]],"<<-queryStore(",data_map[[store]],",keys,variables)",sep="")
    rtn_str  <- paste("getLastResult(",data_map[[store]],")",sep="")
    eval(parse(text=eval_str))
    rval <- eval(parse(text=rtn_str))
    return (rval)
  }
  dataplex_created <- TRUE
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