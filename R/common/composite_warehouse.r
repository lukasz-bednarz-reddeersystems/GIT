sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
	Class                = "CompositeWarehouse",
	representation       = representation(
		warehouse_names  = "character",
		orig_trader_ids  = "integer",
		orig_instruments = "list",
		orig_positions   = "list",
		orig_psn_smries  = "list",
		orig_dates       = "list",
		orig_trade_ids   = "list",
		orig_pads        = "list",
		orig_maps        = "list",
		orig_dtr_stores  = "list"
	),
	prototype(
		n_warehouses     = as.integer(0)
	),
	contains = c("TradeWarehouse")
)

#Prototype is made only once and then copied with each call to 'new' 
#for objects along the same branch in the class hierachy.
#Must ensure new environments created since they are passed
#by reference.
setMethod("initialize", "CompositeWarehouse",
          function(.Object){
            .Object@trades <- new.env(parent = emptyenv())
            .Object
          }
)

join_trades <- function(current_trades, new_trades){
	new_ids <- ls(new_trades)
	current_ids <- ls(current_trades)
	for(id in new_ids){
		if(id %in% current_ids){
			instrument_trades <- unlist(Map(function(obj)return(obj@trade_id),current_trades[[id]]))
			nmes <- instrument_trades
			tl <- current_trades[[id]]
	      	cnt <- length(tl)+1
			for(trade in new_trades[[id]]){
				if(trade@trade_id %in% instrument_trades){
				 message(paste("Found duplicate trade",trade@trade_id,"for instrument",id))	
				}
				else{
				 tl[[cnt]] <- trade
				 nmes <- c(nmes,trade@trade_id)
				 cnt <- cnt+1
				}
			}
			names(tl) <- nmes
			current_trades[[id]] <- tl
		}	
		else{
			current_trades[[id]] <- new_trades[[id]]
		}
	}
	return(current_trades)
}

append_variable <- function(current_list,new_variable,name){
	current_names <- names(current_list)
	current_list[[length(current_list)+1]] <- new_variable
	names(current_list) <- c(current_names,name)
	return(current_list)
}

setGeneric("appendWarehouseVars", function(object,warehouse,name){standardGeneric("appendWarehouseVars")})
setMethod("appendWarehouseVars","CompositeWarehouse",
		  function(object,warehouse,name){
		  	message("Appending warehouse variables ...") 
		  	object@orig_trader_ids  <- append_variable(object@orig_trader_ids,warehouse@trader_id,name)
		  	object@orig_instruments <- append_variable(object@orig_instruments,listInstruments(warehouse),name)
		  	object@orig_positions   <- append_variable(object@orig_positions,warehouse@positions,name)
		  	object@orig_psn_smries  <- append_variable(object@orig_psn_smries,warehouse@psn_summary,name)
		  	object@orig_dates       <- append_variable(object@orig_dates,c(warehouse@start_date,warehouse@end_date),name)
		  	object@orig_trade_ids   <- append_variable(object@orig_trade_ids,listTrades(warehouse),name)
		  	object@orig_pads        <- append_variable(object@orig_pads,warehouse@dly_data_pad,name)
		  	object@orig_maps        <- append_variable(object@orig_maps,warehouse@map,name)
		  	object@orig_dtr_stores  <- append_variable(object@orig_dtr_stores,warehouse@fctr_datstr,name)
		  	return(object)
		  }
)

setGeneric("setWarehouseVars", function(object,warehouse){standardGeneric("setWarehouseVars")})
setMethod("setWarehouseVars","CompositeWarehouse",
	  	  function(object,warehouse){
	  	  	message("Setting composite warehouse variables ...")
	  	  	object@instruments <- as.numeric(ls(object@trades))
	  	  	if(length(object@trader_id) == 0){
	  	  		object@trader_id  <- warehouse@trader_id
	  	  	} else if(object@trader_id == warehouse@trader_id){
	  	  		object@trader_id  <- warehouse@trader_id
	  	  	} 
	  	  	else{
	  	  		object@trader_id  <- warehouse@trader_id
	  	  		message(paste("Composite_warehouse contains multiple trader ids, using",warehouse@trader_id))
	  	  	}
	  	  	object@start_date <- min(as.Date(unlist(lapply(object@orig_dates,as.character))))
	  	  	object@end_date   <- max(as.Date(unlist(lapply(object@orig_dates,as.character))))
	  	  	object@dly_data_pad <- warehouse@dly_data_pad
	  	  	return(object)
	  	  }
)

setGeneric("generateMap", function(object){standardGeneric("generateMap")})
setMethod("generateMap","CompositeWarehouse",
		  function(object){
		  	message("Generating composite warehouse map ...")
		  	object@map <- list()
		  	instruments <- ls(object@trades)
		  	for(instrument in instruments){
		  		trade_list <- object@trades[[instrument]]
		  		dex <- 1
		  		for(trade in trade_list){
		  			object <- tryCatch({
		  				updateMap(object,trade@trade_id,instrument,dex)
		  			},error = function(cond){
		  				message(paste("Could not map trade",trade@trade_id,"for instrument",instrument,":",cond))
		  				return(object)
		  			})
		  			dex <- dex + 1
		  		}
		  	}
		  	return(object)
		  }
)

setGeneric("addWarehouse", function(object,warehouse,name){standardGeneric("addWarehouse")})
setMethod("addWarehouse","CompositeWarehouse",
		  function(object,warehouse,name){
		  	if(name %in% object@warehouse_names)stop(paste("Warehouse",name,"already found in the composite object."))
		  	message("Appending warehouse to composite warehouse ... ")
		  	if(length(object@trades) > 0){
		  		object@trades <- join_trades(object@trades,warehouse@trades)
		  	}
		  	else{
		  		object@trades <- warehouse@trades
		  	}
		  	object <- appendWarehouseVars(object,warehouse,name)
		  	object <- setWarehouseVars(object,warehouse)
		  	object@warehouse_names <- c(object@warehouse_names,name)
		  	object <- generateMap(object)
		  	object <- createPositionSummary(object)
		    return(object)
		  }
)

setGeneric("getWarehouse", function(object,name){standardGeneric("getWarehouse")})
setMethod("getWarehouse","CompositeWarehouse",
	      function(object,name){
	      	message(paste("Getting warehouse",name,"..."))
	      	tw <- new("TradeWarehouse")
	      	get_trades <- object@orig_trade_ids[[name]]
	      	instruments <- object@orig_instruments[[name]]
	      	trd <- new.env(parent=emptyenv())
	      	for(instrument in instruments){
	      		nmes <- c()
	      		tl <- list()
	      		cnt <- 1
	      		all_trades <- getInstrumentTrades(object,instrument)
	      		for(trade in all_trades){
	      			if(trade@trade_id %in% get_trades){
	      				tl[cnt] <- trade
	      				nmes <- c(nmes,trade@trade_id)		
	      				cnt <- cnt + 1
	      			}
	      		}
	      		names(tl) <- nmes
	      		trd[[as.character(instrument)]] <- tl
	      	}
	      	message(paste("Got",length(get_trades),"trades."))
	      	tw@trades <- trd
	      	message(paste("Restoring warehouse variables ..."))
	      	tw@trader_id <- as.integer(object@orig_trader_ids[[name]])
	      	tw@instruments <- object@orig_instruments[[name]]
	      	tw@positions <- object@orig_positions[[name]]
	      	tw@psn_summary <- object@orig_psn_smries[[name]]
	      	tw@start_date <- as.Date(object@orig_dates[[name]][1])
	      	tw@end_date <- as.Date(object@orig_dates[[name]][2])
	      	tw@dly_data_pad <- as.integer(object@orig_pads[[name]])
	      	tw@map <- object@orig_maps[[name]]
	      	tw@fctr_datstr <- object@orig_dtr_stores[[name]]
	      	tw <- buildFeatureList(tw)
	      	return(tw)
	      }
)

setGeneric("getWarehouseSummary", function(object,name){standardGeneric("getWarehouseSummary")})
setMethod("getWarehouseSummary","CompositeWarehouse",
	      function(object,name){
	      	return(object@orig_psn_smries[[name]])
	      }
)

setGeneric("addFeatures", function(object,warehouse,keep_old=TRUE){standardGeneric("addFeatures")})
setMethod("addFeatures","CompositeWarehouse",
	      function(object,warehouse,keep_old=TRUE){
	      	if(class(warehouse)[[1]]!='TradeWarehouse')stop(message("Argument must be a TradeWarehouse to merge features."))
	      	message("Merging warehouse features ...")
	      	if(keep_old)message("Retaining previous versions of features for each trade.")
	      	trade_ids <- listTrades(warehouse)
	      	for(trade in trade_ids){
	      		message(paste("Looking for",trade,"..."))
	      		original_trade <- getTrade(object,trade)
	      		if(length(original_trade)>0){
	      			original_features <- original_trade@features
	      			new_trade <- getTrade(warehouse,trade)
	      			new_features <- new_trade@features
	      			message("Updating features ... ")
	      			for(feature in new_features){
	      				feature_nme <- class(feature)[[1]]
	      				if(length(original_features[[feature_nme]])>0){
	      					if(!keep_old)original_features[[feature_nme]]<-new_features[[feature_nme]]
	      				}
	      				else{
	      					original_features<-new_features	
	      				}
	      			}
	      			original_trade@features <- original_features
	      			object<-setTrade(object,original_trade)
	      		}
	      	}
	      	message("Feature merge complete.")
	      	return(object)
	      }
)

setGeneric("copyTradeFields", function(object,warehouse,fields){standardGeneric("copyTradeFields")})
setMethod("copyTradeFields","CompositeWarehouse",
		  function(object,warehouse,fields){
		  	if(class(warehouse)[[1]]!='TradeWarehouse')stop(message("Argument must be a TradeWarehouse to merge features."))
	      	message("Copying trade fields ...")
	      	trade_ids <- listTrades(warehouse)
	      	for(trade in trade_ids){
	      		message(paste("Looking for",trade,"..."))
	      		original_trade <- getTrade(object,trade)
	      		new_trade <- getTrade(warehouse,trade)
	      		if(length(original_trade)>0){
	      			for(f in fields){
	      				if((sum(slotNames(original_trade)==f)>0)&(sum(slotNames(new_trade)==f)>0)){
	      					tryCatch({
	      						slot(original_trade,f) <- slot(new_trade,f)
	      						},error=function(cond){
	      							stop(paste("Error copying",f,"on trade",trade,":",cond))
	      						})
	      					object<-setTrade(object,original_trade)	
	      				}
	      				else{
	      					message(paste("Field",f,"absent from original or replacement trade."))
	      				}	
	      			}
	      		}	
		  }
		  message("Trade field duplication complete.")
		  return(object)
		}
)

setGeneric("addDailyData", function(object,warehouse,keep_old=TRUE){standardGeneric("addDailyData")})
setMethod("addDailyData","CompositeWarehouse",
	      function(object,warehouse,keep_old=TRUE){
	      	if(class(warehouse)[[1]]!='TradeWarehouse')stop(message("Argument must be a TradeWarehouse to add daily data."))
	      	message("Merging warehouse trade daily data ...")
	      	if(keep_old)message("Retaining previous versions of daily data for each trade.")
	      	trade_ids <- listTrades(warehouse)
	      	for(trade in trade_ids){
	      		message(paste("Looking for",trade,"..."))
	      		original_trade <- getTrade(object,trade)
	      		if(length(original_trade)>0){
	      			original_data <- original_trade@daily_data
	      			new_trade <- getTrade(warehouse,trade)
	      			new_data <- new_trade@daily_data
	      			message("Updating data ... ")
					if(nrow(original_data@data)>0){
	      				if(!keep_old)original_data <- new_data
	      			}
	      			else{
	      				original_data<-new_data	
	      			}
	      		}
	      		original_trade@daily_data <- original_data
	      		object<-setTrade(object,original_trade)
	      	}
	      	message("Daily data update complete.")
	      	return(object)
	      }
)

setGeneric("replaceSummary", function(object,warehouse){standardGeneric("replaceSummary")})
setMethod("replaceSummary","CompositeWarehouse",
	function(object,warehouse){
		replace_to <- paste(warehouse@trader_id,"_",warehouse@start_date,"_",warehouse@end_date,sep="")
		if(!replace_to%in%object@warehouse_names)stop(paste("Warehouse",replace_to,"not found. Must add before replacing summary."))
		object@orig_psn_smries[[replace_to]] <- warehouse@psn_summary
		object@orig_positions[[replace_to]] <- warehouse@positions
		if(length(object@orig_psn_smries)>1)object <- createPositionSummary(object)
		return(object)
	}
)