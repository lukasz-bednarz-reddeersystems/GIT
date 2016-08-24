#' @include objectstore.r
#' @include keymap.r
#' @include global_configs.r
#' @include common_composite_warehouse.r
NULL


#' An S4 class handling queries to WarehouseObjectstore.
#'
#' @export

setClass(
	Class = "WarehouseQuery",
	prototype = prototype(
		#fields need to match column names
		#of key data frame
		fields = c('id','start','end')
	),
	contains =c("ObjectQuery")
)


#' An S4 class for storing daily risk models.
#'
#' Inherits from "VirtualObjectStore"
#'
#' @slot key_map  "KeyMap"
#' @slot warehouse_q "WarehouseQuery"
#'
#' @export

setClass(
	Class = "WarehouseObjectStore",
	representation  = representation(
		key_map     = "KeyMap",
		warehouse_q = "WarehouseQuery"
	),
	prototype       = prototype(
		data_path   = model_defaults@data_path,
		warehouse_q = new("WarehouseQuery")
	),
	contains = c("VirtualObjectStore")
)

setGeneric("generateKey",function(object,trader_id,start,end){standardGeneric("generateKey")})
setMethod("generateKey","WarehouseObjectStore",
		  function(object,trader_id,start,end){
		  	keya <- data.frame(id=trader_id,date=start)
		  	colnames(keya) <- c('id','date')
		  	keyb <- data.frame(id=trader_id,date=end)
		  	colnames(keyb) <- c('id','date')
	        key <- rbind(keya,keyb)
	        return(key)
		  }
)

#' Get warehouse from WarehouseObjectstore for given keys
#'
#' Retrieves from WarehouseObjectstore warehouse matching
#' keys passed as arguments.
#'
#' @param object object of class "WarehouseObjectStore"
#' @param trader_id "integer", trader ID
#' @param start "Date" start date of Warehouse
#' @param end "Date" end date of Warehouse
#' @return \code{wh} object of class "CompositeWarehouse" if present otherwise NULL
#' @export

setGeneric("getWarehouseFromStore",function(object,trader_id,start,end){standardGeneric("getWarehouseFromStore")})

#' @describeIn getWarehouseFromStore
#' Get warehouse from WarehouseObjectstore for given keys
#'
#' Retrieves from WarehouseObjectstore warehouse matching
#' keys passed as arguments.
#'
#' @inheritParams getWarehouseFromStore
#' @return \code{wh} object of class "CompositeWarehouse" if present otherwise NULL
#' @export
setMethod("getWarehouseFromStore","WarehouseObjectStore",
	      function(object,trader_id,start,end){
	      	key <- generateKey(object,trader_id,start,end)
	      	got_data <- isKeyKnown(object@warehouse_q,key)
		  	if(got_data==FALSE){
		  		message("Data item has not been stored, run a query first.")
		  		wh <- NULL
		  	}
		  	else{
		  		object@key_map <- mapFields(object@key_map,key)
	      		object@warehouse_q <- getCurrentKeyQuery(object@key_map,object@warehouse_q)
	      		nme <- getIdentifier(object@warehouse_q)
	      		wh <- getWarehouse(getFromObjectStore(object,object@id),nme)
		  	}
	      	return(wh)
	      }
)


#' Query WarehouseObjectstore for given keys
#'
#' Updates Warehouse if it doesn't contain data.
#'
#' @param object object of class "WarehouseObjectStore"
#' @param trader_id "integer", trader ID
#' @param start "Date" start date of Warehouse
#' @param end "Date" end date of Warehouse
#' @return \code{object} object object of class "WarehouseObjectStore"
#' @export

setGeneric("queryWarehouseStore",function(object,trader_id,start,end){standardGeneric("queryWarehouseStore")})

#' @describeIn queryWarehouseStore
#' Query WarehouseObjectstore for given keys
#'
#' Updates Warehouse if it doesn't contain data.
#'
#' @inheritParams queryWarehouseStore
#' @return \code{object} object object of class "WarehouseObjectStore"
#' @export

setMethod("queryWarehouseStore","WarehouseObjectStore",
		  function(object,trader_id,start,end){

		    key <- tryCatch({
		  			generateKey(object,trader_id,start,end)
		  		   },error=function(cond){
		  		   	stop(paste('Key generation failure during warehouse object store query:',cond))
		  		   })
		  	got_data <- isKeyKnown(object@warehouse_q,key)
		  	if(got_data==FALSE){
		  		message("Key not found, updating ...")
		  		object <- updateWarehouseStore(object,key)
		  		object@warehouse_q <- updateKnownKeys(object@warehouse_q,key)
		  		commitWarehouseStore(object)
		  	}
		  	else{
		  		message("Key found in store.")
		  	}
		  	return(object)
		  }
)


#' Update WarehouseObjectstore for given keys
#'
#' Updates Warehouse if it doesn't contain data.
#'
#' @param object object of class "WarehouseObjectStore"
#' @param keys "data.frame", keys to update for
#' @return \code{object} object object of class "WarehouseObjectStore"

setGeneric("updateWarehouseStore",function(object,keys){standardGeneric("updateWarehouseStore")})
setMethod("updateWarehouseStore","WarehouseObjectStore",
		  function(object,keys){
		  	message("Creating new warehouse objects... ")

		    object@key_map <- mapFields(object@key_map,keys)
		  	for(keys_row in 1:numberKeyValues(object@key_map)){
		  	      object@warehouse_q <- getCurrentKeyQuery(object@key_map,object@warehouse_q)
		  	      trader_id  <- as.integer(getQueryValueByField(object@warehouse_q,"id"))
		  	      start_date <- as.Date(getQueryValueByField(object@warehouse_q,"start"))
		  	      end_date <- as.Date(getQueryValueByField(object@warehouse_q,"end"))
		  	      new_warehouse <- tryCatch({
		  	          build_warehouse(trader_id,start_date,end_date)
		  	      	},error=function(cond){
		  	      		message(paste("Error building trade warehouse during object store query:",cond))
		  	      		stop("Could not create required TradeWarehouse object.")
		  	      	})
              object <- updateWarehouseStoreForKey(object,new_warehouse)
              object@key_map <- advanceCurrentKey(object@key_map)
        	}
       		return(object)
		  }
)


#' Adds Warehouse to Warehouse Objectstore
#'
#' @param object object of class "WarehouseObjectStore"
#' @param new_warehouse object of class "CompositeWarehouse"
#' @return \code{object} object object of class "WarehouseObjectStore"

setGeneric("updateWarehouseStoreForKey",function(object,new_warehouse){standardGeneric("updateWarehouseStoreForKey")})
setMethod("updateWarehouseStoreForKey","WarehouseObjectStore",
		  function(object,new_warehouse){
		  	message("Updating warehouse store ...")
		  	old_warehouse <- getFromObjectStore(object,object@id)
		  	name <- getIdentifier(object@warehouse_q)
		  	if(length(old_warehouse)==0){
		  		cw <- new("CompositeWarehouse")
		  		cw <- addWarehouse(cw,new_warehouse,name)
		  		object <- placeInObjectStore(object,cw,object@id)
		  	}
		  	else if(class(old_warehouse)[[1]]=="CompositeWarehouse"){
		  		 old_warehouse <- addWarehouse(old_warehouse,new_warehouse,name)
		  		 object <- placeInObjectStore(object,old_warehouse,object@id)
		  	}
		  	else{
		  		stop(paste("Invalid warehouse object of class",class(old_warehouse)[[1]]))
		  	}
		  	return(object)
		 }
)



#' Get KeyMap ID
#'
#' @param object object of class "WarehouseObjectStore"
#' @return \code{keymap_id} "character"

setGeneric("getKeyMapID",function(object){standardGeneric("getKeyMapID")})
setMethod("getKeyMapID","WarehouseObjectStore",
	      function(object){
	      	return(paste(object@id,"_keymap",sep=""))
	      }
)


#' Get QueryID ID
#'
#' @param object object of class "WarehouseObjectStore"
#' @return \code{objectquery_id} "character"

setGeneric("getQueryID",function(object){standardGeneric("getQueryID")})
setMethod("getQueryID","WarehouseObjectStore",
	      function(object){
	      	return(paste(object@id,"_objectquery",sep=""))
	      }
)


#' Get QueryID ID
#'
#' @param object object of class "WarehouseObjectStore"
#' @return \code{objectquery_id} "character"

setGeneric("commitWarehouseStore",function(object){standardGeneric("commitWarehouseStore")})
setMethod("commitWarehouseStore","WarehouseObjectStore",
		  function(object){
		  	object <- placeInObjectStore(object,object@key_map,getKeyMapID(object))
		  	object <- placeInObjectStore(object,object@warehouse_q,getQueryID(object))
		  	saveObject(object)
		  }
)

# !!! Comented unused method with missing dependency tearDownFeatures !!!
#
# setGeneric("tearDownAllFeatures",function(object,trader_id,start,end){standardGeneric("tearDownAllFeatures")})
# setMethod("tearDownAllFeatures","WarehouseObjectStore",
# 			function(object,trader_id,start,end){
# 			  wh <- getWarehouseFromStore(object,trader_id,start,end)
# 			  if(length(wh)>0){
# 			  	wh <- tearDownFeatures(wh,wh@features)
# 			  	object <- placeInObjectStore(object,wh,object@id)
# 			  }
# 			  return(object)
# 			}
# )

setGeneric("pushFeatures",function(object,warehouse,keep_old=TRUE){standardGeneric("pushFeatures")})
setMethod("pushFeatures","WarehouseObjectStore",
		  function(object,warehouse,keep_old=TRUE){
		  	if(keep_old){
		  		message("Keeping previous features, only new features will be stored.")
		  	}
		  	else{
		  		message("Replacing all features.")
		  	}
		  	wh <- getFromObjectStore(object,object@id)
		  	if(class(wh)[[1]]!="CompositeWarehouse")stop("Attempt to push features to a non CompositeWarehouse object.")
		  	wh <- addFeatures(wh,warehouse,keep_old)
		  	object <- placeInObjectStore(object,wh,object@id)
		  	return(object)
		  }
)

setGeneric("pushSummary",function(object,warehouse){standardGeneric("pushSummary")})
setMethod("pushSummary","WarehouseObjectStore",
		  function(object,warehouse){
		  	wh <- getFromObjectStore(object,object@id)
		  	if(class(wh)[[1]]!="CompositeWarehouse")stop("Attempt to push features to a non CompositeWarehouse object.")
		  	wh <- replaceSummary(wh,warehouse)
		  	object <- placeInObjectStore(object,wh,object@id)
		  	return(object)
		  }
)

setGeneric("pushTradeDailyData",function(object,warehouse,keep_old=TRUE){standardGeneric("pushTradeDailyData")})
setMethod("pushTradeDailyData","WarehouseObjectStore",
		  function(object,warehouse,keep_old=TRUE){
		  	if(keep_old){
		  		message("Keeping previous daily data, only new features data be stored.")
		  	}
		  	else{
		  		message("Replacing all trade daily data.")
		  	}
		  	wh <- getFromObjectStore(object,object@id)
		  	if(class(wh)[[1]]!="CompositeWarehouse")stop("Attempt to push features to a non CompositeWarehouse object.")
		  	wh <- addDailyData(wh,warehouse,keep_old)
		  	object <- placeInObjectStore(object,wh,object@id)
		  	return(object)
		  }
)

setGeneric("pushTradeFields",function(object,warehouse,fields){standardGeneric("pushTradeFields")})
setMethod("pushTradeFields","WarehouseObjectStore",
		  function(object,warehouse, fields){
		  	wh <- getFromObjectStore(object,object@id)
		  	if(class(wh)[[1]]!="CompositeWarehouse")stop("Attempt to push features to a non CompositeWarehouse object.")
		  	wh <- copyTradeFields(wh,warehouse,fields)
		  	object <- placeInObjectStore(object,wh,object@id)
		  	return(object)
		  }
)

#' Create WarehouseObjectStore object
#'
#' Factory function for WarehouseObjectStore objects.
#' Creates new WarehouseObjectStore object for given filename.
#' If file already exists loads file content to memory.
#'
#' @param name 'character', name of the objectstore file
#' @return \code{whstr} object of class "WarehouseObjectStore"
#' @export

warehouse_objectstore_factory <- function(name){
	message("Initialising trade warehouse store...")
	whstr <- new("WarehouseObjectStore",id=name)
	whstr@key_map <- new("KeyMap",key_columns=c('id','date'),key_generator=date_trader_kgen_fn)
	pth <- getPath(whstr)
	if(file.exists(pth)){
		message(paste("Found warehouse store at",pth))
		whstr <- loadObject(whstr)
		whstr@key_map <- getFromObjectStore(whstr,getKeyMapID(whstr))
		whstr@warehouse_q <- getFromObjectStore(whstr,getQueryID(whstr))
	}
	else{
		message(paste("No previous store data found at",pth,"new store created."))
	}
	return(whstr)
}
