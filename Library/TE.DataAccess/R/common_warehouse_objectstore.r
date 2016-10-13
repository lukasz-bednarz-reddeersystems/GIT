#' @include remote_objectstore.r
#' @include keymap.r
#' @include global_configs.r
#' @include common_composite_warehouse.r
NULL



setClass(
	Class = "VirtualWarehouseQuery",
	prototype = prototype(
		#fields need to match column names
		#of key data frame
		fields = c('id','start','end')
	),
	contains =c("ObjectQuery", "VIRTUAL")
)

#' An S4 class handling queries to WarehouseObjectstore.

setClass(
  Class = "WarehouseQuery",
  contains =c("VirtualWarehouseQuery")
)


setClass(
  Class = "RemoteWarehouseQuery",
  prototype = prototype(
    #fields need to match column names
    #of key data frame
    tb_name = "tRDTE_WarehouseObjectstore"
  ),
  contains =c("RemoteObjectQuery", "VirtualWarehouseQuery")
)




#' An S4 class for storing daily risk models.
#'
#' Inherits from "VirtualObjectStore"
#'
#' @slot key_map  "KeyMap"
#' @slot objectstore_q "WarehouseQuery"
#'
#' @export

setClass(
	Class = "WarehouseObjectStore",
	representation  = representation(
		key_map     = "KeyMap",
		objectstore_q = "RemoteWarehouseQuery"
	),
	prototype       = prototype(
		data_path   = model_defaults@data_path,
		objectstore_q = new("RemoteWarehouseQuery")
	),
	contains = c("VirtualRemoteObjectStore")
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "VirtualWarehouseQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemoteWarehouseQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "WarehouseQuery"),
          function(object, objectstore_q){

             # copy slots of Warehouse Query
            new_query <- new("RemoteWarehouseQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)

#' Get objectstore query object
#'
#' @param object object of class "WarehouseObjectStore"
#' @export

setGeneric("getObjectStoreKeyMap",function(object){standardGeneric("getObjectStoreKeyMap")})

#' @describeIn getObjectStoreKeyMap Get key map of the objectstore
#'
#' @inheritParams getObjectStoreKeyMap
#'
#' @return \code{key_map} "character" object of class "KeyMap"
#' @export
setMethod("getObjectStoreKeyMap","WarehouseObjectStore",
          function(object){
            return(object@key_map)
          }
)

#' Set objectstore key_map object
#'
#' Private method to store object key map object
#'
#' @rdname private_setObjectStoreKeyMap
#' @param object object of class "WarehouseObjectStore"
#' @param key_map object of class "KeyMap"

setGeneric(".setObjectStoreKeyMap",function(object, key_map){standardGeneric(".setObjectStoreKeyMap")})

setMethod(".setObjectStoreKeyMap",
          signature( object = "WarehouseObjectStore",
                     key_map = "KeyMap"),
          function(object, key_map){
            object@key_map <- key_map
            return(object)
          }
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

            query <- getObjectStoreQuery(object)

            got_data <- isKeyKnown(query,key)
            if(got_data==FALSE){
              message("Data item has not been stored, run a query first.")
              wh <- NULL
            }
            else{
              key_map <- getObjectStoreKeyMap(object)
              key_map <- mapFields(key_map,key)
              object <- .setObjectStoreKeyMap(object, key_map)

              query <- getCurrentKeyQuery(key_map,query)
              object <- .setObjectStoreQuery(object, query)

              nme <- getIdentifier(query)
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

		    query <-getObjectStoreQuery(object)
		  	got_data <- isKeyKnown(query,key)
		  	if(got_data==FALSE){
		  		message("Key not found, updating ...")
		  		object <- updateWarehouseStore(object,key)

		  		query <- updateKnownKeys(query, key)
		  		object <- .setObjectStoreQuery(object, query)

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

		    key_map <- getObjectStoreKeyMap(object)
		    key_map <- mapFields(key_map,keys)

		    for(keys_row in 1:numberKeyValues(key_map)){

		  	      query <- getObjectStoreQuery(object)
		  	      query <- getCurrentKeyQuery(key_map,query)
		  	      object <- .setObjectStoreQuery(object, query)

  	  	      trader_id  <- as.integer(getQueryValueByField(query,"id"))
		  	      start_date <- as.Date(getQueryValueByField(query,"start"))
		  	      end_date <- as.Date(getQueryValueByField(query,"end"))
		  	      new_warehouse <- tryCatch({
		  	          build_warehouse(trader_id,start_date,end_date)
		  	      	},error=function(cond){
		  	      		message(paste("Error building trade warehouse during object store query:",cond))
		  	      		stop("Could not create required TradeWarehouse object.")
		  	      	})
              object <- updateWarehouseStoreForKey(object,new_warehouse)
              key_map <- advanceCurrentKey(key_map)
              object <- .setObjectStoreKeyMap(object, key_map)
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
		  	name <- getIdentifier(getObjectStoreQuery(object))
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

setGeneric("commitWarehouseStore",function(object){standardGeneric("commitWarehouseStore")})
setMethod("commitWarehouseStore","WarehouseObjectStore",
		  function(object){

		  	object <- placeInObjectStore(object,
		  	                             getObjectStoreKeyMap(object),
		  	                             getKeyMapID(object))
		  	object <- placeInObjectStore(object,
		  	                             getObjectStoreQuery(object),
		  	                             getQueryID(object))

		  	ret <- updateTradesObjectStores(object)

		  	if (!ret) {
		  	  message("Some trades were not saved when updating trades objectstore")
		  	}

		  	saveObject(object)
		  }
)



setGeneric("updateTradesObjectStores",function(object){standardGeneric("updateTradesObjectStores")})
setMethod("updateTradesObjectStores","WarehouseObjectStore",
          function(object){

            wh <- getFromObjectStore(object,object@id)

            trades <- listTrades(wh)
            message(paste("Resetting features for",length(trades),"trades..."))
            for(trd_id in trades){
              trade <- getTrade(wh,trd_id)
              ret <- saveTradeInRemoteStore(trade)
            }
            return(ret)
          }
)


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
	whstr <- .setObjectStoreKeyMap(whstr,
	                               new("KeyMap",key_columns=c('id','date'),key_generator=date_trader_kgen_fn))

	query <- getObjectStoreQuery(whstr)
	pth <- getPath(whstr)

	if (!file.exists(pth)) {
	  message(sprintf("File initially not found in local path %s. Checking remote store",pth))
	  key <- key_from_name(basename(pth))
	  is_known <- isKeyKnownInRemoteStore(query, key)

	  if (is_known) {
	    whstr <- updateLocalStoreFile(whstr,key)
	  }
	}

	if(file.exists(pth)){
		message(paste("Found warehouse store at",pth))
		whstr <- loadObject(whstr)
		whstr <- .setObjectStoreKeyMap(whstr,
		                               getFromObjectStore(whstr,getKeyMapID(whstr))
		                               )
		whstr <- .setObjectStoreQuery(whstr,
		                              getFromObjectStore(whstr,getQueryID(whstr)))

	}

	else{
		message(paste("No previous store data found at",pth,"new store created."))
	}
	return(whstr)
}


#' Copy warehouses from local objectstores to remote store.
#'
#' copies all locally stored warehouses to remote store and updates keys
#'
#' @return \code{count} number of warehouses copied
#' @export

update_warehouse_remote_storage <- function(){
  message("Generating list of existing stores...")
  pth <- model_defaults@data_path

  # list of all objectstore files
  rds.files <- list.files(pth, "_objectstore.rds")

  # function fo find the store
  wh.cond.fn <- function(x){
    name.el <- strsplit(x, "_")[[1]]
    if (length(name.el) != 4) return(FALSE)
    if (!grepl("^[0-9]+$", name.el[1], perl = TRUE)) return(FALSE)
    dates <- tryCatch({ as.Date(name.el[2:3])})
    if (!is.Date(dates)) return(FALSE)
    return(TRUE)
  }

  wh_str.files <- rds.files[sapply(rds.files, wh.cond.fn)]

  for (name in wh_str.files) {
    name <- gsub("_objectstore.rds", "", name)

    whstr <- warehouse_objectstore_factory(name)

    whstr <- saveObjectInRemoteStore(whstr)

  }


  return(whstr)
}
