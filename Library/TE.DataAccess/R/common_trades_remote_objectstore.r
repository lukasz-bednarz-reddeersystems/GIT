#' @include objectstore.r
#' @include global_configs.r
#' @include data_access_sql_procedures.r
NULL

#' Generate Trade objectstore name from keys
#'
#' Creates Trade ObjectStore and loads data from
#' associated file if exists.
#'
#' @param keys "data.frame", keys from which name(s) of objectstore is created
#'
#' @export

get_trade_objectstore_name <- function(keys) {

  key_cols <- c('id', 'instrument', 'buysell', 'strategy', 'leg_start')

  rv <- apply(keys[key_cols], 1, function(x){paste0(c("trade_store", unlist(x)), collapse = "_")})
  return(rv)
}

#' helper function to generate key from objectstore name
#'
#' @param name "character" name of the objectstore
#' @return \code{key} "data.frame" with columns "id", "leg_start", "leg_end"
key_from_trade_objectstore_name <- function(name) {

  str_keys <- strsplit(name, "_")

  key <- data.frame(id          = str_keys[[1]][3],
                    instrument  = as.integer(str_keys[[1]][4]),
                    buysell     = str_keys[[1]][5],
                    strategy    = str_keys[[1]][6],
                    leg_start       = as.Date(str_keys[[1]][7]),
                    leg_end         = as.Date(str_keys[[1]][8]))

  return(key)
}


setClass(
  Class          = "VirtualTradeQuery",
  prototype = prototype(
    fields       = c('hash', 'id', 'instrument', 'buysell', 'strategy', 'leg_start', 'leg_end', 'status')
  ), contains = c("ObjectQuery", "VIRTUAL")
)

setMethod("hashKey",
          signature(object = "VirtualTradeQuery",
                    key    = "data.frame"),
          function(object,key){
            hash <- hash_data_frame(key[object@fields[2:6]], algo = "murmur32")
            hashedkey <- cbind(data.frame(hash=hash),key)
            return(hashedkey)
          }
)

setGeneric("setTradeQuery",function(object,key){standardGeneric("setTradeQuery")})
setMethod("setTradeQuery",
          signature(object = "VirtualTradeQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- setQueryValuesFromKey(object,hashedkey)
            return(object)
          }
)

setGeneric("updateStoredTradeKeys",function(object,key){standardGeneric("updateStoredTradeKeys")})
setMethod("updateStoredTradeKeys",
          signature(object = "VirtualTradeQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- updateKnownKeys(object,hashedkey)
            return(object)
          }
)

setGeneric("isTradeStored",function(object,key){standardGeneric("isTradeStored")})
setMethod("isTradeStored",
          signature(object = "VirtualTradeQuery",
                    key    = "data.frame"),
          function(object,key){

            if(length(object@known_keys)==0){
              rval <- FALSE
            }
            else{
              hash <- hash_data_frame(key[object@fields[2:6]], algo = "murmur32")

              rval <- hash%in%object@known_keys[['hash']]

            }
            if (!rval && is.na(key$strategy)){
              message(sprintf("Trade not found for given key trying with out strategy column"))

              res <- merge(object@known_keys, key[setdiff(colnames(key), "strategy")])

              if (nrow(res) > 0 ) {
                rval <- TRUE
              }
            }

            return(rval)
          }
)



setClass(
  Class          = "VirtualRemoteTradeQuery",
  contains = c("RemoteObjectQuery", "VIRTUAL")
)

setMethod(".generateRemoteQueryKey",
          signature(object = "VirtualRemoteTradeQuery",
                    key = "data.frame"),
          function(object,key){

            sql_query <- getSQLQueryObject(object)

            key <- key[c('id', 'instrument', 'buysell', 'strategy', 'leg_start', 'leg_end')]


            colnames(key) <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql_query, colnames(key))

            return(key)
          }
)


setMethod(".generateRemoteInsertKey",
          signature(object = "VirtualRemoteTradeQuery",
                    key = "data.frame"),
          function(object,key){

            sql <- getSQLInsertObject(object)

            key <- key[c('id', 'instrument', 'buysell', 'strategy', 'leg_start', 'leg_end', 'status')]

            colnames(key) <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql, colnames(key))

            hash_col <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql, "hash")

            hash_df <- data.frame(hash= hash_data_frame(key))

            colnames(hash_df) <- hash_col

            key <- cbind(hash_df, key)

            return(key)
          }
)


#' An S4 class handling queries to TradeObjectstore.
#'
#' @export

setClass(
  Class          = "TradeQuery",
  contains = c("VirtualTradeQuery")
)

#' An S4 class handling queries to WarehouseObjectstore.

setClass(
  Class = "RemoteTradeQuery",
  prototype = prototype(
    #fields need to match column names
    #of key data frame
    tb_name = "tRDTE_TradesObjectstore"
  ),
  contains =c("VirtualRemoteTradeQuery", "VirtualTradeQuery")
)

#' Initialize method for "RemoteTradeQuery" class
#'
#' @param .Object, object of class "RemoteTradeQuery"
#' @return \code{.Object} object of class "RemoteTradeQuery"
setMethod("initialize", "RemoteTradeQuery",
          function(.Object){
            sql_query <- new("BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate",
                             .getObjectQueryDBName(.Object),
                             .getObjectQuerySchemaName(.Object),
                             .getObjectQueryTableName(.Object))
            .Object <- setSQLQueryObject(.Object, sql_query)

            sql_insert <- new("BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate",
                              .getObjectQueryDBName(.Object),
                              .getObjectQuerySchemaName(.Object),
                              .getObjectQueryTableName(.Object))
            .Object <- setSQLInsertObject(.Object, sql_insert)

            return(.Object)

          }
)

#' An S4 class implementing of Trade Objectstore.
#'
#' Implements storage, queries, and update of Trades
#' in an object and saving in related file.
#'
#' Inherits from "VirtualObjectStore"
#'
#' @slot objectstore_q      "TradeQuery"
#' @slot qry_store_nme    "character",
#'
#' @export

setClass(
  Class          = "TradeObjectStore",
  representation = representation(
    objectstore_q   = "VirtualTradeQuery",
    qry_store_nme   = "character"
  ),
  prototype      = prototype(
    objectstore_q  = new("RemoteTradeQuery"),
    qry_store_nme= "trade_queries"
  ),
  contains = c("VirtualRemoteObjectStore")
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "VirtualTradeQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemoteTradeQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "TradeQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemoteTradeQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)




setMethod(".generateKeyFromID",
          signature( object = "TradeObjectStore"),
          function(object){

            return(object@objectstore_key)
          }
)


#' Initialize method for "TradeObjectStore" class
#'
#' @param .Object, object of class "TradeObjectStore"
#' @param id "character "id to set when initializing
#' @param key "data.frame" key defining objectstore to set when initializing
#' @return \code{.Object} object of class "TradeObjectStore"

setMethod("initialize", "TradeObjectStore",
          function(.Object,id, key){
            .Object@stored <- new.env(parent = emptyenv())
            .Object@id <- id
            .Object@objectstore_key <- key
            .Object@data_path <- tempdir()
            .Object
          }
)

setGeneric("initialiseTradeStore",function(object){standardGeneric("initialiseTradeStore")})
setMethod("initialiseTradeStore","TradeObjectStore",
          function(object){
            object <- loadObject(object)

            query <- getFromObjectStore(object,object@qry_store_nme)

            object <- .setObjectStoreQuery(object, query)
            return(object)
          }
)

#' Query store for Trade
#'
#' Querries Trade Objectstore for Trade stored under given key
#' Returns Trade if present NULL otherwise
#'
#' @param object object of class "TradeObjectStore"
#' @param key "data.frame" with key related to query
#' @return \code{rval} object of class "Trade"
#'
#' @export

setGeneric("queryTradeStore",function(object,key){standardGeneric("queryTradeStore")})

#' @describeIn queryTradeStore
#' Query store for Trade
#'
#' Querries Trade Objectstore for Trade stored under given key
#' Returns Trade if present NULL otherwise
#'
#' @inheritParams queryTradeStore
#' @return \code{rval} object of class "Trade"
#' @export

setMethod("queryTradeStore","TradeObjectStore",
          function(object,key){
            query <- getObjectStoreQuery(object)
            if(isTradeStored(query,key)){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in ppmodel store."))
              query <- setTradeQuery(query,key)
              object <- .setObjectStoreQuery(object, query)
              name <- getIdentifier(query)
              rval <- getFromObjectStore(object,name)
            }
            else{
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"not found in ppmodel store."))
              rval <- NULL
            }

            return(rval)
          }
)

#' Query store for Trade
#'
#' Querries Trade Objectstore for Trade stored under given key
#' Returns Trade if present NULL otherwise
#'
#' @param object object of class "TradeObjectStore"
#' @return \code{rval} object of class "Trade"


setGeneric("getAllTradesdFromTradeStore",function(object){standardGeneric("getAllTradesdFromTradeStore")})

#' @describeIn getAllTradesdFromTradeStore
#' Query store for Trade
#'
#' Querries Trade Objectstore for Trade stored under given key
#' Returns Trade if present NULL otherwise
#'
#' @inheritParams getAllTradesdFromTradeStore
#' @return \code{rval} list of objects of class "Trade"
setMethod("getAllTradesdFromTradeStore","TradeObjectStore",
          function(object){
            names <- ls(object@stored)

            names <- setdiff(names, object@qry_store_nme)

            rval <- mget(names, object@stored, ifnotfound = sapply(names, function(x)NULL))

            return(rval)
          }
)

#' Store PPmodel in Store
#'
#' Stores Trade and reated Query in Store
#'
#' @param object object of class "TradeObjectStore"
#' @param trade_object object of class "Trade"
#' @param key "data.frame" with key related to query
#' @param force "logical" force update of Trade if it is already present
#' @return \code{object} object of class "TradeObjectStore"
#'
#' @export

setGeneric("updateTradeStore",function(object,trade_object,key,force=FALSE){standardGeneric("updateTradeStore")})


#' @describeIn updateTradeStore
#' Store PPmodel in Store
#'
#' Stores Trade and reated Query in Store
#'
#' @inheritParams updateTradeStore
#' @return \code{object} object of class "TradeObjectStore"
#' @export

setMethod("updateTradeStore","TradeObjectStore",
          function(object,trade_object,key,force=FALSE){
            query <- getObjectStoreQuery(object)
            if(isTradeStored(query,key) && !force){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in trade store."))
              message("No update made.")
            }
            else{
              if(force)message("Force update flag set, data will be overwritten ...")
              message(paste("Updating trades store for key",paste(unlist(Map(as.character,key)),collapse=", "),collapse=", "))
              query <- setTradeQuery(query,key)
              query <- updateStoredTradeKeys(query,key)

              object <- .setObjectStoreQuery(object, query)

              object <- placeInObjectStore(object,query,object@qry_store_nme)
              object <- placeInObjectStore(object,trade_object,getIdentifier(query))
            }
            return(object)
          }
)


#' Commit store data to file
#'
#' Saves data stored in the object into file.
#'
#' @param object object of class "TradeObjectStore"
#' @return \code{object} object of class "TradeObjectStore"
#'
#' @export

setGeneric("commitTradeStore",function(object){standardGeneric("commitTradeStore")})

#' @describeIn commitTradeStore
#' Commit store data to file
#'
#' @inheritParams commitTradeStore
#' @return \code{object} object of class "TradeObjectStore"
#' @export

setMethod("commitTradeStore","TradeObjectStore",
          function(object){
            saveObject(object)
          }
)

setGeneric("getTradeStoreContents",function(object){standardGeneric("getTradeStoreContents")})
setMethod("getTradeStoreContents","TradeObjectStore",
          function(object){
            names <- getNamesFromStore(object)
            names <- names[names!=object@qry_store_nme]
            return(names)
          }
)

#' Create TradeObjectstore object
#'
#' Creates Trade ObjectStore and loads data from
#' associated file if exists.
#'
#' @param key "data.frame", with keys for trade
#' required keys are: c("id", "instrument", "buysell", "strategy", "leg_start", "leg_end")
#'
#' @export

trade_objectstore_factory <- function(key){
  message("Initialising trade store ...")

  name <- get_trade_objectstore_name(key)

  trdstr <- new("TradeObjectStore",id=name, key = key)

  pth <- getPath(trdstr)

  query <- getObjectStoreQuery(trdstr)
  is_known <- isKeyKnownInRemoteStore(query, key)

  if (is_known) {
    trdstr <- updateLocalStoreFile(trdstr, key)
  }

  if (file.exists(pth)) {
    trdstr <- initialiseTradeStore(trdstr)
  }
  else{
    message(paste("No previous store data found at",pth,"new store created."))
  }

  return(trdstr)
}

#' Copy Trades from local objectstores to remote store.
#'
#' copies all locally stored ppmodels to remote store and updates keys
#'
#' @return \code{count} number of warehouses copied

update_trade_remote_storage <- function(){
  message("Generating list of existing stores...")
  pth <- model_defaults@data_path

  # list of all objectstore files
  rds.files <- list.files(pth, "ppmodel_store_.*_objectstore.rds")

  # function fo find the store
  wh.cond.fn <- function(x){
    name.el <- strsplit(x, "_")[[1]]
    if (length(name.el) != 7) return(FALSE)
    if (!grepl("^[0-9]+$", name.el[4], perl = TRUE)) return(FALSE)
    dates <- tryCatch({ as.Date(name.el[5:6])})
    if (!is.Date(dates)) return(FALSE)
    return(TRUE)
  }

  wh_str.files <- rds.files[sapply(rds.files, wh.cond.fn)]

  count <- 0
  for (name in wh_str.files) {

    name <- gsub("_objectstore.rds", "", name)

    whstr <- ppmodel_objectstore_factory(name)

    stored_name <- setdiff(getNamesFromStore(whstr), whstr@qry_store_nme)

    stored_name <- grep(gsub("ppmodel_store_", "", name), stored_name, value = TRUE)

    item <- tryCatch({
      getFromObjectStore(whstr, stored_name)
    }, error = function(cond) {

     stop(cond)
    })

    new_whstr <- new("TradeObjectStore", id = name)

    key <- key_from_ppmodel_objectstore_name(name)

    new_whstr <- updateTradeStore(new_whstr, item, key, TRUE)

    saveObject(new_whstr)

    count <- count + 1

  }


  return(count)
}
