#' @include datastore.r global_configs.r objectstore.r
NULL

setClass(
  Class          = "VirtualAnalysisQuery",
  prototype = prototype(
    fields       = c('hash','model_class','id','start', 'end')
  ), contains = c("ObjectQuery")
)



setMethod("hashKey",
          signature(object = "VirtualAnalysisQuery",
                    key    = "data.frame"),
          function(object,key){
            # hash <- murmur3.32(paste(key[object@fields[2]],key[object@fields[3]],sep=""))

            hash <- hash_data_frame(key[object@fields[2:5]], algo = "murmur32")

            hashedkey <- cbind(data.frame(hash=hash),key)
            return(hashedkey)
          }
)

setGeneric("setAnalysisQuery",function(object,key){standardGeneric("setAnalysisQuery")})
setMethod("setAnalysisQuery",
          signature(object = "VirtualAnalysisQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- setQueryValuesFromKey(object,hashedkey)
            return(object)
          }
)

setGeneric("updateStoredAnalysisKeys",function(object,key){standardGeneric("updateStoredAnalysisKeys")})
setMethod("updateStoredAnalysisKeys",
          signature(object = "VirtualAnalysisQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- updateKnownKeys(object,hashedkey)
            return(object)
          }
)

setGeneric("isAnalysisStored",function(object,key){standardGeneric("isAnalysisStored")})
setMethod("isAnalysisStored",
          signature(object = "VirtualAnalysisQuery",
                    key    = "data.frame"),
          function(object,key){


            if(length(object@known_keys)==0){
              rval <- FALSE
            }
            else{
              hash <- hash_data_frame(key[object@fields[-1]], algo = "murmur32")
              rval <- hash%in%object@known_keys[['hash']]

              if (!rval){
                hash <- murmur3.32(paste(key[object@fields[2]],key[object@fields[3]],sep=""))
                rval <- hash%in%object@known_keys[['hash']]
              }

            }
            return(rval)
          }
)


#' An S4 class handling queries to AnalysisObjectstore.
#'
#' @export

setClass(
  Class          = "AnalysisQuery",
  contains = c("VirtualAnalysisQuery")
)


#' An S4 class handling queries to WarehouseObjectstore.

setClass(
  Class = "RemoteAnalysisQuery",
  prototype = prototype(
    #fields need to match column names
    #of key data frame
    tb_name = "tRDTE_AnalysisObjectstore"
  ),
  contains =c("RemoteObjectQuery", "VirtualAnalysisQuery")
)

#' Initialize method for "RemoteAnalysisQuery" class
#'
#' @param .Object, object of class "RemoteAnalysisQuery"
#' @return \code{.Object} object of class "RemoteAnalysisQuery"
setMethod("initialize", "RemoteAnalysisQuery",
          function(.Object){
            sql_query <- new("BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID",
                             .getObjectQueryDBName(.Object),
                             .getObjectQuerySchemaName(.Object),
                             .getObjectQueryTableName(.Object))
            .Object <- setSQLQueryObject(.Object, sql_query)

            sql_insert <- new("BlobStorage.SQLProcedureCall.JointFileTable_UpdateByHashID",
                              .getObjectQueryDBName(.Object),
                              .getObjectQuerySchemaName(.Object),
                              .getObjectQueryTableName(.Object))
            .Object <- setSQLInsertObject(.Object, sql_insert)

            return(.Object)

          }
)


setClass(
  Class          = "AnalysisObjectStore",
  representation = representation(
    objectstore_q  = "RemoteAnalysisQuery",
    qry_store_nme= "character"
  ),
  prototype      = prototype(
    objectstore_q  = new("RemoteAnalysisQuery"),
    data_path    = model_defaults@data_path,
    qry_store_nme= "analysis_queries"
  ),
  contains = c("VirtualRemoteObjectStore")
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "VirtualAnalysisQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemoteAnalysisQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "AnalysisQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemotePPModelQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)


setMethod(".generateKeyFromID",
          signature( object = "AnalysisObjectStore"),
          function(object){

            id <- getID(object)

            key <- key_from_analysis_objectstore_name(id)

            return(key)
          }
)

setGeneric("initialiseAnalysisStore",function(object){standardGeneric("initialiseAnalysisStore")})
setMethod("initialiseAnalysisStore","AnalysisObjectStore",
          function(object){
            object <- loadObject(object)

            query <- getFromObjectStore(object,object@qry_store_nme)

            object <- .setObjectStoreQuery(object, query)

            return(object)
          }
)

#' Query objectstore for given set of parameters
#'
#' @param object object of class "DailyRiskModelObjectStore"
#' @param key "data.frame" wiht query keys
#'
#' @export
setGeneric("queryAnalysisStore",function(object,key){standardGeneric("queryAnalysisStore")})

#' @describeIn queryAnalysisStore
#' Query objectstore for given set of parameters
#'
#' @inheritParams  queryAnalysisStore
#' @return \code{rval} object of class "AnalysisObjectStore" if query sucessfull,
#' otherwise NULL
#'
#' @export
setMethod("queryAnalysisStore",
          signature(object = "AnalysisObjectStore",
                    key = "data.frame"),
          function(object,key){
            query <- getObjectStoreQuery(object)

            if(isAnalysisStored(query,key)){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in analysis store."))

              query <- setAnalysisQuery(query,key)
              object <- .setObjectStoreQuery(object, query)

              name <- getIdentifier(object@warehouse_q)
              rval <- getFromObjectStore(object,name)
            }
            else{
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"not found in analysis store."))
              rval <- NULL
            }

            return(rval)
          }
)


#' Update Analysis Store with new AnalysisObject
#'
#' @param object object of class "AnalysisObjectStore"
#' @param analysis_object object of class "VirtualAnalysisBlock"
#' @param key "data.frame" with query keys
#' @param force "logical" should store be forced if object for given keys exists
#'
#' @export
setGeneric("updateAnalysisStore",function(object,analysis_object,key,force=FALSE){standardGeneric("updateAnalysisStore")})

#' @describeIn updateAnalysisStore
#' Update Analysis Store with new AnalysisObject
#'
#' @inheritParams updateAnalysisStore
#' @return \code{object} object of class "AnalysisObjectStore"
#'
#' @export
setMethod("updateAnalysisStore",
          signature(object = "AnalysisObjectStore",
                    analysis_object = "ANY",
                    key = "data.frame",
                    force = "logical"),
          function(object,analysis_object,key,force=FALSE){
            if(isAnalysisStored(object@warehouse_q,key) && !force){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in analysis store."))
              message("No update made.")
            }
            else{
              if(force)message("Force update flag set, data will be overwritten ...")
              message(paste("Updating analysis store for key",paste(unlist(Map(as.character,key)),collapse=", "),collapse=", "))
              object@warehouse_q <- setAnalysisQuery(object@warehouse_q,key)
              object@warehouse_q <- updateStoredAnalysisKeys(object@warehouse_q,key)
              object <- placeInObjectStore(object,object@warehouse_q,object@qry_store_nme)
              object <- placeInObjectStore(object,analysis_object,getIdentifier(object@warehouse_q))
            }
            return(object)
          }
)

#' Commit AnalysisObjecstore to nonvolatile memory
#'
#' @param object object of class "AnalysisObjectStore"
#'
#' @export
setGeneric("commitAnalysisStore",function(object){standardGeneric("commitAnalysisStore")})

#' @describeIn commitAnalysisStore
#' Commit AnalysisObjecstore to nonvolatile memory
#'
#' @inheritParams commitAnalysisStore
#' @return \code{object} object of class "AnalysisObjectStore"
#'
#' @export
setMethod("commitAnalysisStore","AnalysisObjectStore",
          function(object){
            saveObject(object)
          }
)

setGeneric("getAnalysisStoreContents",function(object){standardGeneric("getAnalysisStoreContents")})
setMethod("getAnalysisStoreContents","AnalysisObjectStore",
          function(object){
            names <- getNamesFromStore(object)
            names <- names[names!=object@qry_store_nme]
            return(names)
          }
)


#' get analysis objectstore for keys and trader
#'
#' @param keys "data.frame" with query keys
#' @param trader_col "character" name of the column in key to be used as
#' identifier. Default is 'TraderID'
#' @return \code{rv} "character" hashed name of the objectstore
#'
#' @export
get_analysis_objectstore_name <- function(keys,trader_col='TraderID') {

  keys <- keys[c("analysis_class", trader_col, "start", "end")]

  rv <- apply(keys, 1, function(x){paste0(c("analysis", unlist(x)), collapse = "_")})
  return(rv)
}


#' get analysis objectstore for keys and trader
#'
#' this is legacy version of the analysis store name generator
#'
#' @param keys "data.frame" with query keys
#' @param trader_col "character" name of the column in key to be used as
#' identifier. Default is 'TraderID'
#' @return \code{rv} "character" hashed name of the objectstore
get_old_analysis_objectstore_name <- function(keys,trader_col='TraderID') {
  date_hash <- digest(sort(as.character(c(keys$start,keys$end))),serialize=FALSE)
  trader_prefix <- paste(sort(unique(keys[[trader_col]])),collapse="_")
  rv <- paste("analysis",trader_prefix,date_hash,sep='_')
  return(rv)
}

#' get analysis objectstore for given name
#'
#' @param name "character" hashed name of the objectstore
#' @return \code{anstr} object of class "AnalysisObjectStore"
#'
#' @export
analysis_objectstore_factory <- function(name){
  message("Initialising analysis store ...")
  anstr <- new("AnalysisObjectStore",id=name)
  pth <- getPath(anstr)
  if(file.exists(pth)){
    message(paste("Found analysis store at",pth))
    anstr <- initialiseAnalysisStore(anstr)
  }
  else{
    message(paste("No previous store data found at",pth,"new store created."))
  }
  return(anstr)
}

revert_hash <- function(hash, values, hash_function) {

  hashed_values <- sapply(values, hash_function)

  if (hash %in% hashed_values){

    idx <- which(hashed_values == hash)

    ret <- values[[idx]]
  }
  else {
    ret <- NULL
  }

  return(ret)

}

#' helper function to generate key from objectstore name
#'
#' @param name "character" name of the objectstore
#' @return \code{key} "data.frame" with columns "id", "start", "end"
key_from_hashed_ppmodel_objectstore_name <- function(name) {

  str_keys <- strsplit(name, "_")

  key <- data.frame(id          = str_keys[[1]][2],
                    hash        = str_keys[[1]][3])


  # possible guess for reverting analysis store hash
  dates <- range_years_lookback(11, today() - 3650, today())[c("start", "end")]

  keys_list <- split(dates,seq(nrow(dates)), list)

  date_key <- revert_hash(key$hash,
                          keys_list,
                          function(x){digest(sort(as.character(c(x$start,x$end))),serialize=FALSE)})

  key <- cbind(key$id, date_key)

  return(key)

}


#' Copy analysis blocks from local objectstores to remote store.
#'
#' copies all locally stored blocks to remote store and updates keys
#' fixes issues with wrong hashes
#'
#' @return \code{count} number of warehouses copied
#' @export

update_analysis_remote_storage <- function(){
  message("Generating list of existing stores...")
  pth <- model_defaults@data_path

  # list of all objectstore files
  rds.files <- list.files(pth, "analysis_.*_objectstore.rds")

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

    anstr <- warehouse_objectstore_factory(name)

    query <- getObjectStoreQuery(anstr)

    store_key <- key_from_hashed_ppmodel_objectstore_name(name)

    known_keys <- getKnownKeys(query)

    for (analysis in known_keys$analysis_module) {

      new_key <- cbind(data.frame(analysis_class = analysis), store_key)

      new_name <- get_analysis_objectstore_name(new_key, trader_col = "id")

      new_anstr <- analysis_objectstore_factory



      new_anstr <- saveObject(new_anstr)
    }


  }


  return(whstr)
}
