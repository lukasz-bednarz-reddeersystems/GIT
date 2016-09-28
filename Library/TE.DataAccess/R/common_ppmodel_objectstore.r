#' @include objectstore.r
#' @include global_configs.r
NULL

#' Generate PPModel objectstore name from keys
#'
#' Creates PPModel ObjectStore and loads data from
#' associated file if exists.
#'
#' @param keys "data.frame", keys from which name(s) of objectstore is created
#'
#' @export

get_ppmodel_objectstore_name <- function(keys) {
  rv <- apply(keys, 1, function(x){paste0(c("ppmodel_store", unlist(x)), collapse = "_")})
  return(rv)
}

#' helper function to generate key from objectstore name
#'
#' @param name "character" name of the objectstore
#' @return \code{key} "data.frame" with columns "id", "start", "end"
key_from_ppmodel_objectstore_name <- function(name) {

  str_keys <- strsplit(name, "_")

  key <- data.frame(model_class = str_keys[[1]][3],
                    id          = as.integer(str_keys[[1]][4]),
                    start       = as.Date(str_keys[[1]][5]),
                    end         = as.Date(str_keys[[1]][6]))
  return(key)
}


setClass(
  Class          = "VirtualPPModelQuery",
  prototype = prototype(
    fields       = c('hash', 'model_class','id','start', 'end')
  ), contains = c("ObjectQuery", "VIRTUAL")
)

setMethod("hashKey",
          signature(object = "VirtualPPModelQuery",
                    key    = "data.frame"),
          function(object,key){
            hash <- hash_data_frame(key[object@fields[2:5]], algo = "murmur32")
            hashedkey <- cbind(data.frame(hash=hash),key)
            return(hashedkey)
          }
)

setGeneric("setPPModelQuery",function(object,key){standardGeneric("setPPModelQuery")})
setMethod("setPPModelQuery",
          signature(object = "VirtualPPModelQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- setQueryValuesFromKey(object,hashedkey)
            return(object)
          }
)

setGeneric("updateStoredPPModelKeys",function(object,key){standardGeneric("updateStoredPPModelKeys")})
setMethod("updateStoredPPModelKeys",
          signature(object = "VirtualPPModelQuery",
                    key    = "data.frame"),
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- updateKnownKeys(object,hashedkey)
            return(object)
          }
)

setGeneric("isPPModelStored",function(object,key){standardGeneric("isPPModelStored")})
setMethod("isPPModelStored",
          signature(object = "VirtualPPModelQuery",
                    key    = "data.frame"),
          function(object,key){

            if(length(object@known_keys)==0){
              rval <- FALSE
            }
            else{
              hash <- hash_data_frame(key[object@fields[2:5]], algo = "murmur32")

              rval <- hash%in%object@known_keys[['hash']]

              # this is to account for old way of hashing that had issues
              if (!rval) {
                hash <- murmur3.32(paste(key[object@fields[2:5]],sep=""))
                rval <- hash%in%object@known_keys[['hash']]
              }

            }
            return(rval)
          }
)




#' An S4 class handling queries to PPModelObjectstore.
#'
#' @export

setClass(
  Class          = "PPModelQuery",
  contains = c("VirtualPPModelQuery")
)

#' An S4 class handling queries to WarehouseObjectstore.

setClass(
  Class = "RemotePPModelQuery",
  prototype = prototype(
    #fields need to match column names
    #of key data frame
    tb_name = "tRDTE_PPModelObjectstore"
  ),
  contains =c("RemoteObjectQuery", "VirtualPPModelQuery")
)

#' Initialize method for "RemotePPModelQuery" class
#'
#' @param .Object, object of class "RemotePPModelQuery"
#' @return \code{.Object} object of class "RemotePPModelQuery"
setMethod("initialize", "RemotePPModelQuery",
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

#' An S4 class implementing of PPModel Objectstore.
#'
#' Implements storage, queries, and update of PPModels
#' in an object and saving in related file.
#'
#' Inherits from "VirtualObjectStore"
#'
#' @slot warehouse_q      "PPModelQuery"
#' @slot qry_store_nme    "character",
#'
#' @export

setClass(
  Class          = "PPModelObjectStore",
  representation = representation(
    objectstore_q  = "VirtualPPModelQuery",
    qry_store_nme= "character"
  ),
  prototype      = prototype(
    objectstore_q  = new("RemotePPModelQuery"),
    data_path    = model_defaults@data_path,
    qry_store_nme= "ppmodel_queries"
  ),
  contains = c("VirtualRemoteObjectStore")
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "VirtualPPModelQuery"),
          function(object, objectstore_q){

            # copy slots of Warehouse Query
            new_query <- new("RemotePPModelQuery")

            new_query@values <- objectstore_q@values
            new_query@known_keys <- objectstore_q@known_keys

            object <- callNextMethod(object, new_query)
            return(object)
          }
)


setMethod(".setObjectStoreQuery",
          signature( object = "VirtualRemoteObjectStore",
                     objectstore_q = "PPModelQuery"),
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
          signature( object = "PPModelObjectStore"),
          function(object){

            id <- getID(object)

            name <- key_from_ppmodel_objectstore_name(id)

            return(name)
          }
)


# #' Initialize method for "PPModelObjectStore" class
# #'
# #' @param .Object, object of class "PPModelObjectStore"
# #' @param id id to set when initializing
# #' @return \code{.Object} object of class "PPModelObjectStore"
#
#
# setMethod("initialize", "PPModelObjectStore",
#           function(.Object,id){
#             .Object@id <- id
#             .Object
#           }
# )

setGeneric("initialisePPModelStore",function(object){standardGeneric("initialisePPModelStore")})
setMethod("initialisePPModelStore","PPModelObjectStore",
          function(object){
            object <- loadObject(object)

            query <- getFromObjectStore(object,object@qry_store_nme)

            object <- .setObjectStoreQuery(object, query)
            return(object)
          }
)

#' Query store for PPModel
#'
#' Querries PPModel Objectstore for PPModel stored under given key
#' Returns PPModel if present NULL otherwise
#'
#' @param object object of class "PPModelObjectStore"
#' @param key "data.frame" with key related to query
#' @return \code{rval} object of class "PPModel"
#'
#' @export

setGeneric("queryPPModelStore",function(object,key){standardGeneric("queryPPModelStore")})

#' @describeIn queryPPModelStore
#' Query store for PPModel
#'
#' Querries PPModel Objectstore for PPModel stored under given key
#' Returns PPModel if present NULL otherwise
#'
#' @inheritParams queryPPModelStore
#' @return \code{rval} object of class "PPModel"
#' @export

setMethod("queryPPModelStore","PPModelObjectStore",
          function(object,key){
            if(isPPModelStored(object@warehouse_q,key)){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in ppmodel store."))
              object@warehouse_q <- setPPModelQuery(object@warehouse_q,key)
              name <- getIdentifier(object@warehouse_q)
              rval <- getFromObjectStore(object,name)
            }
            else{
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"not found in ppmodel store."))
              rval <- NULL
            }

            return(rval)
          }
)

#' Store PPmodel in Store
#'
#' Stores PPModel and reated Query in Store
#'
#' @param object object of class "PPModelObjectStore"
#' @param ppmodel_object object of class "PPModel"
#' @param key "data.frame" with key related to query
#' @param force "logical" force update of PPModel if it is already present
#' @return \code{object} object of class "PPModelObjectStore"
#'
#' @export

setGeneric("updatePPModelStore",function(object,ppmodel_object,key,force=FALSE){standardGeneric("updatePPModelStore")})


#' @describeIn updatePPModelStore
#' Store PPmodel in Store
#'
#' Stores PPModel and reated Query in Store
#'
#' @inheritParams updatePPModelStore
#' @return \code{object} object of class "PPModelObjectStore"
#' @export

setMethod("updatePPModelStore","PPModelObjectStore",
          function(object,ppmodel_object,key,force=FALSE){
            if(isPPModelStored(object@warehouse_q,key) && !force){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in ppmodel store."))
              message("No update made.")
            }
            else{
              if(force)message("Force update flag set, data will be overwritten ...")
              message(paste("Updating ppmodel store for key",paste(unlist(Map(as.character,key)),collapse=", "),collapse=", "))
              object@warehouse_q <- setPPModelQuery(object@warehouse_q,key)
              object@warehouse_q <- updateStoredPPModelKeys(object@warehouse_q,key)
              object <- placeInObjectStore(object,object@warehouse_q,object@qry_store_nme)
              object <- placeInObjectStore(object,ppmodel_object,getIdentifier(object@warehouse_q))
            }
            return(object)
          }
)


#' Commit store data to file
#'
#' Saves data stored in the object into file.
#'
#' @param object object of class "PPModelObjectStore"
#' @return \code{object} object of class "PPModelObjectStore"
#'
#' @export

setGeneric("commitPPModelStore",function(object){standardGeneric("commitPPModelStore")})

#' @describeIn commitPPModelStore
#' Commit store data to file
#'
#' @inheritParams commitPPModelStore
#' @return \code{object} object of class "PPModelObjectStore"
#' @export

setMethod("commitPPModelStore","PPModelObjectStore",
          function(object){
            saveObject(object)
          }
)

setGeneric("getPPModelStoreContents",function(object){standardGeneric("getPPModelStoreContents")})
setMethod("getPPModelStoreContents","PPModelObjectStore",
          function(object){
            names <- getNamesFromStore(object)
            names <- names[names!=object@qry_store_nme]
            return(names)
          }
)

#' Create PPModelObjectstore object
#'
#' Creates PPModel ObjectStore and loads data from
#' associated file if exists.
#'
#' @param name "character", name of the objectstore
#'
#' @export

ppmodel_objectstore_factory <- function(name){
  message("Initialising ppmodel store ...")
  ppmstr <- new("PPModelObjectStore",id=name)
  pth <- getPath(ppmstr)

  if (!file.exists(pth)) {
    message(sprintf("File initially not found in local path %s. Checking remote store",pth))
    key <- key_from_ppmodel_objectstore_name(basename(pth))
    query <- getObjectStoreQuery(ppmstr)
    is_known <- isKeyKnownInRemoteStore(query, key)

    if (is_known) {
      ppmstr <- updateLocalStoreFile(ppmstr,key)
    }
  }

  if(file.exists(pth)){
    message(paste("Found ppmodel store at",pth))
    ppmstr <- initialisePPModelStore(ppmstr)
  }
  else{
    message(paste("No previous store data found at",pth,"new store created."))
  }
  return(ppmstr)
}

#' Copy PPModels from local objectstores to remote store.
#'
#' copies all locally stored ppmodels to remote store and updates keys
#'
#' @return \code{count} number of warehouses copied
#' @export

update_ppmodel_remote_storage <- function(){
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

  for (name in wh_str.files) {
    name <- gsub("_objectstore.rds", "", name)

    whstr <- ppmodel_objectstore_factory(name)
    whstr <- saveObjectInRemoteStore(whstr)

  }


  return(whstr)
}
