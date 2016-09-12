#' @include datastore.r global_configs.r
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


#' An S4 class handling queries to PPModelObjectstore.
#'
#' @export

setClass(
  Class          = "PPModelQuery",
  prototype = prototype(
    fields       = c('hash', 'model_class','id','start', 'end')
  ), contains = c("ObjectQuery")
)

setMethod("hashKey","PPModelQuery",
          function(object,key){
            hash <- murmur3.32(paste(key[object@fields[2:5]],sep=""))
            hashedkey <- cbind(data.frame(hash=hash),key)
            return(hashedkey)
          }
)

setGeneric("setPPModelQuery",function(object,key){standardGeneric("setPPModelQuery")})
setMethod("setPPModelQuery","PPModelQuery",
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- setQueryValuesFromKey(object,hashedkey)
            return(object)
          }
)

setGeneric("updateStoredPPModelKeys",function(object,key){standardGeneric("updateStoredPPModelKeys")})
setMethod("updateStoredPPModelKeys","PPModelQuery",
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- updateKnownKeys(object,hashedkey)
            return(object)
          }
)

setGeneric("isPPModelStored",function(object,key){standardGeneric("isPPModelStored")})
setMethod("isPPModelStored","PPModelQuery",
          function(object,key){
            hash <- murmur3.32(paste(key[object@fields[2:5]],sep=""))
            if(length(object@known_keys)==0){
              rval <- FALSE
            }
            else{
              rval <- hash%in%object@known_keys[['hash']]
            }
            return(rval)
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
    warehouse_q  = "PPModelQuery",
    qry_store_nme= "character"
  ),
  prototype      = prototype(
    warehouse_q  = new("PPModelQuery"),
    data_path    = model_defaults@data_path,
    qry_store_nme= "ppmodel_queries",
    stored       = new.env(parent = emptyenv())
  ),
  contains = c("VirtualObjectStore")
)

#' Initialize method for "PPModelObjectStore" class
#'
#' @param .Object, object of class "PPModelObjectStore"
#' @param id id to set when initializing
#' @return \code{.Object} object of class "PPModelObjectStore"
#' @export
setMethod("initialize", "PPModelObjectStore",
          function(.Object,id){
            .Object@id <- id
            .Object
          }
)

setGeneric("initialisePPModelStore",function(object){standardGeneric("initialisePPModelStore")})
setMethod("initialisePPModelStore","PPModelObjectStore",
          function(object){
            object <- loadObject(object)
            object@warehouse_q <- getFromObjectStore(object,object@qry_store_nme)
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
  anstr <- new("PPModelObjectStore",id=name)
  pth <- getPath(anstr)
  if(file.exists(pth)){
    message(paste("Found ppmodel store at",pth))
    anstr <- initialisePPModelStore(anstr)
  }
  else{
    message(paste("No previous store data found at",pth,"new store created."))
  }
  return(anstr)
}
