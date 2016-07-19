sourceTo("../lib/objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

get_ppmodel_objectstore_name <- function(keys) {
  rv <- apply(keys, 1, function(x){paste0(c("ppmodel_store", unlist(x)), collapse = "_")})
  return(rv)
}



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

setMethod("initialize", "VirtualObjectStore",
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

setGeneric("queryPPModelStore",function(object,key){standardGeneric("queryPPModelStore")})
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

setGeneric("updatePPModelStore",function(object,ppmodel_object,key,force=FALSE){standardGeneric("updatePPModelStore")})
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

setGeneric("commitPPModelStore",function(object){standardGeneric("commitPPModelStore")})
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
