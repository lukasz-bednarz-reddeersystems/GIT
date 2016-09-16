#' @include datastore.r global_configs.r objectstore.r
NULL


#' An S4 class handling queries to AnalysisObjectstore.
#'
#' @export

setClass(
  Class          = "AnalysisQuery",
  prototype = prototype(
    fields       = c('hash','key_hash','analysis_module')
  ), contains = c("ObjectQuery")
)

setMethod("hashKey","AnalysisQuery",
          function(object,key){
            hash <- murmur3.32(paste(key[object@fields[2]],key[object@fields[3]],sep=""))
            hashedkey <- cbind(data.frame(hash=hash),key)
            return(hashedkey)
          }
)

setGeneric("setAnalysisQuery",function(object,key){standardGeneric("setAnalysisQuery")})
setMethod("setAnalysisQuery","AnalysisQuery",
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- setQueryValuesFromKey(object,hashedkey)
            return(object)
          }
)

setGeneric("updateStoredAnalysisKeys",function(object,key){standardGeneric("updateStoredAnalysisKeys")})
setMethod("updateStoredAnalysisKeys","AnalysisQuery",
          function(object,key){
            hashedkey <- hashKey(object,key)
            object <- updateKnownKeys(object,hashedkey)
            return(object)
          }
)

setGeneric("isAnalysisStored",function(object,key){standardGeneric("isAnalysisStored")})
setMethod("isAnalysisStored","AnalysisQuery",
          function(object,key){
            hash <- murmur3.32(paste(key[object@fields[2]],key[object@fields[3]],sep=""))
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
  Class          = "AnalysisObjectStore",
  representation = representation(
    warehouse_q  = "AnalysisQuery",
    qry_store_nme= "character"
  ),
  prototype      = prototype(
    warehouse_q  = new("AnalysisQuery"),
    data_path    = model_defaults@data_path,
    qry_store_nme= "analysis_queries"
  ),
  contains = c("VirtualObjectStore")
)

setGeneric("initialiseAnalysisStore",function(object){standardGeneric("initialiseAnalysisStore")})
setMethod("initialiseAnalysisStore","AnalysisObjectStore",
          function(object){
            object <- loadObject(object)
            object@warehouse_q <- getFromObjectStore(object,object@qry_store_nme)
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
            if(isAnalysisStored(object@warehouse_q,key)){
              message(paste("Key",paste(unlist(Map(as.character,key)),collapse=", "),"found in analysis store."))
              object@warehouse_q <- setAnalysisQuery(object@warehouse_q,key)
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
