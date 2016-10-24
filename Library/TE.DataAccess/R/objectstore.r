#' @include query.r
NULL



#' helper function to generate key from objectstore name
#'
#' @param name "character" name of the objectstore
#' @return \code{key} "data.frame" with columns "id", "start", "end"
key_from_name <- function(name) {

  str_keys <- strsplit(name, "_")

  key <- data.frame(id    = str_keys[[1]][1],
                    start = str_keys[[1]][2],
                    end   = str_keys[[1]][3])
  return(key)
}


#' An S4 class implementing handling queries to objectstores derived from VirtualObjectStore.
#'
#' @slot fields      "character",
#' @slot values      "character",
#' @slot known_keys  "data.frame",

setClass(
	Class = "ObjectQuery",
	representation = representation(
		known_keys = "data.frame"
	),
	contains = c("VirtualQuery", "VIRTUAL")
)


setGeneric("hashKey",function(object,key){standardGeneric("hashKey")})


setGeneric("getKnownKeys",function(object,key){standardGeneric("getKnownKeys")})
setMethod("getKnownKeys","ObjectQuery",
          function(object,key){
            return(object@known_keys)
          }
)


setGeneric("getIdentifier",function(object){standardGeneric("getIdentifier")})
setMethod("getIdentifier","ObjectQuery",
		 function(object){
		 	return(paste(object@values,collapse='_'))
		 }
)

setGeneric("updateKnownKeys",function(object,key){standardGeneric("updateKnownKeys")})
setMethod("updateKnownKeys","ObjectQuery",
	      function(object,key){
	      	if(length(object@known_keys)==0){
	      	 object@known_keys <- key
	      	}
	      	else{
	      	 object@known_keys <- unique(rbind(object@known_keys,key))
	      	}
	      	return(object)
	      }
)


setGeneric("isKeyKnown",function(object,key){standardGeneric("isKeyKnown")})
setMethod("isKeyKnown","ObjectQuery",
	      function(object,key){
	      	rval <- FALSE
	      	test <- merge(object@known_keys,key,by=colnames(object@known_keys))
	      	if(nrow(test)>0)rval <- TRUE
	      	return(rval)
	      }
)

setGeneric("isKeyKnownInLocalStore",function(object,key){standardGeneric("isKeyKnownInLocalStore")})
setMethod("isKeyKnownInLocalStore",
          signature(object = "ObjectQuery",
                    key = "data.frame"),
          function(object,key){
            return(isKeyKnown(object, key))
          }
)

#' A VIRTUAL S4 class implementing basic functionalities of objectstore.
#'
#' Objectstore is an object that can store named datasets in separate environment
#' the datasets are stored and accessed using unique key values.
#' Objetstore also stores it's content to .Rds file and can be loaded later.
#' The API implements handling of reading and writing to the file.
#'
#' Prototype is made only once and then copied with each call to 'new'
#' for objects along the same branch in the class hierachy.
#' Must ensure new environments created since they are passed
#' by reference.
#'
#' @slot stored      "environment"
#' @slot id          "character"
#' @slot data_path   "character"
#' @slot objectstore_q "ObjectQuery"

setClass(
	Class           = "VirtualObjectStore",
	representation	= representation(
		stored         = "environment",
		id             = "character",
		data_path       = "character",
		objectstore_q   = "ObjectQuery",
		objectstore_key = "data.frame"
	),
	contains = c("VIRTUAL")

)




#' Get ID of the objectstore
#'
#' @param object object of class "VirtualObjectStore"
#' @return \code{id} "character" object id
#' @export

setGeneric("getID",function(object){standardGeneric("getID")})

#' @describeIn getID Get ID of the objectstore
#'
#' @inheritParams getID
#'
#' @return \code{id} "character" object id
#' @export
setMethod("getID","VirtualObjectStore",
          function(object){
            return(object@id)
          }
)

#' Initialize method for "VirtualObjectStore" class
#'
#' @param .Object, object of class "VirtualObjectStore"
#' @param id "character" name of the objectstore
#' @return \code{.Object} object of class "VirtualObjectStore"

setMethod("initialize", "VirtualObjectStore",
          function(.Object,id){
            .Object@stored <- new.env(parent = emptyenv())
            .Object@id <- id
            .Object
          }
)




setGeneric(".generateKeyFromID",function(object, objectstore_q){standardGeneric(".generateKeyFromID")})
setMethod(".generateKeyFromID",
          signature( object = "VirtualObjectStore"),
          function(object){

            id <- getID(object)

            name <- key_from_name(id)

            return(name)
          }
)


#' Get key of the objectstore
#'
#' @param object object of class "VirtualObjectStore"

setGeneric("getObjectstoreKey",function(object){standardGeneric("getObjectstoreKey")})

#' @describeIn getObjectstoreKey Get ID of the objectstore
#'
#' @inheritParams getObjectstoreKey
#'
#' @return \code{key} "data.frame" with query defining store
setMethod("getObjectstoreKey","VirtualObjectStore",
          function(object){
            slot_names <- slotNames(object)
            if ("objectstore_key" %in% slot_names && nrow(object@objectstore_key) > 0) {
              return(object@objectstore_key)
            }
            else {
              return(.generateKeyFromID(object))
            }

          }
)


#' Set objectstore key
#'
#' Private method to store object key
#'
#' @rdname private_setObjectStoreKey
#' @param object object of class "VirtualObjectStore"
#' @param objectstore_key "data.frame" with key defining objectstore

setGeneric(".setObjectStoreKey",function(object, objectstore_key){standardGeneric(".setObjectStoreKey")})
setMethod(".setObjectStoreKey",
          signature( object = "VirtualObjectStore",
                     objectstore_key = "data.frame"),
          function(object, objectstore_key){
            object@objectstore_key <- objectstore_key
            return(object)
          }
)


#' Get objectstore query object
#'
#' @param object object of class "VirtualObjectStore"
#' @export

setGeneric("getObjectStoreQuery",function(object){standardGeneric("getObjectStoreQuery")})

#' @describeIn getObjectStoreQuery
#'
#' Get ID of the objectstore
#'
#' @inheritParams getObjectStoreQuery
#'
#' @return \code{objectstore_q} "character" object of class "ObjectQuery"
#' @export
setMethod("getObjectStoreQuery","VirtualObjectStore",
          function(object){
            return(object@objectstore_q)
          }
)

#' Set objectstore query object
#'
#' Private method to store objectquery object
#'
#' @rdname private_setObjectStoreQuery
#' @param object object of class "VirtualObjectStore"
#' @param objectstore_q object of class "ObjectQuery"

setGeneric(".setObjectStoreQuery",function(object, objectstore_q){standardGeneric(".setObjectStoreQuery")})
setMethod(".setObjectStoreQuery",
          signature( object = "VirtualObjectStore",
                     objectstore_q = "ObjectQuery"),
          function(object, objectstore_q){
            object@objectstore_q <- objectstore_q
            return(object)
          }
)


setGeneric("getPath",function(object){standardGeneric("getPath")})
setMethod("getPath","VirtualObjectStore",
		  function(object){
		  	return(gsub(" ","",paste(object@data_path,"/",object@id,"_objectstore.rds",sep="")))
		  }
)


setGeneric("loadObject",function(object){standardGeneric("loadObject")})
setMethod("loadObject","VirtualObjectStore",
		  function(object){
		  	pth <- getPath(object)
		  	message(paste("Object store loading from path:",pth))
		  	object@stored <- readRDS(pth)
		  	return(object)
		  }
)


setGeneric("saveObject",function(object){standardGeneric("saveObject")})
setMethod("saveObject","VirtualObjectStore",
		  function(object){
		  	pth <- getPath(object)
		  	message(paste("Object store saving to path:",pth))
		  	saveRDS(object@stored,pth)
		  }
)


setGeneric("placeInObjectStore",function(object,item,name){standardGeneric("placeInObjectStore")})
setMethod("placeInObjectStore","VirtualObjectStore",
	      function(object,item,name){
	      	object@stored[[name]] <- item
	      	return(object)
	      }
)

setGeneric("removeFromObjectStore",function(object,name){standardGeneric("removeFromObjectStore")})
setMethod("removeFromObjectStore",
          signature(object = "VirtualObjectStore",
                    name = "character"),
          function(object, name){

            if(name %in% ls(object@stored)){
              remove(list = name, envir = object@stored)
              message(sprintf("Removed object %s from objectstore %s",
                              name, getID(object)))
            } else {
              message(sprintf("Requested to remove nonexistent object %s from objectstore %s",
                              name, getID(object)))
            }

            return(object)

          }
)

setGeneric("getFromObjectStore",function(object,name){standardGeneric("getFromObjectStore")})
setMethod("getFromObjectStore","VirtualObjectStore",
	      function(object,name){
	      	return(object@stored[[as.character(name)]])
	      }
)

setGeneric("getNamesFromStore",function(object){standardGeneric("getNamesFromStore")})
setMethod("getNamesFromStore","VirtualObjectStore",
          function(object){
            rval <- NULL
            if(length(object@stored)==0){
              message("No objects loaded, load the store first.")
            }
            else{
              rval <- ls(object@stored)
            }
            return(rval)
          }
)


