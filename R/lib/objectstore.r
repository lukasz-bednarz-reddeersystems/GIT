setClass(
	Class = "ObjectQuery",
	representation = representation(
		fields     = "character",
		values     = "character",
		known_keys = "data.frame"
	)
)

if (!isGenericS4("hashKey")) {
	setGeneric("hashKey",function(object,key){standardGeneric("hashKey")})
}

setGeneric("setQueryValuesFromKey",function(object,key){standardGeneric("setQueryValuesFromKey")})
setMethod("setQueryValuesFromKey","ObjectQuery",
          function(object,key){
            if(class(key)[[1]]!='data.frame')stop("Key must be data.frame with colnames matching fields.")
            if(nrow(key)!=1)stop("key must be exactly one row")
            fill_fields <- colnames(key)
            for(field in fill_fields){
              object <- setQueryValueByField(object,field,key[[field]])
            }
            return(object)
          }
)

setGeneric("getQueryValueByField",function(object,field){standardGeneric("getQueryValueByField")})
setMethod("getQueryValueByField","ObjectQuery",
          function(object,field){
          	return(object@values[object@fields==field])
          }
)

setGeneric("setQueryValueByField",function(object,field,value){standardGeneric("setQueryValueByField")})
setMethod("setQueryValueByField","ObjectQuery",
          function(object,field,value){
            object@values[object@fields==field] <- as.character(value)
            return(object)
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

setClass(
	Class           = "VirtualObjectStore",
	representation	= representation(
		stored      = "environment",
		id          = "character",
		data_path   = "character"
	)
)

#Prototype is made only once and then copied with each call to 'new' 
#for objects along the same branch in the class hierachy.
#Must ensure new environments created since they are passed
#by reference.
setMethod("initialize", "VirtualObjectStore",
          function(.Object,id){
            .Object@stored <- new.env(parent = emptyenv())
            .Object@id <- id
            .Object
          }
)

setGeneric("getID",function(object){standardGeneric("getID")})
setMethod("getID","VirtualObjectStore",
          function(object){
            return(object@id)
          }
)

setGeneric("getPath",function(object){standardGeneric("getPath")})
setMethod("getPath","VirtualObjectStore",
		  function(object){
		  	return(paste(object@data_path,"/",object@id,"_objectstore.rds",sep=""))
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


