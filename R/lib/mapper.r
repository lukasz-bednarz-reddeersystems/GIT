#Mapper class enables a mutivalued variable to behave as a single valued variable when appropriate
#(for example when stored within a data column) but to retain all values if reuired (such that 
#all possibilities can be queried for example).
#This is a one to many mapping from the root_domain (which is a key) 
#to the maps_to range (which is multivalued).
#Each point in the range can hold values (at that location in the maps_to list).
library(hash)

setClass(
	Class          = "VirtualMapper",
	representation = representation(
		root_domain   = "hash",
		stored_indexes = "list",
		env = "environment" 
	),
	contains = c('VIRTUAL')      
)



setGeneric(".getStoredIndexes",function(object){standardGeneric(".getStoredIndexes")})
setMethod(".getStoredIndexes", "VirtualMapper",
          function(object){
            return(object@stored_indexes)
          }
)



setGeneric(".setStoredIndexes",function(object, indexes){standardGeneric(".setStoredIndexes")})
setMethod(".setStoredIndexes", "VirtualMapper",
          function(object, indexes){
            object@stored_indexes <- indexes
            return(object)
          }
)

setGeneric(".getDomainNames",function(object){standardGeneric(".getDomainNames")})
setMethod(".getDomainNames", "VirtualMapper",
          function(object){
            
            return(object@env$names)		 
          }
)

setGeneric(".setDomainNames",function(object, names){standardGeneric(".setDomainNames")})
setMethod(".setDomainNames", "VirtualMapper",
          function(object, names){
            
            object@env$names <- names
            
            return(object)		 
          }
)


setGeneric(".checkNameExists",function(object,name){standardGeneric(".checkNameExists")})
setMethod(".checkNameExists", "VirtualMapper",
          function(object,name){
            
            if (!is.null(object@root_domain[[name]]))
              return(TRUE)
            else {
              return(FALSE)
            }
          }
)

setGeneric(".checkValueExists",function(object,value){standardGeneric(".checkValueExists")})
setMethod(".checkValueExists", "VirtualMapper",
          function(object,value){
            
            if (!is.null(invert(object@root_domain)[[value]]))
              return(TRUE)
            else {
              return(FALSE)
            }
          }
)


setGeneric(".getNameIndex",function(object,name){standardGeneric(".getNameIndex")})
setMethod(".getNameIndex", "VirtualMapper",
          function(object,name){
            return(which(.getDomainNames(object) == name))
          }
)

setGeneric(".appendToStoredIndex",function(object,index, i = NULL){standardGeneric(".appendToStoredIndex")})
setMethod(".appendToStoredIndex", "VirtualMapper",
          function(object,index, i = NULL){
            len <- length(object@stored_indexes)
            if (is.null(i) & (len > 1)){
              for (el in seq(len)) {
                if (length(index) == len) {
                  object@stored_indexes[[el]] <- c(object@stored_indexes[[el]] , index[el])
                } else if (length(index) == 1 ) {
                  object@stored_indexes[[el]] <- c(object@stored_indexes[[el]] , index)
                } else {
                  stop("incorrect number of elements to store")
                }
              }
            } else if (is.null(i) & (len == 1)) {
              if (length(index ) == 1) {
                object@stored_indexes[[1]] <- c(object@stored_indexes[[1]] , index)
              } else {
                stop("incorrect number of elements to store")
              }
            } else if (is.null(i) & (len == 0)) {
              if (length(index ) == 1) {
                object@stored_indexes[[1]] <-  index
              } else {
                stop("incorrect number of elements to store")
              }
            } else {
               object@stored_indexes[[i]] <- c(object@stored_indexes[[i]] , index)
            }
            
            return(object)
          }
)

setGeneric(".appendToRootDomain",function(object,name,value){standardGeneric(".appendToRootDomain")})
setMethod(".appendToRootDomain", "VirtualMapper",
          function(object,name, value){
            object@root_domain[[name]] <- value
            object <- .setDomainNames(object, c(.getDomainNames(object), name))
            return(object)
          }
)


setGeneric(".appendToRange",function(object,name,value){standardGeneric(".appendToRange")})
setMethod(".appendToRange", "VirtualMapper",
          function(object,name,value){
            
            name_exists <- .checkNameExists(object, name)
            
            if (!name_exists) {
              object <- .appendToRootDomain(object, name, value)
            }
            
            index <- .getNameIndex(object, name)
            object <- .appendToStoredIndex(object, index)
            
            return(object)
          }
)

setGeneric(".getStoredDomain",function(object){standardGeneric(".getStoredDomain")})
setMethod(".getStoredDomain", "VirtualMapper",
          function(object){
            stored_indexes <- .getStoredIndexes(object)
            domain_names <- .getDomainNames(object)
            
            stored_names <- lapply(stored_indexes, function(x){ domain_names[x]})
            
            
            return(stored_names)		 
          }
)

setGeneric(".getStoredRange",function(object){standardGeneric(".getStoredRange")})
setMethod(".getStoredRange", "VirtualMapper",
          function(object){
            stored_names <- .getStoredDomain(object)
            stored_range <- values(object@root_domain)
            names(stored_range) <- keys(object@root_domain)
            
            stored_range <- lapply(stored_names, function(x){ stored_range[x]})
            
            return(stored_range)		 
          }
)


setGeneric("getRange",function(object){standardGeneric("getRange")})
setMethod("getRange", "VirtualMapper",
	function(object){
	  
	  rv <- .getStoredRange(object)
	  if (length(rv) == 0){
	    return(NULL)
	  } else if(length(rv) == 1) {
	    return(rv[[1]])
	  } else {
	    return(rv)
	  }
	  		 
	}
)

setGeneric("getDomain",function(object){standardGeneric("getDomain")})
setMethod("getDomain", "VirtualMapper",
          function(object){
            rv <- .getStoredDomain(object)
            if (length(rv) == 0){
              return(NULL)
            } else if(length(rv) == 1) {
              return(rv[[1]])
            } else {
              return(rv)
            }
          }
)


setGeneric("appendToRange",function(object,name,value){standardGeneric("appendToRange")})
setMethod("appendToRange", "VirtualMapper",
	function(object,name,value){
	  stored_values <- .getStoredRange(object)
		
	  if(value %in% stored_values) {
	    stop("Cannot add duplicate name to the set.")
	  } else {
	    object <- .appendToRange(object, name, value)
	  }
		return(object)
	}
)



setGeneric("setRange",function(object,range_list){standardGeneric("setRange")})
setMethod("setRange","VirtualMapper",
          function(object,range_list){
            check <- length(unique(na.omit(names(range_list)))) == length(na.omit(unique(range_list)))
            if(!check) {
              stop("Attempt to set range to an inappropriate value.")
            } else {
              object <- .setStoredIndexes(object, list())
              for(name in names(range_list)) {
                object <- appendToRange(object, name, range_list[name])
              } 
            }
            return(object)		 
          }
)

##############################################################################
# override of generic comparator .
#
# Args:
#   object : object of type "VirtualDataStoreClient"
##############################################################################

setMethod("==", signature(e1="VirtualMapper", e2="VirtualMapper"),
          definition=function(e1, e2)
          {
            test <- setequal(.getStoredIndexes(e1), .getStoredIndexes(e2))
            return(test)
          })

setMethod("==", signature(e1="VirtualMapper", e2="ANY"),
          definition=function(e1, e2)
          {
            test <- .checkValueExists(e1, as.character(e2))
            return(test)
          })

setMethod("==", signature(e1="ANY", e2="VirtualMapper"),
          definition=function(e1, e2)
          {
            test <- .checkValueExists(e2, as.character(e1))
            return(test)
          })


setMethod("!=", signature(e1="VirtualMapper", e2="VirtualMapper"),
          definition=function(e1, e2)
          {
            test <- setequal(.getStoredIndexes(e1), .getStoredIndexes(e2))
            return(!test)
          })

setMethod("!=", signature(e1="VirtualMapper", e2="ANY"),
          definition=function(e1, e2)
          {
            test <- .checkValueExists(e1, as.character(e2))
            return(!test)
          })

setMethod("!=", signature(e1="ANY", e2="VirtualMapper"),
          definition=function(e1, e2)
          {
            test <- .checkValueExists(e2, as.character(e1))
            return(!test)
          })

##############################################################################
# override of %in% operator .
#
# Args:
#   object : object of type "VirtualDataStoreClient"
##############################################################################
setGeneric("%in%")
setMethod("%in%", c("ANY", "VirtualMapper"),
          function(x, table)
          {
            sapply(.getStoredRange(table), function(el){ match(x, el, nomatch=0L) > 0})
          })

setMethod("%in%", c("VirtualMapper", "ANY"),
          function(x, table)
          {
            drop(sapply(.getStoredRange(x), function(el){ match( el, table,nomatch=0L) > 0}, USE.NAMES = FALSE))
          })

setMethod("%in%", c("VirtualMapper", "VirtualMapper"),
          function(x, table)
          {
            match(.getStoredRange(x), .getStoredRange(table), nomatch=0L) > 0L
          })

##############################################################################
# override of extraction operators .
#
# Args:
#   object : object of type "VirtualDataStoreClient"
##############################################################################

setMethod("[", c("VirtualMapper", "missing", "missing", "missing"),
          function(x)
          {
            return(x)
          })

setMethod("[", c("VirtualMapper", "ANY", "missing", "missing"),
          function(x, i)
          {
            ret <- x
            stored_indexes <- .getStoredIndexes(x)
            x <- .setStoredIndexes(x, stored_indexes[i])
            return(x)
          })

setMethod("[", c("VirtualMapper", "ANY", "ANY"),
          function(x, i)
          {
            stop("Improper number of subscripts")
          })


setMethod("[<-", c("VirtualMapper", "missing", "missing", "VirtualMapper"),
          function(x,i,j, value)
          {
            if (length(x) != length(value)) {
              stop("incoming data length is not equal to destination length")
            } else {
              return(value)
              }
            
          })

setMethod("[<-", c("VirtualMapper", "ANY", "missing", "VirtualMapper"),
          function(x,i,j, value)
          {
            if (length(i) != length(value)) {
              stop("incoming data length is not equal to destination length")
            } else {
              stored_indexes <- .getStoredIndexes(x)
              stored_indexes[i] <- .getStoredIndexes(value) 
              x <- .setStoredIndexes(x, stored_indexes)
              return(x)
            }
            
          })



#This determines if the root domain maps to a specific value
setGeneric("mapsTo",function(object,...){standardGeneric("mapsTo")})
#This should be implemented in the subclass, it may for example check the names
#of the maps_to list, or it may check the value, or both.
#Should return Boolean value.


setClass(
  Class          = "TestMapper",
  prototype = list(
    stored_indexes = list(),
    root_domain = hash(),
    env = new.env()
  ),
contains = c('VirtualMapper')      
)
