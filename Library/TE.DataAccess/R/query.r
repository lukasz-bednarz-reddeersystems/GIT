#' An S4 class implementing basic query accessors and functions.
#'
#' @slot fields      "character",
#' @slot values      "character",

setClass(
	Class = "VirtualQuery",
	representation = representation(
		fields     = "character",
		values     = "character"
	),
	contains = c("VIRTUAL")
)

setGeneric("getQueryKeyColumnNames",function(object){standardGeneric("getQueryKeyColumnNames")})
setMethod("getQueryKeyColumnNames",
          signature(object = "VirtualQuery"),
          function(object){
            return(object@fields)
          }
)


setGeneric("getQueryKeyValues",function(object){standardGeneric("getQueryKeyValues")})
setMethod("getQueryKeyValues",
          signature(object = "VirtualQuery"),
          function(object){
            return(object@values)
          }
)


setGeneric("setQueryValuesFromKey",function(object,key){standardGeneric("setQueryValuesFromKey")})
setMethod("setQueryValuesFromKey",
          signature(object = "VirtualQuery",
                    key    = "data.frame"),
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
setMethod("getQueryValueByField",
          signature(object = "VirtualQuery",
                    field    = "character"),
          function(object,field){
          	return(object@values[object@fields==field])
          }
)


setGeneric("setQueryValueByField",function(object,field,value){standardGeneric("setQueryValueByField")})
setMethod("setQueryValueByField",
          signature(object = "VirtualQuery",
                    field  = "character",
                    value  = "ANY"),
          function(object,field,value){
            object@values[object@fields==field] <- as.character(value)
            return(object)
          }
)

