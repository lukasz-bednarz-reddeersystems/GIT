#' Generate keys based on ID and Date columns
#'
#' Generates the values for each field to be retrieved from the data layer
#' also maps the URL field names to the XML variable names, output
#' data.frame column names should match the names of the query
#' fields in the middle layer URL
#'
#' @param x data.frame with columns based on which the keys will be generated
#' @param cols, cols[1] is the name of the ID column, cols[2] is the name of the date column
#' @param id_outname is the output name of the ID
#' @return \code{key_values} data.frame with generated keys
#' @export

date_id_kgen_fn <- function(x,cols,id_outname=NULL){
  ids <- tryCatch(
    {
      if(class(x[[cols[1]]])=="character" || class(x[[cols[1]]])== "factor"){
        as.character(as.matrix(unique(x[cols[1]])))
      }
      else
      {
        as.numeric(as.matrix(unique(x[cols[1]])))
      }

    }, error = function(cond)
    {
      message("Mapping from column names to values failed in date/id key generator.")
    })
  key_values <- data.frame(id=ids[1],start=min(x[x[cols[1]]==ids[1],cols[2]]),end=max(x[x[cols[1]]==ids[1],cols[2]]))
  i <- 2
  while(i <= length(ids)){
    key_values <- rbind(key_values,data.frame(id=ids[i],start=min(x[x[cols[1]]==ids[i],cols[2]],na.rm=TRUE),end=max(x[x[cols[1]]==ids[i],cols[2]],na.rm=TRUE)))
    i <- i+1
  }
  if(length(id_outname)>0)colnames(key_values)[1]<-id_outname
  return(key_values)
}

date_instrument_kgen_fn <- function(x,cols){
  return(date_id_kgen_fn(x,cols))
}

date_strat_kgen_fn <- function(x,cols){
  return(date_id_kgen_fn(x,cols,id_outname='strategy'))
}

date_trader_kgen_fn <- function(x,cols){
  return(date_id_kgen_fn(x,cols))
}

date_trader_bool_kgen_fn <- function(x,cols,bool_field){
  bool_col <- colnames(x)[which(unlist(Map(class,x))=='logical')]
  if(length(bool_col)==0)stop("date_trader_bool_kgen_fn could not find a boolean key column.")
  bool_val <- unique(x[bool_col])
  if(length(bool_val)>1)stop("date_trader_bool_kgen_fn found multiple bool key values, this is not supported.")
  dcols <- setdiff(cols,bool_col)
  kframe <- date_id_kgen_fn(x[dcols],dcols)
  bool_frame <- data.frame(field=as.numeric(bool_val),kframe)
  colnames(bool_frame) <- c(bool_field,colnames(kframe))
  return(bool_frame)
}

date_trader_mbam_kgen_fn <- function(x,cols){
  return(date_trader_bool_kgen_fn(x,cols,'mbam'))
}

simple_id_kgen_fn <- function(x,cols){
  if(ncol(x)>1)stop("Key generator expecting only one ID column")
  ids <- unique(x[cols[1]])
  key_values <- data.frame(id=ids[[1]])
  return(key_values)
}

date_only_kgen_fn <- function(x,cols){
  key_values <- data.frame(start=min(x[[cols[1]]],na.rm=TRUE),end=max(x[[cols[1]]],na.rm=TRUE))
  return(key_values)
}


#' Generate keys based on incoming data frame
#'
#' Directly translates a key value frame to a frame used by the
#' middleware by simply renaming the key value frame columns
#' to the supplied field name
#'
#' @param x data.frame with columns based on which the keys will be generated
#' @param cols, column names used to create keys
#' @param fields name of the columns
#' @return \code{key_values} data.frame with generated keys

direct_keygen <- function(x,cols,fields){
  y <- x[cols]
  dummy <- tryCatch({
              colnames(y) <- fields
           }, error = function(cond){
              message(paste("Direct keygen failed:",cond))
           })
  return(y)
}

watchlist_keygen <- function(x,cols){
  return(direct_keygen(x,cols,c('id','fn','date')))
}


#' An S4 class to handle key generation
#'
#' Handles the relationship between queries for missing data rows
#' and thevalues of the URL fields that obtain that data
#'
#' @slot key_generator "function"
#' @slot key_columns   "character"
#' @slot key_values    "data.frame"
#' @slot current_key   "data.frame"
#' @slot cntr          "numeric"
#' @slot fields        "character"
#' @export

setClass(
  Class          = "KeyMap",
  representation = representation(
    key_generator= "function",
    key_columns  = "character",
    key_values   = "data.frame",
    current_key  = "data.frame",
    cntr         = "numeric",
    fields       = "character"
  ),
  prototype = prototype(
    cntr = 1
  )
)
setGeneric("mapFields", function(object,values){standardGeneric("mapFields")})
setMethod("mapFields", "KeyMap",
          function(object,values){
            object@key_values <- unique(object@key_generator(values,object@key_columns))
            object@fields <- colnames(object@key_values)
            object@cntr <- 1
            object <- tryCatch({
                                  setCurrentKey(object)
                               },error=function(cond){
                                  stop(paste("Set key failed when mapping fields:",cond))
                               })
            return(object)
          }
)
setGeneric("setCurrentKey", function(object){standardGeneric("setCurrentKey")})
setMethod("setCurrentKey","KeyMap",
          function(object){
            kn <- object@cntr%%nrow(object@key_values)+1
            if(ncol(object@key_values)>1){
              object@current_key <- object@key_values[kn,]
            }
            else{
              object@current_key <- data.frame(object@key_values[kn,])
              colnames(object@current_key) <- object@fields
            }
            return(object)
          }
)
setGeneric("numberKeyValues", function(object){standardGeneric("numberKeyValues")})
setMethod("numberKeyValues", "KeyMap",
          function(object){
            return(nrow(object@key_values))
          }
)
setGeneric("advanceCurrentKey", function(object){standardGeneric("advanceCurrentKey")})
setMethod("advanceCurrentKey","KeyMap",
          function(object){
            object@cntr <- object@cntr+1
            object <- tryCatch({
                                  setCurrentKey(object)
                               },error=function(cond){
                                  stop(paste("Set key failed when advancing key:",cond))
                               })
            return(object)
          }

)
setGeneric("getCurrentKeyQuery", function(object,query_obj){standardGeneric("getCurrentKeyQuery")})
setMethod("getCurrentKeyQuery",
          signature("KeyMap",
                    "VirtualQuery"),
          function(object,query_obj){
            values = c()
            query_fields <- getQueryKeyColumnNames(query_obj)

            if(length(query_fields)==0)stop("KeyMap: No fields set within query object.")
            if(length(intersect(query_fields,object@fields))!=length(object@fields))stop("KeyMap: Query object requests unknown fields.")
            for(field in query_fields){
              values <- tryCatch({
                                  c(values,as.character(object@current_key[field][[1]]))
                                 },error=function(cond){
                                  stop(paste("Error when getting key for field",field,"in",class(object)[[1]],":",cond))
                                 })
            }
            query_obj <- setQueryValuesFromKey(query_obj, values)
            return(query_obj)
          }
)
