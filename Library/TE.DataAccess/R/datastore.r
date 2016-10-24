#' @include dataset.r keymap.r urldatareader.r
NULL

# @exportClass NullableResult
setClassUnion("NullableResult",c("NULL","DataSet"))

#' A virtual S4 class to represent generic data set.
#'
#' DataStore maintains a DataSet with an associated source query
#' such thatdata already obtained is cached
#'
#' @slot dataset      "DataSet"
#' @slot key_map      "KeyMap"
#' @slot last_result  "NullableResult"

setClass(
  Class          = "VirtualDataStore",
  representation = representation(
    dataset      = "DataSet",
    last_result  = "NullableResult",
    key_map      = "KeyMap"
  ),
  contains = c("VIRTUAL")
)

setGeneric("updateStore", function(object,values,get_variables){standardGeneric("updateStore")})

setGeneric("queryStore", function(object,values,get_variables){standardGeneric("queryStore")})
setMethod("queryStore", "VirtualDataStore",
          function(object,values,get_variables){

            key_cols <- colnames(values)
              values <- unique(values)
              if(length(object@dataset) > 0)
              {
                if(nrow(object@dataset@data) > 0)
                {
                  object@dataset <- keyDiff(object@dataset,values)
                  missing_keys <- object@dataset@last_result
                }
                else
                {
                  missing_keys <- values
                }
              }
              else
              {
                message("No dataset found, attempting to intialise using input data.")
                object@dataset <- new("DataSet", colnames(values), indexed  = TRUE, unique_rows = TRUE)
                missing_keys <- values
              }
              if(nrow(missing_keys)>0){

                object <- updateStore(object,missing_keys,get_variables)
              }

              #object@last_result@data_cols <- get_variables
              object@last_result <- tryCatch({
                innerJoinFrame(object@dataset,values,key_cols)
                },error=function(cond){
                  message(paste("Error creating datastore query result:",cond))
                  return(new("DataSet"))
                })
              if(nrow(object@last_result@data)>0){
                object@last_result <- tryCatch({
                  last_result_df <- object@last_result@data[c(key_cols,get_variables)]

                  # removing rows that do not have any data
                  have_data <- apply(last_result_df[get_variables], 1, function(x){any(!is.na(x))})
                  last_result_df <- last_result_df[have_data,]

                  resetData(object@last_result,last_result_df)
                },error=function(cond){
                  message(paste("Error resetting last result field in datastore:",cond))
                  return(new("DataSet"))
                })
              }
              return(object)
          }
)

# @exportClass DataClass
setClassUnion("DataClass",c("VirtualDataStore","DataSet"))
setGeneric("getLastResult", function(object){standardGeneric("getLastResult")})
setMethod("getLastResult", "DataClass",
          function(object){
            return(object@last_result)
          }
)



#' An S4 class to represent generic data set.
#'
#' DataStore maintains a DataSet with an associated source URL
#' such thatdata already obtained is cached
#'
#' @slot dataset      "DataSet"
#' @slot urlparser    "URLParser"
#' @slot urlquery     "URLQuery"
#' @slot key_map      "KeyMap"
#' @slot last_result  "NullableResult"

setClass(
  Class          = "DataStore.URL",
  representation = representation(
    urlparser    = "URLParser",
    urlquery     = "URLQuery"
  ),
  contains = c("VirtualDataStore")
)



setMethod("updateStore",
          signature(object  = "DataStore.URL",
                    values  = "data.frame",
                    get_variables = "character"),
          function(object,values,get_variables){

            message("Data not cached, updating datastore...")

            data <- object@dataset@data
            object@key_map <- mapFields(object@key_map,values)
            num_key_values <- numberKeyValues(object@key_map)

            for(values_row in 1:num_key_values){
              object@urlquery <- getCurrentKeyQuery(object@key_map,object@urlquery)
              object@key_map <- advanceCurrentKey(object@key_map)
              object@urlquery <- buildURL(object@urlquery)
              object@urlparser <- tryCatch({
                runURLs(object@urlparser,c(object@urlquery@url))
              }, error=function(cond){
                message(paste("Error fetching URL data:",cond))
                return(object@urlparser)
              })
              url_data <- getURLData(object@urlparser,1)

              # merging with original keys to avoid unnecessary querrries for weekends
              url_data <- merge(url_data, values, all = TRUE)

              cn <- getColnames(object@urlparser)
              if(length(cn)==0)cn <- colnames(data)
              if(length(url_data)==0 && length(cn)>0){
                diff <- setdiff(cn,colnames(values))

                # URL query has reduced number of keys after field mapping therefore we need to set all rows of URL data
                # to avoid future unnecessary querries
                if(num_key_values == 1 & nrow(values) > num_key_values) {
                  url_data <- cbind(values,data.frame(t(rep(NA,length(diff)))))
                } else {
                  url_data <- cbind(values[values_row,],data.frame(t(rep(NA,length(diff)))))
                }
                colnames(url_data) <- c(colnames(values),diff)
              }
              if(length(url_data)>0){
                if(length(colnames(values)) != length(intersect(colnames(values),colnames(url_data)))){
                  #This is a bit of a hack at the moment... will work for url types that return singleton data items within a date range
                  #but will not work if the url returns data computed over the range. Need to handle differnt range types.
                  adding <- setdiff(colnames(values),colnames(url_data))
                  message(paste("Not all key columns are contained in the datastore",class(object)[[1]],", adding",paste(adding,collapse=",")))
                  nmes <- colnames(url_data)
                  url_data <- merge(values[values_row,],url_data,by=intersect(colnames(values),colnames(url_data)))
                  #colnames(url_data) <- c(adding,nmes)
                }
                data <- unique(url_data)
                object@dataset <- initialiseOrAppendData(object@dataset,data)
              }
              else{
                message("Could not update data store... gave up.")
              }
            }
            return(object)
          }
)
