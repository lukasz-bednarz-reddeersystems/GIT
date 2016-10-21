#' @include datastore.r
NULL



####################################################
#
# DataStore.VirtualSQLProcedureCall Class
#
####################################################

#' Virtual S4 class handling sql procedures calls to DataStore DB.
#'
#' Implements handling of access to data via sql stored procedures calls.
#' Inherits from "VirtualSQLProcedureCall"
#'
#' @export
setClass(
  Class     = "DataStore.VirtualSQLProcedureCall",
  prototype = list(
    db_name        = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema      = "Razor",
    key_cols       = c("InstrumentID", "Date"),
    key_values     = data.frame(RiskModelName = character(),
                                Date = as.Date(character())),
    results_parser = TE.SQLQuery:::convert_column_class,
    arguments    = c("@lInstrumentID", "@dtStartDate", "@dtEndDate")
  ),
  contains  = c("VirtualSQLProcedureCall", "VIRTUAL")
)

setMethod("getQueryKeyValues",
          signature(object = "DataStore.VirtualSQLProcedureCall"),
          function(object){
            return(object@key_values)
          }
)


setMethod("getQueryKeyColumnNames",
          signature(object = "DataStore.VirtualSQLProcedureCall"),
          function(object){
            return(object@key_cols)
          }
)


#' An S4 class to represent generic data set.
#'
#' DataStore maintains a DataSet with an associated source SQL
#' procedure call
#' such thatdata already obtained is cached
#'
#' Inherits from: "VirtualDataStore"
#'                "VirtualSQLQueryHandler"
#'

setClass(
  Class          = "DataStore.SQL",
  slots          = c(
    sql_query    = "DataStore.VirtualSQLProcedureCall"
  ),
  contains = c("VirtualDataStore",
               "VirtualSQLQueryHandler")
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
