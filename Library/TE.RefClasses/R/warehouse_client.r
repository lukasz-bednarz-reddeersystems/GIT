#' @include datasource_client.r
#' @include warehouse_client_functions.r
NULL


####################################
#
# VirtualWarehouseClient Class
#
####################################

pass_thru_results_parser <- function(x){return(x)}


#' Virtual S4 class handling data access via warehouse.
#'
#' Inherits from "VirtualDataSourceClient

setClass(
  Class                = "VirtualWarehouseClient",
  prototype = list(
    key_cols = c("id", "start", "end")
  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

setMethod("dataRequest",
          signature(object = "VirtualWarehouseClient", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object,key_values)

            values <- getDataSourceReturnColumnNames(object)
            colnames_map <- getDataSourceClientColumnNameMap(object)

            first <- TRUE

            for(i_row in seq(nrow(key_values))) {

              trader <- as.integer(key_values$id[i_row])
              start <- as.Date(key_values$start[i_row])
              end <- as.Date(key_values$end[i_row])

              # using yearly store.
              # wh_str_key <- dated_whole_year_lookback(trader, end)
              # using monthly store.
              wh_str_key <- dated_full_month(trader, end)

              wh_str_name <- warehouse_name_from_key(wh_str_key)
              wh_str <- warehouse_objectstore_factory(wh_str_name)
              # update warehouse store
              wh_str <- tryCatch({
                queryWarehouseStore(wh_str, wh_str_key$id,
                                            wh_str_key$start,
                                            wh_str_key$end )
              }, error = function(cond){
                message(paste("Error occured during update of warehouse store" , wh_str_name))
                message(sprintf("during query for trader: %s, start: %s, end: %s" , trader, start, end))
                wh_str
              })

              wh <- getWarehouseFromStore(wh_str, wh_str_key$id,
                                                  wh_str_key$start,
                                                  wh_str_key$end)

              if (is.null(wh)) {
                query_data <- .generateDataFilledWithNA(object, trader, start, end)
              } else {

                query_data <- getData(getRawPositionData(wh))
              }

              query_data <- query_data[(query_data$Date >= start) & (query_data$Date <= end), ]

              if (first) {
                ret_data <- query_data
                first <- FALSE
              } else{
                ret_data <- rbind(ret_data[!(ret_data$Date %in% unique(query_data$Date)),], query_data)
              }

            }

            ret_data <- ret_data[values]

            if (0 == nrow(ret_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }

            # translating column names
            colnames(ret_data) <- .translateDataSourceColumnNames(object, values)

            # forcing new variables set
            object <- .setRequiredVariablesNames(object, colnames(ret_data))
            object <- .setStoredVariablesNames(object, colnames(ret_data))

            # storing Reference data internaly
            object <- setReferenceData(object, ret_data)
            #object <- .PostProcessResultsData(object)

            return(object)
          }
)

