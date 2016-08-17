#' @include datasource_client.r
NULL

################################################################################
#
# MarketData Class
#
# Wrapper class to retrieve market data symbols as REference Class
#
###############################################################################

#' Concrtete S4 class handling Market data from external sources.
#'
#' Implements handling and retrieval of Market data from various sources
#' via quantmod library. Also computes log returns from Close price.
#'
#' Inherits from "VirtualDataSourceClient"
#' @export

setClass(
  Class             = "MarketData",
  prototype         = list(
    key_cols           = c("symbol", "start", "end"), # query keys column names
    key_values         = data.frame(symbol = character(),
                                    start = as.Date(character()),
                                    end = as.Date(character())), # query keys
    values             = c("Close"),
    column_name_map    = hash(c("Close"), c("Return")),
    required_colnms    = c("Date")
  ),
  contains          = c("VirtualDataSourceClient")
)

#' @export
setMethod("initialize",
          signature(.Object = "MarketData"),
          function(.Object, symbol, start, end){
            .Object <- dataRequest(.Object, data.frame(symbol, start, end))

            return(.Object)
          })


setMethod("dataRequest",
          signature(object = "MarketData", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object,key_values)
            values <- getDataSourceReturnColumnNames(object)
            req_cols <- getRequiredVariablesNames(object)

            query_data <- getSymbols(as.character(unique(key_values$symbol)), auto.assign = FALSE)

            query_data <- as.data.frame(query_data)

            cols <- character()
            ret_cols <- character()

            for (col in values) {
              tmp <- grep(paste0(".", col), colnames(query_data), perl = TRUE, value = TRUE)
              cols <- c(cols, tmp)
              ret_cols <- c(ret_cols, gsub(col, .translateDataSourceColumnNames(object, col), tmp))
            }


            tmp_data <- query_data[cols]
            tmp_data[-1,cols] <- apply(tmp_data[cols], 2, function(x){diff(x)/x[-1]})

            colnames(tmp_data) <- ret_cols

            tmp_data$Date <- as.Date(rownames(tmp_data))

            query_data$Date <- as.Date(rownames(query_data))

            query_data <- query_data[-1,]

            query_data <- merge(query_data, tmp_data, by = "Date")

            query_data <- query_data[query_data$Date >= min(key_values$start) & query_data$Date <= max(key_values$end),]

            object <- setReferenceData(object, query_data)

            return(object)
          }
)

setClassUnion("NullableMarketData" , c("NULL", "MarketData"))

################################################################################
#
# MarketDataSX5P Class
#
# Wrapper class to retrieve market data symbols as REference Class
#
###############################################################################

#' Concrtete S4 class handling SX5P index data.
#'
#' Implements handling and retrieval of SX5P index
#' via quantmod library
#'
#' Inherits from "MarketData"
#' @export

setClass(
  Class             = "MarketDataSX5P",
  prototype         = list(
    required_colnms    = c("Date", "SX5P.Return")
  ),
  contains          = c("MarketData")
)

#' @export
setMethod("initialize",
          signature(.Object = "MarketDataSX5P"),
          function(.Object, start, end){
            .Object <- dataRequest(.Object, data.frame(symbol = "^SX5P", start, end))

            return(.Object)
          })

setClassUnion("NullableMarketDataSX5P" , c("NULL", "MarketDataSX5P"))

################################################################################
#
# MarketDataSX5E Class
#
# Wrapper class to retrieve market data symbols as REference Class
#
###############################################################################

#' Concrtete S4 class handling SX5E index data.
#'
#' Implements handling and retrieval of SX5E index
#' via quantmod library
#'
#' Inherits from "MarketData"
#' @export

setClass(
  Class             = "MarketDataSX5E",
  prototype         = list(
    required_colnms    = c("Date", "SX5E.Return")
  ),
  contains          = c("MarketData")
)

#' @export
setMethod("initialize",
          signature(.Object = "MarketDataSX5E"),
          function(.Object, start, end){
            .Object <- dataRequest(.Object, data.frame(symbol = "^SX5E", start, end))

            return(.Object)
          })

setClassUnion("NullableMarketDataSX5E" , c("NULL", "MarketDataSX5E"))
