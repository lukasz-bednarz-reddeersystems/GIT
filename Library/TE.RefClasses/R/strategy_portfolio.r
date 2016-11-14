#' @include portfolio.r
#' @include transformations_handler.r
#' @include transformation_functions.r
#' @include datasource_client.r
NULL


####################################
#
# StrategyPortfolio Class
#
####################################

#' S4 class implementing handling of strategy portfolio
#'
#' This is concrete class handling data retrieval
#' of strategy portfolio data.
#' Inherits from:
#'  "Portfolio"
#'  "VirtualTransformationsHandler"
#'  "VirtualDataSourceClient"
#'
#' @slot trader_id      "numeric"
#' @export

setClass(
  Class          = "StrategyPortfolio",
  slots = c(
    trader_id = "numeric"
  ),
  prototype      = list(
    key_cols           = c("TraderID", "start", "end"), # query keys column names
    key_values         = data.frame(TraderID = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character())),
    values             = c('Name','Trader','UserID','Direction','InstrumentID','Ticker','Date','MarketValue', 'TodayPL'), # columns that neeed to be returned from datastore
    column_name_map    = hash('Name' = 'Strategy',
                              'UserID' = 'TraderID'),
    required_colnms = c('Strategy','TraderID','InstrumentID','Date','Weight')
  ),
  contains = c("Portfolio",
               "VirtualTransformationsHandler",
               "VirtualDataSourceClient")
)

# setMethod("initialize", "StrategyPortfolio", function(.Object, trader = NULL){
#   .Object@trader_id = trader
#   return(.Object)
#
# })


#' Returns TraderID
#'
#' @param object object of class 'StrategyPortfolio'.
#' @export

setGeneric("getTraderID", function(object){standardGeneric("getTraderID")})

#' @describeIn getTraderID
#' Returns TraderID
#'
#' @inheritParams getTraderID
#' @return \code{trader_id} integer.
#' @export
setMethod("getTraderID",
          signature(object = "StrategyPortfolio"),
          function(object){
            return(object@trader_id)
          }
)



#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'StrategyPortfolio'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'StrategyPortfolio'.
#' @export
setMethod("dataRequest",
          signature(object = "StrategyPortfolio", key_values = "data.frame"),
          function(object, key_values){


            datastore_cols = getDataSourceReturnColumnNames(object)
            data_colnames  = .translateDataSourceColumnNames(object, datastore_cols)

            req_key_vals <- aggregate(start~TraderID, data = key_values, min)

            req_key_vals <- merge(req_key_vals,
                                  aggregate(end~TraderID, data = key_values, max),
                                  by = "TraderID")

            first <- TRUE
            for( row_idx in seq(nrow(req_key_vals))) {

              trader <- req_key_vals$TraderID[row_idx]
              start <- req_key_vals$start[row_idx]
              end <- req_key_vals$end[row_idx]

              holdings_data <- position_composite_factory(as.integer(trader),as.Date(start),as.Date(end))

              holdings_data <- holdings_data@data@data[datastore_cols]


              holdings_data <- aggregate(TodayPL ~ ., data = holdings_data, sum)


              colnames(holdings_data) <- data_colnames


              if (first) {
                ret_data <- holdings_data
                allocation <- get_trader_allocation(trader,start,end)
                first <- FALSE
              } else {
                ret_data <- rbind(ret_data, holdings_data)
                allocation <- rbind(allocation, get_trader_allocation(trader,start,end))
              }

            }

            ret_data <- unique(ret_data)

            # aggregating due to issue with Middleware where for some days position MarketValue can be zero
            # and non-zero in the same day
            aggregate_data <- aggregate(MarketValue ~ TraderID + InstrumentID + Date, FUN = sum, data = ret_data)
            ret_data <- merge(unique(ret_data[, !(colnames(ret_data) %in% "MarketValue")]),
                                   aggregate_data,
                                   by = c("TraderID", "InstrumentID", "Date"),
                                   all.y = TRUE)

            aggregate_data <- aggregate(TodayPL ~ TraderID + InstrumentID + Date, FUN = sum, data = ret_data)
            ret_data <- merge(unique(ret_data[, !(colnames(ret_data) %in% "TodayPL")]),
                              aggregate_data,
                              by = c("TraderID", "InstrumentID", "Date"),
                              all.y = TRUE)

            allocation$Month <- format(allocation$Date,'%Y-%m')
            ret_data$Month <- format(ret_data$Date,'%Y-%m')
            ret_data <- merge(ret_data,unique(allocation[c('TraderID','Allocation','Month')]),by = c('Month', 'TraderID'))
            ret_data$Weight <- ret_data$MarketValue/ret_data$Allocation


            if (getForceUniqueRows(object)) {
              object <- setReferenceData(object,unique(ret_data))
            }else {
              object <- setReferenceData(object, ret_data)
            }

            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object StrategyPortfolio become invalid after call to buildPortfolioHistory", cond))
              stop("Failure when building PortfolioHistory")
            }
            )
            return(object)
          }
)

#' Pool data required for portfolio
#'
#' Pools data from data source specific to derived class implementation
#' Also computes portfolio weights if necessary
#'
#' @param object object of class 'StrategyPortfolio'.
#' @param start Date start of data range.
#' @param end Date end of data range.
#' @return \code{object} object of class 'StrategyPortfolio'.
#' @export
setMethod("buildPortfolioHistory",
          signature(object = "StrategyPortfolio"),
          function(object,start,end){

            trader <- getTraderID(object)
            object <- setStartDate(object,start = start)
            object <- setEndDate(object, end = end)

            holdings_data <- getReferenceData(object)

            if (nrow(holdings_data) == 0) {
              key_vals <- data.frame(TraderID = trader, start = start, end = end)
              object <- dataRequest(object, key_vals)

            }

            return(object)

          }
)






