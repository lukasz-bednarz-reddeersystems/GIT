sourceTo("../common/portfolio/portfolio_with_transformations.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/datasource_client/datasource_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/composite_datasets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)

####################################
#
# StrategyPortfolio Class
#
####################################

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
    values             = c('Name','Trader','UserID','Direction','InstrumentID','Date','MarketValue', 'TodayPL'), # columns that neeed to be returned from datastore
    column_name_map    = hash(c('Name','Trader','UserID','Direction','InstrumentID','Date','MarketValue', 'TodayPL'),
                              c('Strategy','Trader','TraderID','Direction','InstrumentID','Date','MarketValue', 'TodayPL')),
    required_colnms = c('Strategy','TraderID','InstrumentID','Date','Weight')
  ),
  contains = c("PortfolioWithTransformations",
               "VirtualDataSourceClient")
)


setMethod("initialize", "StrategyPortfolio", function(.Object, trader){
  .Object@trader_id = trader
  return(.Object)
  
})

setGeneric("getTraderID", function(object,...){standardGeneric("getTraderID")})
# Returns TraderID slot value.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   TraderID

setMethod("getTraderID", 
          signature(object = "StrategyPortfolio"),
          function(object){
            return(object@trader_id)
          }
)



setMethod("dataRequest",
          signature(object = "StrategyPortfolio", key_values = "data.frame"),
          function(object, key_values){
            
            
            datastore_cols = getDataSourceReturnColumnNames(object)
            data_colnames  = .translateDataSourceColumnNames(object, datastore_cols)
            first <- TRUE
            
            for( row_idx in seq(nrow(key_values))) {
            
              trader <- key_values$TraderID[row_idx]
              start <- key_values$start[row_idx]
              end <- key_values$end[row_idx]
              
              holdings_data <- position_composite_factory(as.integer(trader),as.Date(start),as.Date(end))
              holdings_data <- holdings_data@data@data[datastore_cols]
              
              colnames(holdings_data) <- data_colnames
              
              
              if (first) {
                ret_data <- holdings_data
                first <- FALSE
              } else {
                ret_data <- rbind(ret_data, holdings_data)
              }
            
            }
            
            # aggregating due to issue with Middleware where for some days position MarketValue can be zero
            # and non-zero in the same day
            aggregate_data <- aggregate(MarketValue ~ Trader + InstrumentID + Date, FUN = sum, data = ret_data)
            ret_data <- merge(ret_data[, !(colnames(ret_data) %in% "MarketValue")], 
                                   aggregate_data, 
                                   by = c("Trader", "InstrumentID", "Date"), 
                                   all.y = TRUE)
            
            allocation    <- get_trader_allocation(trader,start,end)
            allocation$Month <- format(allocation$Date,'%Y-%m')
            ret_data$Month <- format(ret_data$Date,'%Y-%m')
            ret_data <- merge(ret_data,allocation[c('TraderID','Allocation','Month')],by = c('Month', 'TraderID'))
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






