sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


################################################################################
#
# ValueTradedInLongShortHedgeAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of 
# Traded Value categorized per long , short and hedge strats
#
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "ValueTradedData",
  prototype         = list(
    required_colnms = c('Strategy','InstrumentID','Date','ValueUSD','TodayPL','TradeID','MarketValue')
  ),
  contains          = c("VirtualTradeData")
)


setClass(
  Class             = "ValueTradedInLongShortHedgeAnalysisBlock",
  slots             = c(
    output          = "ValueTradedData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    output          = new("ValueTradedData"), 
    required_colnms = c('Strategy','InstrumentID','Date','ValueUSD','TodayPL','TradeID','MarketValue')
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "ValueTradedInLongShortHedgeAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("dataRequest",
          signature(object = "ValueTradedInLongShortHedgeAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trade_data <- getTradeDataObject(object)
            
            trade_data_keys <- key_values
            trader <- trade_data_keys$trader
            start <- trade_data_keys$start
            end <- trade_data_keys$end
            
            colnames(trade_data_keys) <- .translateDataSourceColumnNames(object, colnames(key_values))
            
            if (getStoredNRows(trade_data) == 0) {
              # retrieve trade reference data for query key_values
              trade_data <- tryCatch({
                dataRequest(trade_data, trade_data_keys)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
              })
              
              object <- .setTradeDataObject(object, trade_data)
            
            }
            
            return(object)
          }
)



setMethod("Process",  
          signature(object = "ValueTradedInLongShortHedgeAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            history_data <- getTradeDataObject(object)
            
            history_data <- getReferenceData(history_data)
            
            # compute output
            trd_data <- unique(history_data[c('Strategy','InstrumentID','Date','ValueUSD','TodayPL','TradeID','MarketValue')])
            hedge_strats <- '_SHEDGE'
            
            trd_data$ST <- NA
            trd_data$ST[trd_data$MarketValue<0] <- 'S'
            trd_data$ST[trd_data$MarketValue>0] <- 'L'
            trd_data$ST[grepl(hedge_strats,trd_data$Strategy)] <- 'HEDGE'
            trd_data$Month <- format(trd_data$Date,'%Y-%m')
            ttl_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$ST,Month=trd_data$Month),function(x)sum(x,na.rm=TRUE))
            cum_stats <- trd_data
            cum_stats[is.na(cum_stats)] <- 0
            cum_stats <- cum_stats[order(cum_stats$Date),]
            for(s in c('S','L','HEDGE')){
              cum_stats[cum_stats$ST==s,]$TodayPL <- cumsum(cum_stats[cum_stats$ST==s,]$TodayPL)
            }
            trd_data <- trd_data[!is.na(trd_data$TradeID),]
            
            trd_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$ST,Month=trd_data$Month),function(x)sum(x,na.rm=TRUE))
            
            turn_plot <- ggplot(data=trd_pl_smrry,aes(x=Month,y=ValueUSD,group=Strategy,colour=Strategy)) +
              geom_line(size=1)
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, trd_data)
            object <- .setOutputObject(object, outp_object)

            
            object <- .setOutputGGPlotData(object, trd_pl_smrry)

            object <- .setOutputGGPlot(object, turn_plot)
            
            return(object)
          }
)
