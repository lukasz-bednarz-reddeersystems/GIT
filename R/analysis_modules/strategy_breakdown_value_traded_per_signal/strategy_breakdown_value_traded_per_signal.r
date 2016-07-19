sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/event_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownValueTradedPerSignalAnalysisBlock Class
# 
# Computation of value traded strategy breakdown per signals
# 
# Pulls data required for computation and adds required columns.
###############################################################################

strategy_breakdown_per_signal_base_cols    <- c("InstrumentID","TradeID","Date","Strategy","Long","ValueUSD","TodayPL")
strategy_breakdown_per_signal_signal_cols  <- c("Special.Dividend","Results","Close.Period","Dividend","Trading.Statement",
                                                "AGM","Director.Sell.Non.Core","Conference.Call","Road.Show",
                                                "Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" , 
                                                "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue", 
                                                "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected",
                                                "Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed",
                                                "Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing", 
                                                "New.Issue")


setClass(
  Class             = "TradedSignalsData",
  prototype         = list(
    required_colnms = c("Signal", "Long", "Strategy", "Quarter", "ValueUSD", "TodayPL")
  ),
  contains          = c("VirtualTradeData")
)


setClass(
  Class             = "StrategyBreakdownValueTradedPerSignalAnalysisBlock",
  slots             = c(
    output          = "TradedSignalsData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    required_colnms = c(strategy_breakdown_per_signal_base_cols,
                        strategy_breakdown_per_signal_signal_cols),
    output          = new("TradedSignalsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualEventDataHandler"
                        )
)


setMethod("setTradeDataObject",  
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("setEventDataObject",  
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", event_data = "EventData"),
          function(object, event_data){
            .setEventDataObject(object, event_data)
          }
)


setMethod("dataRequest",
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trade_data_keys <- key_values
            trader <- trade_data_keys$trader
            start <- trade_data_keys$start
            end <- trade_data_keys$end

            colnames(trade_data_keys) <- .translateDataSourceColumnNames(object, colnames(key_values))
            
            trade_data <- getTradeDataObject(object)
            
            # retrieve trade reference data for query key_values
            if (getStoredNRows(trade_data) == 0) {
              trade_data <- tryCatch({
                dataRequest(trade_data, trade_data_keys)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
              })
              
              object <- .setTradeDataObject(object, trade_data)
            }
            
            
            event_data <- getEventDataObject(object)
            
            # retrieve event reference data for query key_values
            if (getStoredNRows(event_data) == 0) {
              
              trade_df <- getReferenceData(trade_data)
              InstrumentIDs <- unique(trade_df$InstrumentID)
              dates <- unique(trade_df$Date)
              
              event_data_keys <- expand.grid(InstrumentID = InstrumentIDs, Date = dates)
              
              event_data <- tryCatch({
                dataRequest(event_data, event_data_keys)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(event_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(event_data), cond))
              })
              
              object <- .setEventDataObject(object, event_data)
            }
            
            
            return(object)
          }
)


setMethod("Process",  
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            trade_data <- getTradeDataObject(object)
            trade_df <- getReferenceData(trade_data)
            sig_hist <- trade_df[!is.na(trade_df$TradeID),]
            
            event_data <- getEventDataObject(object)
            event_df   <- getReferenceData(event_data)
            
            sig_hist <- merge(sig_hist, event_df, by = c("InstrumentID","Date"))
            thisQ <- quarter(max(sig_hist$Date[!is.na(sig_hist$TradeID)]), with_year = TRUE)
            
            
            missing_cols <- setdiff(strategy_breakdown_per_signal_signal_cols, colnames(event_df))
            sig_hist[missing_cols] <- FALSE
            
            
            sig_hist <- unique(sig_hist[c(strategy_breakdown_per_signal_base_cols,
                                          strategy_breakdown_per_signal_signal_cols)])
            
            sig_hist$Q <- quarter(sig_hist$Date, with_year = TRUE)
            
            
            # set merged data as an output to be used by other blocks
            object <- setReferenceData(object, sig_hist)

            acols <- strategy_breakdown_per_signal_signal_cols
            first <-  TRUE
            for(signal in acols){
              sh <- sig_hist[!is.na(sig_hist[[signal]]),]
              sh <- sh[sh[[signal]]==TRUE,]
              if(nrow(sh)>0){
                sig_to <- aggregate(sh[c('ValueUSD','TodayPL')],list(Long=sh$Long,Strategy=sh$Strategy,Quarter=sh$Q),function(x)sum(x,na.rm=TRUE))
                sig_to <- aggregate(sig_to[c('ValueUSD','TodayPL')],list(Long=sig_to$Long,Strategy=sig_to$Strategy,Quarter=(sig_to$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
                sig_to <- cbind(Signal=signal,sig_to)
                if(first){
                  all_sig <- sig_to
                  first <- FALSE
                }
                else{
                  all_sig <- rbind(all_sig,sig_to)
                } 
              }
            }
            all_sig$Signal <- gsub("\\."," ",all_sig$Signal)
            
            output_obj <- getOutputObject(object)
            output_obj <- setReferenceData(output_obj, all_sig)
            object <- .setOutputObject(object, output_obj)
            
            plt_sig_value <- ggplot(data=all_sig, aes(x=Signal, fill=Quarter)) +
              geom_bar(aes(weight=ValueUSD),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("Value traded") + xlab("Strategy") + ggtitle('Strategy breakdown') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            
            object <- .setOutputGGPlotData(object, all_sig)

            object <- .setOutputGGPlot(object, plt_sig_value)
            
            return(object)
          }
)
