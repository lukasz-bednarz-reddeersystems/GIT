sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/strategy_data/strategy_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


################################################################################
#
# StrategyBreakdownAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of strategy breakdown plot
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "StrategyBreakDownData",
  prototype         = list(
    required_colnms = c("Type", "Quantity", "Value", "Strategy", "Quarter" )
  ),
  contains          = c("VirtualStrategyData")
)


setClass(
  Class             = "StrategyBreakdownAnalysisBlock",
  slots             = c(
    output          = "StrategyBreakDownData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    output          = new("StrategyBreakDownData"), 
    required_colnms = c('Strategy','InstrumentID','Date','MarketValue','TodayPL','ValueUSD')
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "StrategyBreakdownAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)


setMethod("dataRequest",
          signature(object = "StrategyBreakdownAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trade_data <- getTradeDataObject(object)
            
            trade_data_keys <- key_values
            trader <- trade_data_keys$trader
            start <- trade_data_keys$start
            end <- trade_data_keys$end
            
            colnames(trade_data_keys) <- .translateDataSourceColumnNames(object, colnames(key_values))
            
            # retrieve trade reference data for query key_values
            trade_data <- tryCatch({
              dataRequest(trade_data, trade_data_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
            })
            
            object <- .setTradeDataObject(object, trade_data)
            return(object)
          }
)



setMethod("Process",  
          signature(object = "StrategyBreakdownAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            history_data <- getTradeDataObject(object)
            
            history_data <- getReferenceData(history_data)
            
            thisQ <- quarter(max(history_data$Date[!is.na(history_data$TradeID)]), with_year = TRUE)
            
            # compute output
            psn_data <- unique(history_data[c('Strategy','InstrumentID','Date','MarketValue','TodayPL')])
            psn_data$Q <- quarter(psn_data$Date, with_year = TRUE)
            strat_sz_smrry <- aggregate(psn_data['MarketValue'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q,Date=psn_data$Date),function(x)sum(abs(x),na.rm=TRUE))
            strat_sz_smrry <- aggregate(strat_sz_smrry['MarketValue'],list(Strategy=strat_sz_smrry$Strategy,Quarter=(strat_sz_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
            strat_pl_smrry <- aggregate(psn_data['TodayPL'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)sum(x,na.rm=TRUE))
            strat_pl_smrry <- aggregate(strat_pl_smrry['TodayPL'],list(Strategy=strat_pl_smrry$Strategy,Quarter=(strat_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
            strat_n_psns <- aggregate(psn_data['InstrumentID'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)length(unique(x[!is.na(x)])))
            strat_n_psns <- aggregate(strat_n_psns['InstrumentID'],list(Strategy=strat_n_psns$Strategy,Quarter=(strat_n_psns$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
            
            trd_data <- unique(history_data[c('Strategy','InstrumentID','Date','ValueUSD','TodayPL','TradeID')])
            trd_data$Q <- quarter(trd_data$Date, with_year = TRUE)
            trd_data <- trd_data[!is.na(trd_data$TradeID),]
            trd_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)sum(x,na.rm=TRUE))
            trd_pl_smrry <- aggregate(trd_pl_smrry[c('TodayPL','ValueUSD')],list(Strategy=trd_pl_smrry$Strategy,Quarter=(trd_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
            trd_n_trds <- aggregate(trd_data['InstrumentID'],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)length(unique(x[!is.na(x)])))
            trd_n_trds <- aggregate(trd_n_trds['InstrumentID'],list(Strategy=trd_n_trds$Strategy,Quarter=(trd_n_trds$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
            
            strategy_data <- rbind(cbind(Type='Position level',Quantity='Value',Value=strat_sz_smrry$MarketValue,strat_sz_smrry[c('Strategy','Quarter')]),
                                   cbind(Type='Position level',Quantity='PL',Value=strat_pl_smrry$TodayPL,strat_pl_smrry[c('Strategy','Quarter')]),
                                   cbind(Type='Position level',Quantity='Count',Value=strat_n_psns$InstrumentID,strat_n_psns[c('Strategy','Quarter')]),
                                   cbind(Type='Trade level',Quantity='Value',Value=trd_pl_smrry$ValueUSD,trd_pl_smrry[c('Strategy','Quarter')]),
                                   cbind(Type='Trade level',Quantity='PL',Value=trd_pl_smrry$TodayPL,trd_pl_smrry[c('Strategy','Quarter')]),
                                   cbind(Type='Trade level',Quantity='Count',Value=trd_n_trds$InstrumentID,trd_n_trds[c('Strategy','Quarter')]))
            
            plt_strat <- ggplot(data=strategy_data, aes(x=Strategy, fill=Quarter)) +
              geom_bar(aes(weight=Value),position="dodge") +
              facet_grid(Quantity~Type, scales="free_y") +
              ylab("") + xlab("Strategy") + ggtitle('Strategy breakdown') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, strategy_data)
            object <- .setOutputObject(object, outp_object)

            
            object <- .setOutputGGPlotData(object, strategy_data)

            object <- .setOutputGGPlot(object, plt_strat)
            
            
            
            return(object)
          }
)
