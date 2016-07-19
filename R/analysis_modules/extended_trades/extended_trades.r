sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
# sourceTo("../analysis_modules/analysis_block/price_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
# sourceTo("../analysis_modules/analysis_block/position_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


################################################################################
#
# ExtendedTradesAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "ExtendedTradesAnalysisBlock",
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    output          = new("ExtendedTradeData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)


setMethod("dataRequest",
          signature(object = "ExtendedTradesAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trade_data <- getTradeDataObject(object)
            # position_data <- getPositionDataObject(object)
            # price_data <- getPriceDataObject(object)
            
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
          signature(object = "ExtendedTradesAnalysisBlock"),
          function(object, key_values){
            
            
            trade_rd <- getTradeDataObject(object)
            
            # retrieve needed ref_data
            trades <- getReferenceData(trade_rd)
            trades <- trades[!is.na(trades$TradeID), ]
            
            
            # threshold for finding outliers
            lf_thr <- 10
            
            # generate extended stock classifiers
            
            x <- (trades$MidOnEntry-trades$PriceMavg)/(trades$MidOnEntry*trades$VolInto/10000)
            trades$NFromMAVG20 <- clean_data(x, rm.outliers = "lofactor", scale = TRUE)
            
            x <- (trades$MidOnEntry-trades$MavgPrice50)/(trades$MidOnEntry*trades$DailyN/100)
            trades$NFromMAVG50 <- clean_data(x, rm.outliers = "lofactor", scale = TRUE)
            
            trades$RSI14 <- clean_data(trades$RSI14, scale = TRUE)
            trades$RelativeRSI14 <- clean_data(trades$RelativeRSI14, scale = TRUE)
            
            
            # calculate cash flow version of return
            # trades_mv <- aggregate(trades["ValueUSD"], list(Date       = trades$Date,
            #                                              InstrumentID = trades$InstrumentID,
            #                                              Strategy     = trades$Strategy,
            #                                              MarketValue  = trades$MarketValue,
            #                                              TodayPL      = trades$TodayPL,
            #                                              Sign         = ifelse(trades$Long, 1, -1)), sum)
            # 
            # trades_mv <- aggregate(trades_mv$ValueUSD * trades_mv$Sign, list(Date         = trades_mv$Date,
            #                                                                  InstrumentID = trades_mv$InstrumentID,
            #                                                                  Strategy     = trades_mv$Strategy,
            #                                                                  TodayPL      = trades_mv$TodayPL,
            #                                                                  MarketValue  = trades_mv$MarketValue,
            #                                                                  Sign         = trades_mv$Sign), sum)
            # 
            # colnames(trades_mv) <- c("Date", "InstrumentID", "Strategy", "TodayPL", "MarketValue", "Sign", "TradeValue")
            # 
            # trades_mv$PrevMarketValue <- trades_mv$MarketValue - trades_mv$TradeValue - trades_mv$TodayPL*sign(trades_mv$MarketValue)
            # 
            # trades_mv$Return <- clean_data(trades_mv$TodayPL / abs(trades_mv$PrevMarketValue + trades_mv$TradeValue))
            # 
            # trades <- merge(trades, trades_mv[c("Date", "InstrumentID", "Strategy", "Return")],
            #                 by = c("Date", "InstrumentID", "Strategy"))
            
            trades$Return <- clean_data(trades$TodayPL/trades$MarketValue)
            
            # save trades for future use
            trade_rd <- setReferenceData(trade_rd, trades)
            object <- .setTradeDataObject(object,trade_rd)
            
            # compute extended trades
            distance_measure <- 'NFromMAVG20'
            rsi_measure <- 'RSI14'
            thr <- 1
            
            trd <- trades[!is.na(trades[distance_measure])&!is.na(trades[rsi_measure]),]
            extended_trades <- trd[trd$Long==1&(trd[distance_measure]>thr|trd[rsi_measure]>thr),]
            extended_trades <- rbind(extended_trades,trd[trd$Long==0&(trd[distance_measure]<(-thr)|trd[rsi_measure]<(-thr)),])
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, extended_trades)
            
            object <- .setOutputObject(object, outp_object)
            
            plot_trades <- rbind(cbind(Measure='NFromMAVG20',trades[c('Strategy','Long')],Value=trades$NFromMAVG20),
                                 cbind(Measure='NFromMAVG50',trades[c('Strategy','Long')],Value=trades$NFromMAVG50),
                                 cbind(Measure='RSI14',trades[c('Strategy','Long')],Value=trades$RSI14),
                                 cbind(Measure='RelativeRSI14',trades[c('Strategy','Long')],Value=trades$RelativeRSI14))
            
            object <- .setOutputGGPlotData(object, plot_trades)
            
            extension <- ggplot(data=plot_trades, aes(x=Value, colour=Measure)) +
                          geom_freqpoly(bins = 50) +
                          # scale_x_continuous(limits = c(-5, 5)) +
                          facet_grid( Strategy~Long, scales='free_y') +
                          ylab("") + xlab("") + ggtitle('Stock extension')
            
            object <- .setOutputGGPlot(object, extension)
            
            
            
            return(object)
          }
)
