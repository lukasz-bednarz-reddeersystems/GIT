#' @include analysis_block.r
NULL
################################################################################
#
# StrategyBreakdownValueTradedPerSignalAnalysisBlock Class
#
# Computation of value traded strategy breakdown per signals
#
# Pulls data required for computation and adds required columns.
###############################################################################

#' List of traded signals data base column names
strategy_breakdown_per_signal_base_cols    <- c("InstrumentID","TradeID","Date","Strategy","Long","ValueUSD","TodayPL")

#' List of traded signals names
strategy_breakdown_per_signal_signal_cols  <- c("Special.Dividend","Results","Close.Period","Dividend","Trading.Statement",
                                                "AGM","Director.Sell.Non.Core","Conference.Call","Road.Show",
                                                "Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" ,
                                                "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue",
                                                "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected",
                                                "Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed",
                                                "Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing",
                                                "New.Issue")

devtools::use_data(strategy_breakdown_per_signal_base_cols,
                   strategy_breakdown_per_signal_signal_cols,
                   overwrite = TRUE)

#' TradedSignalsData Reference Data class.
#'
#' Concrete S4 class storing data of signals
#' of stock traded within given timeframe
#'
#' Inherits from "VirtualTradeData"
#'
#' @export

setClass(
  Class             = "TradedSignalsData",
  prototype         = list(
    required_colnms = c("Signal", "Long", "Strategy", "Quarter", "ValueUSD", "TodayPL")
  ),
  contains          = c("VirtualTradeData")
)


#' Analysis Module for computation of breakdown of portfolio
#' attributes per traded signal
#'
#' Computation block class to pull data required for Computation of
#' traded signals breakdown plot.
#' Pulls data required for computation and adds required columns.
#' Generates multifacet signal breakdown ggplot.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualEventDataHandler"
#' @export
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


#' @describeIn setTradeDataObject
#' Set trade_data object in object slot
#' @inheritParams setTradeDataObject
#'
# ' @rdname setTradeDataObject-StrategyBreakdownValueTradedPerSignal-method
# ' @param object object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
# ' @param trade_data object of class "TradeData"
# ' @return \code{object} object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            TE.RefClasses:::.setTradeDataObject(object, trade_data)
          }
)


#' @describeIn setEventDataObject
#' Set event_data object in object slot
#' @inheritParams setEventDataObject
#'
# ' @rdname setEventDataObject-StrategyBreakdownValueTradedPerSignal-method
# ' @param object object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
# ' @param event_data object of class "EventData"
# ' @return \code{object} object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
#' @export

setMethod("setEventDataObject",
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", event_data = "EventData"),
          function(object, event_data){
            TE.RefClasses:::.setEventDataObject(object, event_data)
          }
)

#' @describeIn dataRequest
#'
#' Request data from data source
#'
#' @inheritParams dataRequest
#'
# ' @param object object of class 'StrategyBreakdownValueTradedPerSignalAnalysisBlock'.
# ' @param key_values data.frame with keys specifying data query.
# ' @return \code{object} object of class 'StrategyBreakdownValueTradedPerSignalAnalysisBlock'.
#' @export
setMethod("dataRequest",
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            trade_data_keys <- key_values
            trader <- trade_data_keys$trader
            start <- trade_data_keys$start
            end <- trade_data_keys$end

            colnames(trade_data_keys) <- TE.RefClasses:::.translateDataSourceColumnNames(object, colnames(key_values))

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

              object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
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

              object <- TE.RefClasses:::.setEventDataObject(object, event_data)
            }


            return(object)
          }
)

#' @describeIn Process
#'
#' Trigger computation of analysis data.
#'
#' @inheritParams Process
#'
# ' @param object object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
# ' @return \code{object} object object of class "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "StrategyBreakdownValueTradedPerSignalAnalysisBlock"),
          function(object){

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

            plt_sig_value <- ggplot(data=all_sig, aes_string(x="Signal", fill="Quarter")) +
              geom_bar(aes_string(weight="ValueUSD"),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("Value traded") + xlab("Strategy") + ggtitle('Strategy breakdown') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            object <- .setOutputGGPlotData(object, all_sig)

            object <- .setOutputGGPlot(object, plt_sig_value)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Quarter", "ValueUSD",'TodayPL')))

            return(object)
          }
)
