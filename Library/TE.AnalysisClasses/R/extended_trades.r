#' @include analysis_block.r
NULL

################################################################################
#
# ExtendedTradesAnalysisBlock Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


#' Analysis Module for extraction of Extended Trades
#'
#' Computation block class compute data for
#' extended trades.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler"
#'
#' @export

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


#' Request data from data source
#'
#' @param object object of class 'ExtendedTradesAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'ExtendedTradesAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "ExtendedTradesAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            trade_data <- getTradeDataObject(object)
            # position_data <- getPositionDataObject(object)
            # price_data <- getPriceDataObject(object)

            trade_data_keys <- key_values
            trader <- trade_data_keys$trader
            start <- trade_data_keys$start
            end <- trade_data_keys$end

            colnames(trade_data_keys) <- TE.RefClasses:::.translateDataSourceColumnNames(object, colnames(key_values))

            # retrieve trade reference data for query key_values
            trade_data <- tryCatch({
              dataRequest(trade_data, trade_data_keys)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
            })

            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            return(object)
          }
)



#' Trigger computation of analysis data.
#'
#' @param object object of class "ExtendedTradesAnalysisBlock"
#' @return \code{object} object object of class "ExtendedTradesAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "ExtendedTradesAnalysisBlock"),
          function(object){


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

            trades$Return <- clean_data(trades$TodayPL/trades$MarketValue)

            # save trades for future use
            trade_rd <- setReferenceData(trade_rd, trades)
            object <- TE.RefClasses:::.setTradeDataObject(object,trade_rd)

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

            extension <- ggplot(data=plot_trades, aes_string(x="Value", colour="Measure")) +
                          geom_freqpoly(bins = 50) +
                          # scale_x_continuous(limits = c(-5, 5)) +
                          facet_grid( Strategy~Long, scales='free_y') +
                          ylab("") + xlab("") + ggtitle('Stock extension')

            object <- .setOutputGGPlot(object, extension)



            return(object)
          }
)
