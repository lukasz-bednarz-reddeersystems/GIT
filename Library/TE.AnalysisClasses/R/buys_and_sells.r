#' @include analysis_block.r
#' @include extended_trades.r
NULL


################################################################################
#
# BuysAndSellsAnalysisBlock Class
#
# Number of extended buys and sells in month compared to number market down days
#
###############################################################################


#' Number of extended buys and sells in month compared to number market down days
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualMarketDataHandler"
#' @export


setClass(
  Class             = "BuysAndSellsAnalysisBlock",
  slots             = c(
    market_data = "MarketDataSX5E",
    trade_data  = "ExtendedTradeData"
  ),
  prototype         = list(
    trade_data  = new("ExtendedTradeData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualMarketDataHandler"
                        )
)

#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "ExtendedTradeData"
#' class object
#'
#' @rdname setTradeDataObject-BuysAndSellsAnalysisBlock-method
#' @param object object of class "BuysAndSellsAnalysisBlock"
#' @param trade_data object of class "ExtendedTradeData"
#' @return \code{object} object of class "BuysAndSellsAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "BuysAndSellsAnalysisBlock", trade_data = "ExtendedTradeData"),
          function(object, trade_data){
            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            return(object)
          }
)


#' Request data from data source
#'
#' @param object object of class 'BuysAndSellsAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'BuysAndSellsAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "BuysAndSellsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            #
            trade_data <- getTradeDataObject(object)

            if (getStoredNRows(trade_data) == 0) {

              ext.stock.an <- new("ExtendedTradesAnalysisBlock")

              ext.stock.an <- dataRequest(ext.stock.an, key_values)
              ext.stock.an <- Process(ext.stock.an)

              ext.stock.rd <- getOutputObject(ext.stock.an)

              object <- TE.RefClasses:::.setTradeDataObject(object, ext.stock.rd)
            }

            market_data <- getMarketDataObject(object)

            if (is.null(market_data) || getStoredNRows(market_data) == 0) {
              index.rd     <- new("MarketDataSX5E",
                                  min(key_values$start),
                                  max(key_values$end))

              object <- TE.RefClasses:::.setMarketDataObject(object, index.rd)
            }

            return(object)
          }
)

#' Set market_data object in object slot
#'
#' Public method to set market_data slot with "MarketData"
#' class object
#'
#' @rdname setMarketDataObject-BuysAndSellsAnalysisBlock-method
#' @param object object of class "BuysAndSellsAnalysisBlock"
#' @param market_data object of class "MarketData"
#' @return \code{object} object of class "BuysAndSellsAnalysisBlock"
#' @export

setMethod("setMarketDataObject",
          signature(object = "BuysAndSellsAnalysisBlock", market_data = "MarketData"),
          function(object, market_data){
            object <- TE.RefClasses:::.setMarketDataObject(object, market_data)
            return(object)
          }
)

#' Trigger computation of analysis data.
#'
#' @param object object of class "BuysAndSellsAnalysisBlock"
#' @return \code{object} object object of class "BuysAndSellsAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "BuysAndSellsAnalysisBlock"),
          function(object){

            # retrieve needed ref_data
            trades <- getReferenceData(getTradeDataObject(object))
            index <- getReferenceData(getMarketDataObject(object))

            trader <- unique(trades$TraderName)
            s_str <- paste0(trader, "_S")
            l_str <- paste0(trader, "_L")

            # compute required data
            buy_sells <- trades
            buy_sells$Month <- format(buy_sells$Date,'%Y-%m')
            buy_sells <- aggregate(buy_sells['InstrumentID'],list(Month=buy_sells$Month,Strategy=buy_sells$Strategy,Long=buy_sells$Long),function(x)length(x))
            up_days <- merge(index,trades,by='Date')
            up_days$Up_Buy <- up_days$SX5E.Return>0&(up_days$Long==1)
            up_days$Up_Sell<- up_days$SX5E.Return>0&(up_days$Long==0)
            up_days$Month <-format(up_days$Date,'%Y-%m')
            up_days <- aggregate(up_days[c('Up_Buy','Up_Sell')],list(Month=up_days$Month,Strategy=up_days$Strategy),sum)
            colnames(buy_sells) <- c('Month','Strategy','Long','Count')
            buy_sells$Side <- 'Sell'
            buy_sells[buy_sells$Long==1,]$Side <- 'Buy'
            buy_sells <- rbind(buy_sells[c('Month','Strategy','Count','Side')],
                               cbind(Side='Up_Buy',up_days[c('Month','Strategy')],Count=up_days$Up_Buy),
                               cbind(Side='Up_Sell',up_days[c('Month','Strategy')],Count=up_days$Up_Sell))


            ttls_long <- aggregate(buy_sells[grep(l_str,buy_sells$Strategy),]$Count,list(Month=buy_sells[grep(l_str,buy_sells$Strategy),]$Month,Side=buy_sells[grep(l_str,buy_sells$Strategy),]$Side),sum)
            ttls_long$Strategy <- 'Overall Long'
            colnames(ttls_long) <- c('Month','Side','Count','Strategy')
            ttls_short <- aggregate(buy_sells[grep(s_str,buy_sells$Strategy),]$Count,list(Month=buy_sells[grep(s_str,buy_sells$Strategy),]$Month,Side=buy_sells[grep(s_str,buy_sells$Strategy),]$Side),sum)
            ttls_short$Strategy <- 'Overall Short'
            colnames(ttls_short) <- c('Month','Side','Count','Strategy')


            buy_sells <- rbind(buy_sells,ttls_long)
            buy_sells <- rbind(buy_sells,ttls_short)

             n_buys_sells <- ggplot(data=buy_sells, aes_string(x="Month", fill="Side")) +
              geom_bar(aes_string(weight="Count"),position="dodge") +
              facet_grid(Strategy~.) +
              ylab("") + xlab("") + ggtitle('Stock extension')

            object <- .setOutputGGPlotData(object,buy_sells)
            object <- .setOutputGGPlot(object, n_buys_sells)


            return(object)
          }
)
