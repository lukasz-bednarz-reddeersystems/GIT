#' @include extended_trades.r
NULL


################################################################################
#
# MarketReturnAnalysisBlock Class
#
# Market return of Extended trades positions
#
###############################################################################


#' TradesExtendedReturnPerMonth Reference Data class.
#'
#' Concrete S4 class storing data of Extended Trades Return per Month
#' Generated only by MarketReturnAnalysisBlock module.
#'
#' Inherits from "VirtualTradeData"
#'
#' @export

setClass(
  Class             = "TradesExtendedReturnPerMonth",
  prototype         = list(
    required_colnms = c("Return", "Month", "Strategy", "Long", "Value")
  ),
  contains          = c("VirtualTradeData")
)



#' Analysis Module for computation of
#' market return of extended trades positions
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualMarketDataHandler"
#' @export

setClass(
  Class             = "MarketReturnAnalysisBlock",
  slots             = c(
    market_data = "MarketDataSX5E",
    output      = "TradesExtendedReturnPerMonth"
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualMarketDataHandler"
                        ),
  prototype         = list(
    output      = new("TradesExtendedReturnPerMonth")
  )
)

#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "ExtendedTradeData"
#' class object
#'
#' @rdname setTradeDataObject-MarketReturnAnalysisBlock-method
#' @param object object of class "MarketReturnAnalysisBlock"
#' @param trade_data object of class "ExtendedTradeData"
#' @return \code{object} object of class "MarketReturnAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "MarketReturnAnalysisBlock", trade_data = "ExtendedTradeData"),
          function(object, trade_data){
            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            return(object)
          }
)


#' Set market_data object in object slot
#'
#' Public method to set market_data slot with "MarketDataSX5E"
#' class object
#'
#' @rdname setMarketDataObject-MarketReturnAnalysisBlock-method
#' @param object object of class "MarketReturnAnalysisBlock"
#' @param market_data object of class "MarketDataSX5E"
#' @return \code{object} object of class "MarketReturnAnalysisBlock"
#' @export
setMethod("setMarketDataObject",
          signature(object = "MarketReturnAnalysisBlock", market_data = "MarketDataSX5E"),
          function(object, market_data){
            object <- TE.RefClasses:::.setMarketDataObject(object, market_data)
            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "MarketReturnAnalysisBlock"
#' @return \code{object} object object of class "MarketReturnAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "MarketReturnAnalysisBlock"),
          function(object){

            # retrieve needed ref_data
            trades <- getReferenceData(getTradeDataObject(object))
            index <- getReferenceData(getMarketDataObject(object))

            trader <- unique(trades$TraderName)
            s_str <- paste0(trader, "_S")
            l_str <- paste0(trader, "_L")

            # compute required data
            extended_return <- trades
            extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/10000
            n_trades <- aggregate(extended_return$Gm.PsnReturn,list(Date=extended_return$Date,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(!is.na(x)))
            extended_return <- merge(n_trades,extended_return,by=c('Date','Strategy','Long'))
            extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/extended_return$x
            extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return')],list(Date=extended_return$Date,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(x,na.rm=TRUE))
            extended_return <- merge(extended_return,index,by='Date')
            extended_return$Return[is.infinite(extended_return$Return)] <- NA
            extended_return_cor <- cor(extended_return$Return,extended_return$SX5E.Return,use="na.or.complete")
            extended_return$Month <-format(extended_return$Date,'%Y-%m')
            extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return','SX5E.Return')],list(Month=extended_return$Month,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)mean(log(1+x),na.rm=TRUE))
            extended_return <- rbind(cbind(Return='Position',extended_return[c('Month','Strategy','Long')],Value=extended_return$Gm.PsnReturn),
                                     cbind(Return='Trade',extended_return[c('Month','Strategy','Long')],Value=extended_return$Return),
                                     cbind(Return='Index',extended_return[c('Month','Strategy','Long')],Value=extended_return$SX5E.Return))


            ttl_rtn_long <- aggregate(extended_return[grep(l_str,extended_return$Strategy),]$Value,list(Month=extended_return[grep(l_str,extended_return$Strategy),]$Month,Side=extended_return[grep(l_str,extended_return$Strategy),]$Long,Return=extended_return[grep(l_str,extended_return$Strategy),]$Return),mean)
            ttl_rtn_long$Strategy <- 'Overall Long'
            colnames(ttl_rtn_long) <- c('Month','Long','Return','Value','Strategy')
            ttl_rtn_short <- aggregate(extended_return[grep(s_str,extended_return$Strategy),]$Value,list(Month=extended_return[grep(s_str,extended_return$Strategy),]$Month,Side=extended_return[grep(s_str,extended_return$Strategy),]$Long,Return=extended_return[grep(s_str,extended_return$Strategy),]$Return),mean)
            ttl_rtn_short$Strategy <- 'Overall Short'
            colnames(ttl_rtn_short) <- c('Month','Long','Return','Value','Strategy')

            extended_return <- rbind(extended_return,ttl_rtn_long)
            extended_return <- rbind(extended_return,ttl_rtn_short)
            extended_return$Value[is.infinite(extended_return$Value)] <- 0
            extended_return$Value[is.nan(extended_return$Value)] <- 0

            # generate plot data
            extension_rtns <- ggplot(data=extended_return, aes_string(x="Month", fill="Return")) +
              geom_bar(aes_string(weight="Value"),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily log return due to extended trades')


            object <- .setOutputGGPlotData(object,extended_return)
            object <- .setOutputGGPlot(object, extension_rtns)

            output_obj <- getOutputObject(object)

            output_obj <- setReferenceData(output_obj, extended_return)
            object <- .setOutputObject(object, output_obj)


            return(object)
          }
)
