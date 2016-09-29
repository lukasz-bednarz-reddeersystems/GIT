#' @include market_return.r
NULL


################################################################################
#
# MarketReturnAnalysisBlock Class
#
#1. Number of extended buys and sells in month compared to number market down days
#
###############################################################################

#' Analysis Module for computation of relative
#' market return of extended trades positions
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualExtendedTradeDataHandler",
#'               "VirtualMarketDataHandler"
#' @export

setClass(
  Class             = "RelativeMarketReturnAnalysisBlock",
  slots             = c(
    market_data     = "MarketDataSX5E",
    ex_trade_data   = "TradesExtendedReturnPerMonth",
    output          = "TradesExtendedReturnPerMonth"
  ),
   prototype         = list(
     ex_trade_data  = new("TradesExtendedReturnPerMonth"),
     output         = new("TradesExtendedReturnPerMonth")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualExtendedTradeDataHandler",
                        "VirtualMarketDataHandler"
                        )
)

#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradeData"
#' class object
#'
#' @rdname setTradeDataObject-RelativeMarketReturnAnalysisBlock-method
#' @param object object of class "RelativeMarketReturnAnalysisBlock"
#' @param trade_data object of class "TradeData"
#' @return \code{object} object of class "RelativeMarketReturnAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "RelativeMarketReturnAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            object
          }
)


#' Set market_data object in object slot
#'
#' Public method to set market_data slot with "MarketDataSX5E"
#' class object
#'
#' @rdname setMarketDataObject-RelativeMarketReturnAnalysisBlock-method
#' @param object object of class "RelativeMarketReturnAnalysisBlock"
#' @param market_data object of class "MarketDataSX5E"
#' @return \code{object} object of class "RelativeMarketReturnAnalysisBlock"
#' @export
setMethod("setMarketDataObject",
          signature(object = "RelativeMarketReturnAnalysisBlock", market_data = "MarketDataSX5E"),
          function(object, market_data){
            object <- TE.RefClasses:::.setMarketDataObject(object, market_data)
            return(object)
          }
)

#' Set ex_trade_data object in object slot
#'
#' Public method to set ex_trade_data slot with "TradesExtendedReturnPerMonth"
#' class object
#'
#' @rdname setExtendedTradeDataObject-RelativeMarketReturnAnalysisBlock-method
#' @param object object of class "RelativeMarketReturnAnalysisBlock"
#' @param ex_trade_data object of class "TradesExtendedReturnPerMonth"
#' @return \code{object} object of class "RelativeMarketReturnAnalysisBlock"
#' @export
setMethod("setExtendedTradeDataObject",
          signature(object = "RelativeMarketReturnAnalysisBlock", ex_trade_data = "TradesExtendedReturnPerMonth"),
          function(object, ex_trade_data){
            object <- TE.RefClasses:::.setExtendedTradeDataObject(object, ex_trade_data)
            return(object)
          }
)

#' Trigger computation of analysis data.
#'
#' @param object object of class "RelativeMarketReturnAnalysisBlock"
#' @return \code{object} object object of class "RelativeMarketReturnAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "RelativeMarketReturnAnalysisBlock"),
          function(object){

            # retrieve needed ref_data
            trades <- getReferenceData(getTradeDataObject(object))
            extended_return <- getReferenceData(getExtendedTradeDataObject(object))
            index <- getReferenceData(getMarketDataObject(object))

            trader <- unique(trades$TraderName)
            s_str <- paste0(trader, "_S")
            l_str <- paste0(trader, "_L")

            # compute required data
            all_trd <- unique(trades[c('Gm.PsnReturn','Return','Date','Strategy','Long')])
            all_trd$Gm.PsnReturn <- all_trd$Gm.PsnReturn/10000
            n_trades <- aggregate(all_trd$Gm.PsnReturn,list(Date=all_trd$Date,Strategy=all_trd$Strategy,Long=all_trd$Long),function(x)sum(!is.na(x)))
            all_trd <- merge(n_trades,all_trd,by=c('Date','Strategy','Long'))
            all_trd$Gm.PsnReturn <- all_trd$Gm.PsnReturn/all_trd$x
            all_return <- aggregate(all_trd[c('Gm.PsnReturn','Return')],list(Date=all_trd$Date,Strategy=all_trd$Strategy,Long=all_trd$Long),function(x)sum(x,na.rm=TRUE))
            all_return$Return[is.infinite(all_return$Return)] <- NA
            all_return <- merge(all_return,index,by='Date')
            all_return_cor <- cor(all_return$Return,all_return$SX5E.Return,use="na.or.complete")
            all_return$Month <-format(all_return$Date,'%Y-%m')
            all_return <- aggregate(all_return[c('Gm.PsnReturn','Return')],list(Month=all_return$Month,Strategy=all_return$Strategy,Long=all_return$Long),function(x)mean(log(1+x),na.rm=TRUE))
            all_return <- rbind(cbind(Return='All Positions',all_return[c('Month','Strategy','Long')],Value=all_return$Gm.PsnReturn),
                                cbind(Return='All Trades',all_return[c('Month','Strategy','Long')],Value=all_return$Return))
            extended_return <- rbind(all_return,extended_return)

            focus_return_rel <- extended_return
            vframe <- merge(focus_return_rel[focus_return_rel$Return=='Position',],focus_return_rel[focus_return_rel$Return=='All Positions',],by=c('Month','Long','Strategy'))
            vframe$Value <- vframe$Value.x - vframe$Value.y
            rel_return <- cbind(Return='Position',vframe[c('Month','Long','Strategy','Value')])
            vframe <- merge(focus_return_rel[focus_return_rel$Return=='Trade',],focus_return_rel[focus_return_rel$Return=='All Trades',],by=c('Month','Long','Strategy'))
            vframe$Value <- vframe$Value.x - vframe$Value.y
            dex <- merge(focus_return_rel[focus_return_rel$Return=='Index',],focus_return_rel[focus_return_rel$Return=='All Positions',],by=c('Month','Long','Strategy'))
            dex <- cbind(dex[c('Month','Long','Strategy')],Return='Index',Value=dex$Value.x)
            rel_return <- rbind(rel_return,cbind(Return='Trade',vframe[c('Month','Long','Strategy','Value')]),dex)

            relative_rtns <- ggplot(data=rel_return, aes_string(x="Month", fill="Return")) +
              geom_bar(aes_string(weight="Value"),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily relative log return due to extended trades')

            object <- .setOutputGGPlotData(object,rel_return)
            object <- .setOutputGGPlot(object, relative_rtns)

            output_obj <- new("TradesExtendedReturnPerMonth")

            output_obj <- setReferenceData(output_obj, extended_return)
            object <- .setOutputObject(object, output_obj)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Return", "Value")))


            return(object)
          }
)
