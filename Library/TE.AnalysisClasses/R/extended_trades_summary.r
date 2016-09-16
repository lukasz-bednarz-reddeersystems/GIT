#' @include extended_trades.r
NULL


################################################################################
#
# MarketReturnAnalysisBlock Class
#
#1. Number of extended buys and sells in month compared to number market down days
#
###############################################################################


#' Analysis Module for computation extended trades summary
#'
#' Computation block class compute summary focus data for
#' extended trades.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualExtendedTradeDataHandler"
#' @export
setClass(
  Class             = "ExtendedTradesSummaryAnalysisBlock",
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualExtendedTradeDataHandler"
                        )
)


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradeData"
#' class object
#'
#' @rdname setTradeDataObject-ExtendedTradesSummaryAnalysisBlock-method
#' @param object object of class "ExtendedTradesSummaryAnalysisBlock"
#' @param trade_data object of class "TradeData"
#' @return \code{object} object of class "ExtendedTradesSummaryAnalysisBlock"
#' @export
setMethod("setTradeDataObject",
          signature(object = "ExtendedTradesSummaryAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            return(object)
          }
)


#' Set ex_trade_data object in object slot
#'
#' Public method to set ex_trade_data slot with "ExtendedTradeData"
#' class object
#'
#' @rdname setExtendedTradeDataObject-ExtendedTradesSummaryAnalysisBlock-method
#' @param object object of class "ExtendedTradesSummaryAnalysisBlock"
#' @param ex_trade_data object of class "ExtendedTradeData"
#' @return \code{object} object of class "ExtendedTradesSummaryAnalysisBlock"
#' @export
setMethod("setExtendedTradeDataObject",
          signature(object = "ExtendedTradesSummaryAnalysisBlock", ex_trade_data = "ExtendedTradeData"),
          function(object, ex_trade_data){
           object <-  TE.RefClasses:::.setExtendedTradeDataObject(object, ex_trade_data)
           return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "ExtendedTradesSummaryAnalysisBlock"
#' @return \code{object} object object of class "ExtendedTradesSummaryAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "ExtendedTradesSummaryAnalysisBlock"),
          function(object){

            # retrieve needed ref_data
            extended_trades <- getReferenceData(getExtendedTradeDataObject(object))
            history_data <- getReferenceData(getTradeDataObject(object))


            extended_InstrumentIDs <- extended_trades$InstrumentID
            extended_positions <- merge(data.frame(InstrumentID=extended_InstrumentIDs),history_data,by=c('InstrumentID'))
            smmry_cols <- c('SkewInto','SkewOutof','DeltaSwing','DeltaSkew','DeltaPL')
            mn_data <- unique(extended_positions[c('Strategy','InstrumentID','Date',smmry_cols)])
            mn_smmry<- aggregate(extended_positions[smmry_cols],list(Strategy=extended_positions$Strategy),function(x)mean(x,na.rm=TRUE))
            pl_data <- unique(extended_positions[c('Strategy','InstrumentID','Date','TodayPL')])
            pl_data$Month <- format(pl_data$Date,'%Y-%m')
            pl_mnth_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy,Month=pl_data$Month),function(x)sum(x,na.rm=TRUE))
            colnames(pl_mnth_smmry) <- c('Strategy','Month','PL')
            pl_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy),function(x)sum(x,na.rm=TRUE))
            all_data<- unique(history_data[c('Strategy','InstrumentID','Date','TodayPL')])
            all_data$Month <- format(all_data$Date,'%Y-%m')
            all_mnth_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy,Month=all_data$Month),function(x)sum(x,na.rm=TRUE))
            colnames(all_mnth_pl) <- c('Strategy','Month','PL')
            all_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy),function(x)sum(x,na.rm=TRUE))

            mnthly_pl <- rbind(cbind(Key='Total',all_mnth_pl),
                               cbind(Key='Extended',pl_mnth_smmry))
            mpl <- ggplot(data=mnthly_pl, aes_string(x="Month", fill="Key")) +
              geom_bar(aes_string(weight="PL"),position="dodge") +
              facet_grid(Strategy~.) +
              ylab("PL") + xlab("Month") + ggtitle('PL in positions featuring extended trades')


            object <- .setOutputGGPlotData(object,mnthly_pl)
            object <- .setOutputGGPlot(object, mpl)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Key", "PL")))


            return(object)
          }
)
