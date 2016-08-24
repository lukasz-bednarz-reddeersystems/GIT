#' @include report_analysis_block.r
NULL

################################################################################
#
# ExtendedTradesReport Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################

#' List of Analysis modules used by ExtendedTradesReport Class
extended_trades_report_analysis_blocks <- c("ExtendedTrades",
                                            "BuysAndSells",
                                            "MarketReturn",
                                            "RelativeMarketReturn",
                                            "ExtendedTradesSummary")
#' Extended Trades Report Data class.
#'
#' Report class computing following blocks:
#'  "ExtendedTrades",
#'  "BuysAndSells",
#'  "MarketReturn",
#'  "RelativeMarketReturn",
#'  "ExtendedTradesSummary"
#'
#' Inherits from "VirtualReportAnalysisBlock"
#'
#' @export
setClass(
  Class             = "ExtendedTradesReport",
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    ggplot_list        = list(),
    ggplot_data_list   = list(),
    frontend_data_list = list(),
    output_list        = list()
  ),
  contains          = c("VirtualReportAnalysisBlock"
                        )
)


#' Request data from data source
#'
#' @param object object of class 'ExtendedTradesReport'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'ExtendedTradesReport'.
#' @export
setMethod("dataRequest",
          signature(object = "ExtendedTradesReport", key_values = "data.frame"),
          function(object, key_values){

            object <- setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)


#' Trigger computation of report data.
#'
#' @param object object of class "ExtendedTradesReport"
#' @return \code{object} object object of class "ExtendedTradesReport"
#' @export
setMethod("Process",
          signature(object = "ExtendedTradesReport"),
          function(object){


            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)

            ######################################################
            #
            # Index Data
            #
            ######################################################
            index.rd     <- new("MarketDataSX5E",
                                min(key_values$start),
                                max(key_values$end))


            ######################################################
            #
            # ExtendedTradesAnalysisBlock
            #
            ######################################################

            # create/get data/process extended stock analyzer
            ext.stock.an <- new("ExtendedTradesAnalysisBlock")
            ext.stock.an <- dataRequest(ext.stock.an, key_values)
            ext.stock.an <- Process(ext.stock.an)

            # retrieve raw dataRequest reference data trades + position data(no events or dealer data)
            trades.rd     <- getTradeDataObject(ext.stock.an)

            # retrieve processed result (extended trades)
            ext.stock.rd <- getOutputObject(ext.stock.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, ext.stock.an)

            ######################################################
            #
            # "BuysAndSellsAnalysisBlock"
            #
            ######################################################
            # create buys sells analyzer
            buys.sells.an <- new("BuysAndSellsAnalysisBlock")

            # populate input data from previous computation
            buys.sells.an <- setTradeDataObject(buys.sells.an, ext.stock.rd)
            buys.sells.an <- setMarketDataObject(buys.sells.an, index.rd)

            buys.sells.an <- Process(buys.sells.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, buys.sells.an)

            ######################################################
            #
            # "MarketReturnAnalysisBlock"
            #
            ######################################################
            # create buys sells analyzer
            market.ret.an <- new("MarketReturnAnalysisBlock")

            # set needed ref_data from previous analysis block
            market.ret.an <- setTradeDataObject(market.ret.an, ext.stock.rd)
            market.ret.an <- setMarketDataObject(market.ret.an, index.rd)

            # process
            market.ret.an <- Process(market.ret.an)

            # retrieve processed data for next block
            ext.ret.rd    <- getOutputObject(market.ret.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, market.ret.an)

            ######################################################
            #
            # "RelativeMarketReturnAnalysisBlock"
            #
            ######################################################
            # create analyzer
            rel.mkt.ret.an <- new("RelativeMarketReturnAnalysisBlock")

            # set needed ref_data from previous analysis block
            rel.mkt.ret.an <- setTradeDataObject(rel.mkt.ret.an, trades.rd)
            rel.mkt.ret.an <- setExtendedTradeDataObject(rel.mkt.ret.an, ext.ret.rd)
            rel.mkt.ret.an <- setMarketDataObject(rel.mkt.ret.an, index.rd)

            # process
            rel.mkt.ret.an <- Process(rel.mkt.ret.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, rel.mkt.ret.an)


            ######################################################
            #
            # "ExtendedTradesSummaryAnalysisBlock"
            #
            ######################################################
            # create analyzer
            ext.trd.sum.an <- new("ExtendedTradesSummaryAnalysisBlock")

            # set needed ref_data from previous analysis block
            ext.trd.sum.an <- setTradeDataObject(ext.trd.sum.an, trades.rd)
            ext.trd.sum.an <- setExtendedTradeDataObject(ext.trd.sum.an, ext.stock.rd)

            # process
            ext.trd.sum.an <- Process(ext.trd.sum.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, ext.trd.sum.an)


            return(object)
          }
)
