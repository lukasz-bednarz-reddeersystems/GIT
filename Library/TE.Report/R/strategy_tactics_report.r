#' @include report_analysis_block.r
NULL

################################################################################
#
# StrategyTacticsReport Class
#
# Computation block class to pull data required for Computation of strategy
# tactics report
# Pulls data required for computation and adds required columns.
###############################################################################

#' List of Analysis modules used by StrategyTacticsReport Class
strategy_tactics_report_analysis_blocks <- c("StrategyBreakdown",
                                             "StrategyBreakdownAUMAndTurnover",
                                             "StrategyBreakdownTotalAndDeltaPnL",
                                             "StrategyBreakdownValueTradedPerSignal",
                                             "StrategyBreakdownPnLOnTradeDayPerSignal",
                                             "StrategyBreakdownPnLAndTurnoverPerEvent",
                                             "StrategyBreakdownSignalCharacteristicAndEffectiveness",
                                             "PortfolioFactorExposure",
                                             "ValueTradedInLongShortHedge",
                                             "PnLTradedInLongShortHedge")


#' Strategy Tactics Report class.
#'
#' Report class computing following blocks:
#'  "StrategyBreakdown",
#'  "StrategyBreakdownAUMAndTurnover",
#'  "StrategyBreakdownTotalAndDeltaPnL",
#'  "StrategyBreakdownValueTradedPerSignal",
#'  "StrategyBreakdownPnLOnTradeDayPerSignal",
#'  "StrategyBreakdownPnLAndTurnoverPerEvent",
#'  "StrategyBreakdownSignalCharacteristicAndEffectiveness",
#'  "PortfolioFactorExposure",
#'  "ValueTradedInLongShortHedge",
#'  "PnLTradedInLongShortHedge"
#'
#' Also generates summary plot with following subplots:
#'  "StrategyBreakdownAUMAndTurnover",
#'  "StrategyBreakdownTotalAndDeltaPnL"
#'  "StrategyBreakdownPnLAndTurnoverPerEvent",
#'  "StrategyBreakdownSignalCharacteristicAndEffectiveness",
#'  "PortfolioFactorExposure"
#'
#' Inherits from "VirtualReportAnalysisBlock"
#'
#' @export

setClass(
  Class             = "StrategyTacticsReport",
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
#' @param object object of class 'StrategyTacticsReport'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'StrategyTacticsReport'.
#' @export
setMethod("dataRequest",
          signature(object = "StrategyTacticsReport", key_values = "data.frame"),
          function(object, key_values){

            object <- setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)


#' Trigger computation of report data.
#'
#' @param object object of class "StrategyTacticsReport"
#' @return \code{object} object object of class "StrategyTacticsReport"
#' @export
setMethod("Process",
          signature(object = "StrategyTacticsReport"),
          function(object){


            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)

            ######################################################
            #
            # StrategyBreakdownAnalysisBlock
            #
            ######################################################

            # create/get data/process offside positions analyzer
            strat.brdwn.an <- new("StrategyBreakdownAnalysisBlock")
            strat.brdwn.an <- dataRequest(strat.brdwn.an, key_values)
            strat.brdwn.an <- Process(strat.brdwn.an)

            strat.brdwn.rd <- getOutputObject(strat.brdwn.an)
            trade.data.rd <- getTradeDataObject(strat.brdwn.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, strat.brdwn.an)

            ######################################################
            #
            # "StrategyBreakdownAUMAndTurnoverAnalysisBlock"
            #
            ######################################################
            # create analyzer
            sb.aum.trn.an <- new("StrategyBreakdownAUMAndTurnoverAnalysisBlock")

            sb.aum.trn.an <- setStrategyDataObject(sb.aum.trn.an, strat.brdwn.rd)
            sb.aum.trn.an <- Process(sb.aum.trn.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.aum.trn.an)

            ######################################################
            #
            # "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
            #
            ######################################################
            # create buys sells analyzer
            sb.pnl.dlt.an <- new("StrategyBreakdownTotalAndDeltaPnLAnalysisBlock")

            # set needed ref_data from previous analysis block
            sb.pnl.dlt.an <- setStrategyDataObject(sb.pnl.dlt.an, strat.brdwn.rd)

            # process
            sb.pnl.dlt.an <- Process(sb.pnl.dlt.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.pnl.dlt.an)

            ######################################################
            #
            # "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
            #
            ######################################################
            # create analyzer
            sb.vt.ps.an <- new("StrategyBreakdownValueTradedPerSignalAnalysisBlock")

            # set needed ref_data from previous analysis block
            sb.vt.ps.an <- setTradeDataObject(sb.vt.ps.an, trade.data.rd)

            # get missing data
            sb.vt.ps.an <- dataRequest(sb.vt.ps.an, key_values)

            # process
            sb.vt.ps.an <- Process(sb.vt.ps.an)
            sb.vt.ps.rd <- getOutputObject(sb.vt.ps.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.vt.ps.an)

            ######################################################
            #
            # "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"
            #
            ######################################################
            # create analyzer
            sb.pnl.ps.an <- new("StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock")

            # set needed ref_data from previous analysis block
            sb.pnl.ps.an <- setTradeDataObject(sb.pnl.ps.an, sb.vt.ps.rd)

            # process
            sb.pnl.ps.an <- Process(sb.pnl.ps.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.pnl.ps.an)

            ######################################################
            #
            # "StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock"
            #
            ######################################################
            # create analyzer
            sb.pnl.pe.an <- new("StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock")

            # set needed ref_data from previous analysis block
            sb.pnl.pe.an <- setTradeDataObject(sb.pnl.pe.an, sb.vt.ps.rd)

            # process
            sb.pnl.pe.an <- Process(sb.pnl.pe.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.pnl.pe.an)

            ######################################################
            #
            # "StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock"
            #
            ######################################################
            # create analyzer
            sb.sig.char.an <- new("StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock")

            # set needed ref_data from previous analysis block
            sb.sig.char.an <- setTradeDataObject(sb.sig.char.an, sb.vt.ps.rd)

            # process
            sb.sig.char.an <- Process(sb.sig.char.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, sb.sig.char.an)


            ######################################################
            #
            # "PortfolioFactorExposureAnalysisBlock"
            #
            ######################################################
            # create analyzer
            prtf.fe.an <- new("PortfolioFactorExposureAnalysisBlock")

            # request Data
            prtf.fe.an <- dataRequest(prtf.fe.an, key_values)

            # process
            prtf.fe.an <- Process(prtf.fe.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, prtf.fe.an)

            ######################################################
            #
            # "ValueTradedInLongShortHedgeAnalysisBlock"
            #
            ######################################################
            # create analyzer
            val.traded.an <- new("ValueTradedInLongShortHedgeAnalysisBlock")

            # set needed ref_data from previous analysis block
            val.traded.an <- setTradeDataObject(val.traded.an, trade.data.rd)

            # process
            val.traded.an <- Process(val.traded.an)
            val.traded.rd <- getOutputObject(val.traded.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, val.traded.an)

            ######################################################
            #
            # "PnLTradedInLongShortHedgeAnalysisBlock"
            #
            ######################################################
            # create analyzer
            pnl.traded.an <- new("PnLTradedInLongShortHedgeAnalysisBlock")

            # set needed ref_data from previous analysis block
            pnl.traded.an <- setTradeDataObject(pnl.traded.an, val.traded.rd)

            # process
            pnl.traded.an <- Process(pnl.traded.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, pnl.traded.an)

            ######################################################
            #
            # "Summary Grid Plot"
            #
            ######################################################


            ggplot_list <- getOutputGGPlotList(object)

            inset_plt_list <- lapply(ggplot_list[c("StrategyBreakdownAUMAndTurnover",
                                                    "StrategyBreakdownTotalAndDeltaPnL")], ggplotGrob)
            plt_list <- list()
            plt_list[[1]] <- arrangeGrob(grobs = inset_plt_list, nrow = 1)
            plt_list[2:4] <- lapply(ggplot_list[c("StrategyBreakdownPnLAndTurnoverPerEvent",
                                                    "StrategyBreakdownSignalCharacteristicAndEffectiveness",
                                                    "PortfolioFactorExposure")], ggplotGrob)

            grid_grob <- arrangeGrob(grobs = plt_list)

            ggplot_list[["Summary"]] <- grid_grob

            # set data
            object <- .setOutputGGPlotList(object, ggplot_list)



            return(object)
          }
)
