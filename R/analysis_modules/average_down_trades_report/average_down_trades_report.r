sourceTo("../analysis_modules/report_analysis_block/report_analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions/offside_positions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_gain_vs_days/offside_positions_gain_vs_days.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_pnl_vs_days/offside_positions_pnl_vs_days.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_cumulative_pnl/offside_positions_cumulative_pnl.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_relative_bps_per_month/offside_positions_relative_bps_per_month.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/average_down_trades/average_down_trades.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/average_down_trades_focus/average_down_trades_focus.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(gridExtra)

################################################################################
#
# AverageDownTradesReport Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################

average_down_trades_report_analysis_blocks <- c("OffsidePositions",
                                                "OffsidePositionsGainVsDays",
                                                "OffsidePositionsPnLVsDays",
                                                "OffsidePositionsCumulativePnL",
                                                "OffsidePositionsBpsPerMonth",
                                                "AverageDownTrades",
                                                "AverageDownTradesFocus")

setClass(
  Class             = "AverageDownTradesReport",
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


setMethod("dataRequest",
          signature(object = "AverageDownTradesReport", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)



setMethod("Process",  
          signature(object = "AverageDownTradesReport"),
          function(object, key_values){
            
            
            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)

            ######################################################
            #
            # OffsidePositionsAnalysisBlock
            #
            ######################################################
            
            # create/get data/process offside positions analyzer
            offside.pos.an <- new("OffsidePositionsAnalysisBlock")
            offside.pos.an <- dataRequest(offside.pos.an, key_values)
            offside.pos.an <- Process(offside.pos.an)
            
            offside.pos.rd <- getOutputObject(offside.pos.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.pos.an)
            
            ######################################################
            #
            # "OffsidePositionsGainVsDaysAnalysisBlock"
            #
            ######################################################
            # create buys sells analyzer
            offside.gain.an <- new("OffsidePositionsGainVsDaysAnalysisBlock")
            
            offside.gain.an <- setPositionDataObject(offside.gain.an, offside.pos.rd)
            
            offside.gain.an <- Process(offside.gain.an)
            offside.gain.rd <- getOutputObject(offside.gain.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.gain.an)
            
            
            
            ######################################################
            #
            # "OffsidePositionsPnLVsDaysAnalysisBlock"
            #
            ######################################################
            # create buys sells analyzer
            offside.pnl.an <- new("OffsidePositionsPnLVsDaysAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            offside.pnl.an <- setPositionDataObject(offside.pnl.an, offside.gain.rd)
            
            # process
            offside.pnl.an <- Process(offside.pnl.an)
            
            # retrieve processed data for next block
            offside.pnl.rd    <- getOutputObject(offside.pnl.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.pnl.an)
            
            ######################################################
            #
            # "OffsidePositionsCumulativePnLAnalysisBlock"
            #
            ######################################################
            # create analyzer
            offside.cum.an <- new("OffsidePositionsCumulativePnLAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            offside.cum.an <- setPositionDataObject(offside.cum.an, offside.gain.rd)
            
            # process
            offside.cum.an <- Process(offside.cum.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.cum.an)
            
            ######################################################
            #
            # "OffsidePositionsBpsPerMonthAnalysisBlock"
            #
            ######################################################
            # create analyzer
            offside.bps.an <- new("OffsidePositionsBpsPerMonthAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            offside.bps.an <- setPositionDataObject(offside.bps.an, offside.pos.rd)
            
            # process
            offside.bps.an <- Process(offside.bps.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.bps.an)
            
            ######################################################
            #
            # "AverageDownTradesAnalysisBlock"
            #
            ######################################################
            # create analyzer
            avg.down.trd.an <- new("AverageDownTradesAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            avg.down.trd.an <- setPositionDataObject(avg.down.trd.an, offside.pos.rd)
            
            # get additional trade data
            avg.down.trd.an <- dataRequest(avg.down.trd.an, key_values)
            
            # process
            avg.down.trd.an <- Process(avg.down.trd.an)
            
            # retreive data for later block
            avg.down.trd.rd <- getOutputObject(avg.down.trd.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, avg.down.trd.an)
            
            ######################################################
            #
            # "AverageDownTradesFocusAnalysisBlock"
            #
            ######################################################
            # create analyzer
            avg.down.fcs.an <- new("AverageDownTradesFocusAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            avg.down.fcs.an <- setTradeDataObject(avg.down.fcs.an, avg.down.trd.rd)
            
            # process
            avg.down.fcs.an <- Process(avg.down.fcs.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, avg.down.fcs.an)
            
            ######################################################
            #
            # "Summary Grid Plot"
            #
            ######################################################
            ggplot_list <- getOutputGGPlotList(object)
            
            plt_list <- lapply(ggplot_list[c("OffsidePositions",
                                                    "OffsidePositionsPnLVsDays",
                                                    "AverageDownTrades",
                                                    "OffsidePositionsCumulativePnL")], ggplotGrob)
            
            grid_grob <- arrangeGrob(grobs = plt_list)
            
            ggplot_list[["Summary"]] <- grid_grob
            
            # set data
            object <- .setOutputGGPlotList(object, ggplot_list)

            
            
            return(object)
          }
)
