sourceTo("../analysis_modules/report_analysis_block/report_analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/positions_holding_period/positions_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/positions_holding_day_zero_pnl/positions_holding_day_zero_pnl.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/positions_holding_capital_distribution/positions_holding_capital_distribution.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# PositionsHoldingPeriodReport Class
# 
# Computation block class to pull data required for Computation of positions holding period
# Pulls data required for computation and adds required columns.
###############################################################################

positions_holding_period_report_analysis_blocks <- c("PositionsHoldingPeriod",
                                                     "PositionsHoldingDayZeroPnL", 
                                                     "PositionsHoldingCapitalDistribution")

setClass(
  Class             = "PositionsHoldingPeriodReport",
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
          signature(object = "PositionsHoldingPeriodReport", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)



setMethod("Process",  
          signature(object = "PositionsHoldingPeriodReport"),
          function(object, key_values){
            
            
            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)
            
            ######################################################
            #
            # "PositionsHoldingDayZeroPnLAnalysisBlock"
            #
            ######################################################
            # create analyzer
            pos.hold.d0.an <- new("PositionsHoldingDayZeroPnLAnalysisBlock")
            
            # get data
            pos.hold.d0.an <- dataRequest(pos.hold.d0.an, key_values)
            
            # process
            pos.hold.d0.an <- Process(pos.hold.d0.an)
            
            # retreive data for later block
            offside.pos.rd <- getPositionDataObject(pos.hold.d0.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, pos.hold.d0.an)
            
            ######################################################
            #
            # "PositionsHoldingPeriodAnalysisBlock"
            #
            ######################################################
            # create analyzer
            pos.hold.per.an <- new("PositionsHoldingPeriodAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            pos.hold.per.an <- setPositionDataObject(pos.hold.per.an, offside.pos.rd)
            
            # process
            pos.hold.per.an <- Process(pos.hold.per.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, pos.hold.per.an)
            
            ######################################################
            #
            # "PositionsHoldingCapitalDistributionAnalysisBlock"
            #
            ######################################################
            # create analyzer
            pos.hold.per.an <- new("PositionsHoldingCapitalDistributionAnalysisBlock")
            
            # set needed ref_data from previous analysis block
            pos.hold.per.an <- setPositionDataObject(pos.hold.per.an, offside.pos.rd)
            
            # process
            pos.hold.per.an <- Process(pos.hold.per.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, pos.hold.per.an)
           

            return(object)
          }
)
