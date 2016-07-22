sourceTo("../analysis_modules/report_analysis_block/report_analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_variance_decomposition/portfolio_variance_decomposition.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_factor_returns/portfolio_factor_returns.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_factor_exposures/portfolio_factor_exposures.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(gridExtra)

################################################################################
#
# RiskReviewReport Class
# 
# Computation block class to generate plots required for quarterly report 2016.2
# Pulls data required for computation and outputs plots to plot list
###############################################################################

risk_review_analysis_blocks <- c(
                                             "PortfolioVarianceDecomposition",
                                             "PortfolioFactorReturns",
                                             "PortfolioFactorExposures"
)

setClass(
  Class             = "RiskReviewReport",
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
          signature(object = "RiskReviewReport", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)



setMethod("Process",  
          signature(object = "RiskReviewReport"),
          function(object){
            browser()
            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)
            
            ######################################################
            #
            # PortfolioVarianceDecompositionAnalysisBlock xx
            #
            ######################################################
            
            # create/get data/process offside positions analyzer
            portf.var.an <- new("PortfolioVarianceDecompositionAnalysisBlock")
            
            # gets required data
            portf.var.an <- dataRequest(portf.var.an, key_values)
            
            #process
            portf.var.an <- Process(portf.var.an)
            
            portf_data <- getPortfolioDataObject(portf.var.an)
            betas_data <- getInstrumentBetasDataObject(portf.var.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, portf.var.an)
            
            ######################################################
            #
            # "PortfolioFactorReturnsAnalysisBlock" xx
            #
            ######################################################
            # create analyzer
            portf.ret.an <- new("PortfolioFactorReturnsAnalysisBlock")
            
            # set data
            portf.ret.an <- setPortfolioDataObject(portf.ret.an, portf_data)
            portf.ret.an <- setInstrumentBetasDataObject(portf.ret.an, betas_data)
            
            # gets position and price data
            portf.ret.an <- dataRequest(portf.ret.an, key_values)
            
            # process
            portf.ret.an <- Process(portf.ret.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, portf.ret.an)
            
            
            ######################################################
            #
            # "PortfolioFactorExposuresAnalysisBlock" xx
            #
            ######################################################
            # create analyzer
            portf.betas.an <- new("PortfolioFactorExposuresAnalysisBlock")
            
            # set data
            portf.betas.an <- setPortfolioDataObject(portf.betas.an, portf_data)
            portf.betas.an <- setInstrumentBetasDataObject(portf.betas.an, betas_data)
            
            # process
            portf.betas.an <- Process(portf.betas.an)
            
            # set processed result
            object <- .copyAnalyzerOutputData(object, portf.betas.an)
            
            return(object)
          }
)
