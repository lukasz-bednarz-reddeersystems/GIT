#' @include report_analysis_block.r
NULL


################################################################################
#
# RiskReviewReport Class
#
# Computation block class to generate plots required for risk review
# Pulls data required for computation and outputs plots to plot list
###############################################################################

#' List of Analysis modules used by RiskReviewReport Class
risk_review_analysis_blocks <- c(
                                             "PortfolioVarianceDecomposition",
                                             "PortfolioInstrumentMCTR",
                                             "PortfolioFactorReturns",
                                             "PortfolioFactorExposures"
)


#' Risk Review Report class.
#'
#' Report class computing following blocks:
#'  "PortfolioVarianceDecomposition",
#'  "PortfolioFactorReturns",
#'  "PortfolioFactorExposures"
#'
#' Inherits from "VirtualReportAnalysisBlock"
#'
#' @export

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


#' Request data from data source
#'
#' @param object object of class 'RiskReviewReport'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'RiskReviewReport'.
#' @export

setMethod("dataRequest",
          signature(object = "RiskReviewReport", key_values = "data.frame"),
          function(object, key_values){

            object <- setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)


#' Trigger computation of report data.
#'
#' @param object object of class "PositionsHoldingPeriodReport"
#' @return \code{object} object object of class "PositionsHoldingPeriodReport"
#' @export
setMethod("Process",
          signature(object = "RiskReviewReport"),
          function(object){
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

            portf_data         <- getPortfolioDataObject(portf.var.an)
            betas_data         <- getInstrumentBetasDataObject(portf.var.an)
            factor_correlation <- getFactorCorrelationDataObject(portf.var.an)
            factor_variance    <- getFactorVarianceDataObject(portf.var.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, portf.var.an)


            ######################################################
            #
            # PortfolioInstrumentMCTRAnalysisBlock xx
            #
            ######################################################

            # create/get data/process offside positions analyzer
            portf.mcr.an <- new("PortfolioInstrumentMCTRAnalysisBlock")

            # set data
            portf.mcr.an <- setPortfolioDataObject(portf.mcr.an, portf_data)
            portf.mcr.an <- setInstrumentBetasDataObject(portf.mcr.an, betas_data)
            portf.mcr.an <- setFactorCorrelationDataObject(portf.mcr.an, factor_correlation)
            portf.mcr.an <- setFactorVarianceDataObject(portf.mcr.an, factor_variance)

            # gets required data
            portf.mcr.an <- dataRequest(portf.mcr.an, key_values)

            #process
            portf.mcr.an <- Process(portf.mcr.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, portf.mcr.an)

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
