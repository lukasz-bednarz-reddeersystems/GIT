#' @include analysis_block.r
NULL

################################################################################
#
# PortfolioInstrumentMCTRAnalysisBlock Class
#
# Computation block class to pull data required for portfolio variance decomposition
# Pulls data required for computation and adds required columns.
###############################################################################


#' PortfolioVarianceFactorDecompositionData Reference Data class.
#'
#' Concrete S4 class storing data Portfolio Variance Decomposition
#' Data. Generated only within PortfolioInstrumentMCTRAnalysisBlock
#'
#' Inherits from "VirtualFactorVarianceData"
#'
#' @export

setClass(
  Class             = "PortfolioInstrumentMCTRData",
  prototype         = list(
    required_colnms = c("Date")
  ),
  contains          = c("VirtualFactorVarianceData")
)


#' Analysis Module for computation of Portfolio Decomposition
#'
#' Computation block class to pull data required for portfolio
#' variance decomposition. Pulls data required for computation
#' and adds required columns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPortfolioDataHandler",
#'               "VirtualRiskModelHandler",
#'               "VirtualInstrumentBetasDataHandler",
#'               "VirtualFactorCorrelationDataHandler",
#'               "VirtualFactorVarianceDataHandler"
#' @export

setClass(
  Class             = "PortfolioInstrumentMCTRAnalysisBlock",
  slots             = c(
    portfolio          = "Portfolio",
    instrument_betas   = "InstrumentBetasData",
    factor_correlation = "FactorCorrelationData",
    factor_variance    = "FactorVarianceData",
    instrument_residual_returns = "InstrumentResidualReturnsData",
    output             = "PortfolioInstrumentMCTRData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"),
                           c("id", "start", "end")),
    portfolio       = new("StrategyPortfolio"),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150.1.1"),
    instrument_betas = new("InstrumentBetasData"),
    factor_correlation = new("FactorCorrelationData"),
    factor_variance = new("FactorVarianceData"),
    instrument_residual_returns = new("InstrumentResidualReturnsData"),
    output          = new("PortfolioInstrumentMCTRData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualRiskModelHandler",
                        "VirtualInstrumentBetasDataHandler",
                        "VirtualFactorCorrelationDataHandler",
                        "VirtualFactorVarianceDataHandler",
                        "VirtualInstrumentResidualReturnsDataHandler"
  )
)


#' Set risk_model object in object slot
#'
#' Public method to set trade_data slot with "VirtualRiskModel"
#' class object
#'
#' @rdname setRiskModelObject-PortfolioInstrumentMCTRAnalysisBlock-method
#' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @param risk_model object of class "VirtualRiskModel"
#' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("setRiskModelObject",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- TE.RiskModel:::.setRiskModelObject(object, risk_model)

#             object <- .copyRiskModelToChildren(object)

            return(object)
          }
)


#' Request data from data source
#'
#' @param object object of class 'PortfolioInstrumentMCTRAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'PortfolioInstrumentMCTRAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)
            risk_model <- getRiskModelObject(object)
            lookback <- getRiskModelLookback(object)

            start <- max(key_values$start)
            end <- max(key_values$end)

            portf_data <- getPortfolioDataObject(object)


            # retrieve trade reference data for query key_values
            portf_data <- tryCatch({
              dataRequest(portf_data, key_values)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
            })

            object <- TE.RefClasses:::.setPortfolioDataObject(object, portf_data)

            # retrieve risk model instrument betas
            query_keys <- getReferenceData(portf_data)[c( "InstrumentID","Date")]


            # getting instrument residual returns
            res_returns <- getInstrumentResidualReturnsDataObject(object)
            # important step to copy risk_model info
            res_returns <- setRiskModelObject(res_returns, risk_model)

            res_returns <- tryCatch({
              dataRequest(res_returns, expand.grid(InstrumentID = unique(query_keys$InstrumentID),
                                                   Date = c(as.Date(min(query_keys$Date)) - lookback,
                                                            as.Date(max(query_keys$Date))
                                                            )
                                                   )
                          )

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(res_returns)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(res_returns), cond))
            })

            object <- TE.RefClasses:::.setInstrumentResidualReturnsDataObject(object, res_returns)



            # getting Instrument Betas data
            betas_data <- getInstrumentBetasDataObject(object)
            # important step to copy risk_model info
            betas_data <- setRiskModelObject(betas_data, risk_model)

            betas_data <- tryCatch({
              dataRequest(betas_data, query_keys)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(betas_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
            })

            object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, betas_data)


            # getting Factor Correlation data
            factor_corr <- getFactorCorrelationDataObject(object)
            # important step to copy risk_model info
            factor_corr <- setRiskModelObject(factor_corr, risk_model)

            query_keys <- unique(query_keys["Date"])
            factor_corr <- tryCatch({
              dataRequest(factor_corr, query_keys)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_corr)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_corr), cond))
            })

            object <- TE.RefClasses:::.setFactorCorrelationDataObject(object, factor_corr)


            # getting Factor Variance data
            factor_var <- getFactorVarianceDataObject(object)
            # important step to copy risk_model info
            factor_var <- setRiskModelObject(factor_var, risk_model)

            factor_var <- tryCatch({
              dataRequest(factor_var, query_keys)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_var)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_var), cond))
            })

            object <- TE.RefClasses:::.setFactorVarianceDataObject(object, factor_var)

            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @return \code{object} object object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock"),
          function(object){

            # risk model
            lookback <- getRiskModelLookback(object)

            # Lists for factor names

            # retrieve data
            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)

            betas_data <- getInstrumentBetasDataObject(object)
            betas <- getReferenceData(betas_data)

            factor_corr <- getFactorCorrelationDataObject(object)
            all_fct_cor <- getReferenceData(factor_corr)

            factor_var <- getFactorVarianceDataObject(object)
            all_fct_sd <- getReferenceData(factor_var)

            res_ret_data <- getInstrumentResidualReturnsDataObject(object)
            res_ret <- getReferenceData(res_ret_data)
            res_ret <- merge(res_ret, expand.grid(Date = unique(res_ret$Date), InstrumentID = unique(res_ret$InstrumentID)), all.y = TRUE)
            res_ret[is.na(res_ret)] <- 0

            # compute output

            browser()
            first <- TRUE
            for(rm_date in sort(unique(port$Date))){

              rm_date <- as_date(rm_date)

              if(wday(rm_date)!=7&wday(rm_date)!=1){
                bt <- betas[betas$Date==rm_date,setdiff(colnames(betas),'Date')]
                bt[is.na(bt)] <- 0
                wt <- port[port$Date==rm_date,c('InstrumentID','Weight')]
                colnames(wt) <- c('InstrumentID','Weight')
                fct_cor <- all_fct_cor[all_fct_cor$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]
                fct_sd  <- all_fct_sd[all_fct_sd$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]

                if(nrow(fct_cor)>0 && nrow(fct_sd)>0){
                  # The variance of log returns is equal to the variance of returns upto second order.
                  # 3/5 adjustment factor is derived from 3rd order term of the Taylor series expansion of log(1+x)^2
                  fct_cov <- tryCatch({
                    factor_covariance(fct_cor,fct_sd)
                  }, error = function(cond){

                    message(paste("I have encountered an error in cov_matrix on", rm_date, ". Skipping this date."))
                    NULL
                  })
                  if (is.null(fct_cov)) next()
                  #fct_cov <- 365*3/5*factor_covariance(fct_cor, sqrt(fct_sd))/150

                  mctr_info <- portfolio_instrument_mctv(wt,bt,fct_cov, ins_var, corr_residuals = FALSE)

                  instr_mctv <- mctr_info$instr_risk
                  instr_mctr <- instr_mctv/2/sqrt(mctr_info$total_variance)

                  total_sys_var <- mctr_info$total_variance

                  mctr_info <- portfolio_instrument_mctv(wt,bt,fct_cov, ins_var, corr_residuals = TRUE)

                  instr_mctv_c <- mctr_info$instr_risk
                  instr_mctr_c <- instr_mctv/2/sqrt(mctr_info$total_variance)

                  total_sys_var_c <- portfolio_total_variance(wt,bt,fct_cov, ins_var, corr_residuals = TRUE )



                  vd.tot <- data.frame(Date=rm_date,TotalSystematicVar=total_sys_var[1])
                  vd <- cbind(data.frame(Date = rm_date), as.data.frame(InstrumentID = wd$InstrumentID, MCTR = instr_mctr/sum(market_risk)*100))
                  if(first){
                    variance_decomposition <- vd
                    variance_decomposition.tot <- vd.tot
                    first <- FALSE
                  }
                  else{
                    variance_decomposition.tot <- rbind(variance_decomposition.tot, vd.tot)
                    variance_decomposition <- rbind(variance_decomposition,vd)
                  }
                }
              }
            }

            browser()
            risk_plot_data <- rbind(data.frame(Date     = variance_decomposition$Date,
                                               RiskType = 'TotalSystematic',
                                               Value    = abs_sqrt(variance_decomposition$TotalSystematicVar)*1e2),
                                    data.frame(Date     = variance_decomposition$Date,
                                               RiskType = 'Sector',
                                               Value    = abs_sqrt(variance_decomposition$SectorVar)*1e2))
            plt_risk <- ggplot(data=risk_plot_data,aes_string(x="Date",y="Value",colour="RiskType")) +
              geom_line(size=1) + ylab("Daily risk attribution (bps)") + xlab("Date") + labs(fill="Risk type")

            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, variance_decomposition.tot)
            object <- .setOutputObject(object, outp_object)

            object <- .setOutputGGPlotData(object, risk_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Value")))

            return(object)
          }
)


################################################################################
#
# IndexPortfolioInstrumentMCTRAnalysisBlock Class
#
# Computation block class to pull data required for index portfolio variance
# decomposition. Pulls data required for computation and adds required columns.
###############################################################################

#' Analysis Module for computation of Index Portfolio Decomposition
#'
#' Computation block class to pull data required for index portfolio
#' variance decomposition. Pulls data required for computation
#' and adds required columns.
#'
#' Inherits from "PortfolioInstrumentMCTRAnalysisBlock"
#'
#' @export
setClass(
  Class             = "IndexPortfolioInstrumentMCTRAnalysisBlock",
  prototype         = list(
    key_cols        = c("IndexTicker", "start", "end"),
    key_values      = data.frame(IndexTicker = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("IndexTicker", "start", "end"),
                           c("id", "start", "end")),
    portfolio       = new("IndexPortfolio.BE500"),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150.1.1")

  ),
  contains          = c("PortfolioInstrumentMCTRAnalysisBlock")
)

#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "VirtualIndexPortfolio"
#' class object
#'
#' @rdname setPortfolioDataObject-IndexPortfolioInstrumentMCTR-method
#' @param object object of class "IndexPortfolioInstrumentMCTRAnalysisBlock"
#' @param portfolio object of class "VirtualIndexPortfolio"
#' @return \code{object} object of class "IndexPortfolioInstrumentMCTRAnalysisBlock""
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "IndexPortfolioInstrumentMCTRAnalysisBlock",
                    portfolio = "VirtualIndexPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)
