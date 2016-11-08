#' @include analysis_block.r
NULL

################################################################################
#
# PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock Class
#
# Computation block class to pull data required for portfolio variance decomposition
# Pulls data required for computation and adds required columns.
###############################################################################


#' PortfolioVarianceFactorDecompositionData Reference Data class.
#'
#' Concrete S4 class storing data Portfolio Variance Decomposition
#' Data. Generated only within PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock
#'
#' Inherits from "VirtualFactorVarianceData"
#'
#' @export

setClass(
  Class             = "PortfolioHedgeInstrumentCoreFactorsMCTRData",
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
  Class             = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock",
  slots             = c(
    portfolio          = "Portfolio",
    instrument_betas   = "InstrumentBetasData",
    factor_correlation = "FactorCorrelationData",
    factor_variance    = "FactorVarianceData",
    instrument_residual_returns = "InstrumentResidualReturnsData",
    output             = "PortfolioHedgeInstrumentCoreFactorsMCTRData"
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
    output          = new("PortfolioHedgeInstrumentCoreFactorsMCTRData")
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
#' @rdname setRiskModelObject-PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock-method
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @param risk_model object of class "VirtualRiskModel"
#' @return \code{object} object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("setRiskModelObject",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- TE.RiskModel:::.setRiskModelObject(object, risk_model)

#             object <- .copyRiskModelToChildren(object)

            return(object)
          }
)


#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "StrategyPortfolio"
#' class object
#'
#' @rdname setPortfolioDataObject-PortfolioFactorExposuresAnalysisBlock-method
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @param portfolio object of class "StrategyPortfolio"
#' @return \code{object} object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock", portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)


#' Set instrument_betas object in object slot
#'
#' Public method to set instrument_betas slot with "InstrumentBetasData"
#' class object
#'
#' @rdname setInstrumentBetasDataObject-PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock-method
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @param instrument_betas object of class "InstrumentBetasData"
#' @return \code{object} object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("setInstrumentBetasDataObject",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock", instrument_betas = "InstrumentBetasData"),
          function(object, instrument_betas){
            object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)


#' Set factor_correlation object in object slot
#'
#' Public method to set factor_correlation slot with "InstrumentBetasData"
#' class object
#'
#' @rdname setFactorCorrelationDataObject-PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock-method
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @param factor_correlation object of class "FactorCorrelationData"
#' @return \code{object} object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("setFactorCorrelationDataObject",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock", factor_correlation = "FactorCorrelationData"),
          function(object, factor_correlation){
            object <- TE.RefClasses:::.setFactorCorrelationDataObject(object, factor_correlation)
            return(object)
          }
)


#' Set factor_variance object in object slot
#'
#' Public method to set factor_variance slot with "InstrumentBetasData"
#' class object
#'
#' @rdname setFactorVarianceDataObject-PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock-method
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @param factor_variance object of class "FactorVarianceData"
#' @return \code{object} object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("setFactorVarianceDataObject",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock", factor_variance = "FactorVarianceData"),
          function(object, factor_variance){
            object <- TE.RefClasses:::.setFactorVarianceDataObject(object, factor_variance)
            return(object)
          }
)

#' Request data from data source
#'
#' @param object object of class 'PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock'.
#' @export
setMethod("dataRequest",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)
            risk_model <- getRiskModelObject(object)
            lookback <- getRiskModelLookback(object)

            id <- unique(key_values$id)[1]
            start <- max(key_values$start)
            end <- max(key_values$end)


            portf_data <- getPortfolioDataObject(object)



            if(getStoredNRows(portf_data) == 0){
              # retrieve trade reference data for query key_values
              portf_data <- tryCatch({
                dataRequest(portf_data, key_values)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
              })

              object <- TE.RefClasses:::.setPortfolioDataObject(object, portf_data)
            }

            # retrieve risk model instrument betas
            query_keys <- getReferenceData(portf_data)[c( "InstrumentID","Date")]

            ins_query_keys <- expand.grid(InstrumentID = unique(query_keys$InstrumentID),
                                          Date = c(as.Date(min(query_keys$Date)) - lookback,
                                                   as.Date(max(query_keys$Date))
                                          )
                                          )


            # getting instrument residual returns
            res_returns <- getInstrumentResidualReturnsDataObject(object)

            if(getStoredNRows(res_returns) == 0){
              # important step to copy risk_model info
              res_returns <- setRiskModelObject(res_returns, risk_model)


              res_returns <- tryCatch({
                dataRequest(res_returns,ins_query_keys)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(res_returns)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(res_returns), cond))
              })

              object <- TE.RefClasses:::.setInstrumentResidualReturnsDataObject(object, res_returns)
            }


            # getting Instrument Betas data
            betas_data <- getInstrumentBetasDataObject(object)

            if(getStoredNRows(betas_data) == 0){
              # important step to copy risk_model info
              betas_data <- setRiskModelObject(betas_data, risk_model)

              betas_data <- tryCatch({
                bt <- dataRequest(betas_data, ins_query_keys)
                betas <- getReferenceData(bt)
                betas <- adjust_ipo_betas(betas)
                bt <- setReferenceData(bt, betas)
                bt

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(betas_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
              })

              object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, betas_data)
            }

            # getting Factor Correlation data
            factor_corr <- getFactorCorrelationDataObject(object)

            if(getStoredNRows(factor_corr) == 0){
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
            }


            # getting Factor Variance data
            factor_var <- getFactorVarianceDataObject(object)

            if(getStoredNRows(factor_var) == 0){
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
            }

            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @return \code{object} object object of class "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"),
          function(object){

            # risk model
            lookback <- getRiskModelLookback(object)
            # risk_model <- getRiskModelObject(object)

            # factor_groups <- list(Currency = getRiskModelCurrencyFactorNames(risk_model),
            #                       Commodity = getRiskModelCommodityFactorNames(risk_model),
            #                       Sector    = getRiskModelSectorFactorNames(risk_model),
            #                       Market    = getRiskModelMarketFactorNames(risk_model))

            core_factors <- c("CHF", "EUR", "JPY",
                              "PriceMomentum12M",
                              "PriceMomentum1M",
                              "Size",
                              "Value",
                              "Volatility")

            names(core_factors) <- core_factors
            factor_groups <- as.list(core_factors)

            # retrieve data
            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)

            strats <- unique(port$Strategy)

            strats <- strats[grepl("HEDGE|HDG", strats)]

            port <- port[port$Strategy %in% strats, ]

            betas_data <- getInstrumentBetasDataObject(object)
            betas <- getReferenceData(betas_data)


            factor_corr <- getFactorCorrelationDataObject(object)
            all_fct_cor <- getReferenceData(factor_corr)

            factor_var <- getFactorVarianceDataObject(object)
            all_fct_sd <- getReferenceData(factor_var)

            res_ret_data <- getInstrumentResidualReturnsDataObject(object)
            all_res_ret <- getReferenceData(res_ret_data)
            all_res_ret <- merge(all_res_ret, expand.grid(Date = unique(all_res_ret$Date), InstrumentID = unique(all_res_ret$InstrumentID)), all.y = TRUE)
            #all_res_ret[is.na(all_res_ret)] <- 0

            # compute output


            first <- TRUE
            for(rm_date in sort(unique(port$Date))){

              rm_date <- as_date(rm_date)

              if(wday(rm_date)!=7&wday(rm_date)!=1){
                bt <- betas[betas$Date==rm_date,setdiff(colnames(betas),'Date')]

                bt[is.na(bt)] <- 0
                wt <- unique(port[port$Date==rm_date,c('InstrumentID','Weight')])
                wt <- wt[wt$Weight != 0.0, ]
                colnames(wt) <- c('InstrumentID','Weight')
                fct_cor <- all_fct_cor[all_fct_cor$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]
                fct_sd  <- all_fct_sd[all_fct_sd$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]

                res_ret <- all_res_ret[all_res_ret$Date > rm_date - lookback & all_res_ret$Date <= rm_date, ]

                res_ret <- unique(res_ret)

                res_ret <- merge(expand.grid(InstrumentID = wt[,"InstrumentID"], Date = unique(res_ret$Date)), res_ret, all.x = TRUE)

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

                  mctr_info <- portfolio_instrument_mctv(wt,bt,fct_cov, res_ret, factor_groups, corr_residuals = TRUE)

                  vd.tot <- cbind(data.frame(Date = rm_date), mctr_info$summary_risk)
                  mctr <- cbind(data.frame(Date = rm_date), mctr_info$instr_risk)
                  mctr_pg <- cbind(data.frame(Date = rm_date), mctr_info$instr_group_risk)

                  if(first){
                    mctr_df <- mctr
                    vd_df <- vd.tot
                    mctv_fg_df <- mctr_pg
                    first <- FALSE
                  }
                  else{
                    vd_df <- rbind(vd_df, vd.tot)
                    mctr_df <- rbind(mctr_df,mctr)
                    mctv_fg_df <- rbind(mctv_fg_df,mctr_pg)

                  }
                }
                else {
                  browser()
                }
              }
            }

            mctv_fg_df <- merge(mctv_fg_df, unique(port[c("Date", "InstrumentID", "Weight")]))

            mctv_fg_df <- mctv_fg_df[mctv_fg_df$ACV != 0.0 & mctv_fg_df$MCV != 0,]

            mctr_df.top10 <- arrange(mctv_fg_df, Date, RiskType, desc(ACV))


            mctr_df.top10 <- Reduce(rbind,by(mctr_df.top10, mctr_df.top10[c("Date", "RiskType")], function(x){head(x, 10)}))


            mctr_df.top10$RiskAggregate = "Systematic"

            mctr_df_tot.top10 <- merge(mctr_df.top10[c("Date", "InstrumentID", "RiskType", "Weight")],
                                       mctr_df[setdiff(colnames(mctr_df), "RiskType")])


            mctr_df_tot.top10$RiskAggregate = "Total"

            mctr_df_tot.top10.aggr <- aggregate( cbind(ACV, MCR, PMCR) ~ Date + RiskType, data = mctr_df_tot.top10, sum)

            mctr_df.top10.aggr <- aggregate( cbind(ACV, MCR, PMCR) ~ Date + RiskType, data = mctr_df.top10, sum)

            mctr_df.out <- rbind(mctr_df.top10, mctr_df_tot.top10)


            cf_vd_df <- vd_df[vd_df$RiskAggregate == "Factor Group",]

            plt_data <- rbind(data.frame(Date        = cf_vd_df$Date,
                                         RiskMeasure = "Factor Risk (ACR) (%/Year)",
                                         RiskType    = cf_vd_df$RiskType,
                                         Value       = 1600 * abs_sqrt(cf_vd_df$Variance) ),

                              data.frame(Date        = mctr_df.top10.aggr$Date,
                                         RiskMeasure = "T10 Sys.ACR by ACV (%/Year)",
                                         RiskType    = mctr_df.top10.aggr$RiskType,
                                         Value       = 1600 * abs_sqrt(mctr_df.top10.aggr$ACV)),

                              data.frame(Date        = mctr_df_tot.top10.aggr$Date,
                                         RiskMeasure = "T10  Tot.ACR by ACV (%/Year)",
                                         RiskType    = mctr_df_tot.top10.aggr$RiskType,
                                         Value       = 1600 * abs_sqrt(mctr_df_tot.top10.aggr$ACV)),

                              data.frame(Date        = mctr_df_tot.top10.aggr$Date,
                                         RiskMeasure = "T10 total PMCR by ACV (%)",
                                         RiskType    = mctr_df_tot.top10.aggr$RiskType,
                                         Value       = abs_sqrt(mctr_df_tot.top10.aggr$PMCR))
                              )


            plt_risk <- ggplot(data=plt_data,aes_string(x="Date",y="Value",colour="RiskType")) +
              geom_line(size = 1) +
              facet_grid( RiskMeasure ~ ., scales = "free_y") +
              xlab("Date") +
              labs(fill="Risk type") +
              ggtitle("Risk Contribution of Hedges by Core factors")



            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, mctr_df.out)
            object <- .setOutputObject(object, outp_object)

            object <- .setOutputGGPlotData(object, plt_data)
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
#' Inherits from "PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock"
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
  contains          = c("PortfolioHedgeInstrumentCoreFactorsMCTRAnalysisBlock")
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
