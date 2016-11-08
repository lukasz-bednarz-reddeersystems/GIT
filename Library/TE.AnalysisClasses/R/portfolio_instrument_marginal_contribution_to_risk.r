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


#' @describeIn setRiskModelObject
#' Set risk_model object in object slot
#' @inheritParams setRiskModelObject
#'
# ' @rdname setRiskModelObject-PortfolioInstrumentMCTRAnalysisBlock-method
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @param risk_model object of class "VirtualRiskModel"
# ' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
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


#' @describeIn setPortfolioDataObject
#' Set portfolio object in object slot
#' @inheritParams setPortfolioDataObject
#'
# ' @rdname setPortfolioDataObject-PortfolioFactorExposuresAnalysisBlock-method
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @param portfolio object of class "StrategyPortfolio"
# ' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)


#' @describeIn setInstrumentBetasDataObject
#' Set instrument betas object in object slot
#' @inheritParams setInstrumentBetasDataObject
#'
# ' @rdname setInstrumentBetasDataObject-PortfolioInstrumentMCTRAnalysisBlock-method
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @param instrument_betas object of class "InstrumentBetasData"
# ' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("setInstrumentBetasDataObject",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", instrument_betas = "InstrumentBetasData"),
          function(object, instrument_betas){
            object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)


#' @describeIn setFactorCorrelationDataObject
#' Set factor correlation data in object slot
#' @inheritParams setFactorCorrelationDataObject
#'
# ' @rdname setFactorCorrelationDataObject-PortfolioInstrumentMCTRAnalysisBlock-method
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @param factor_correlation object of class "FactorCorrelationData"
# ' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("setFactorCorrelationDataObject",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", factor_correlation = "FactorCorrelationData"),
          function(object, factor_correlation){
            object <- TE.RefClasses:::.setFactorCorrelationDataObject(object, factor_correlation)
            return(object)
          }
)


#' @describeIn setFactorVarianceDataObject
#' Set factor variance data in object slot
#' @inheritParams setFactorVarianceDataObject
#'
# ' @rdname setFactorVarianceDataObject-PortfolioInstrumentMCTRAnalysisBlock-method
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @param factor_variance object of class "FactorVarianceData"
# ' @return \code{object} object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("setFactorVarianceDataObject",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", factor_variance = "FactorVarianceData"),
          function(object, factor_variance){
            object <- TE.RefClasses:::.setFactorVarianceDataObject(object, factor_variance)
            return(object)
          }
)

#' @describeIn dataRequest
#'
#' Request data from data source
#'
#' @inheritParams dataRequest
#'
# ' @param object object of class 'PortfolioInstrumentMCTRAnalysisBlock'.
# ' @param key_values data.frame with keys specifying data query.
# ' @return \code{object} object of class 'PortfolioInstrumentMCTRAnalysisBlock'.
#' @export
setMethod("dataRequest",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)
            risk_model <- getRiskModelObject(object)
            lookback <- getRiskModelLookback(object)

            start <- max(key_values$start)
            end <- max(key_values$end)
            id <- unique(key_values$id)[1]

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


#' @describeIn Process
#'
#' Trigger computation of analysis data.
#'
#' @inheritParams Process
#'
# ' @param object object of class "PortfolioInstrumentMCTRAnalysisBlock"
# ' @return \code{object} object object of class "PortfolioInstrumentMCTRAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "PortfolioInstrumentMCTRAnalysisBlock"),
          function(object){

            # risk model
            lookback <- getRiskModelLookback(object)
            risk_model <- getRiskModelObject(object)

            factor_groups <- list(Currency = getRiskModelCurrencyFactorNames(risk_model),
                                  Commodity = getRiskModelCommodityFactorNames(risk_model),
                                  Sector    = getRiskModelSectorFactorNames(risk_model),
                                  Market    = getRiskModelMarketFactorNames(risk_model))

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
              }
            }

            ## Possibly for future use
            # ggobj <-  filter(vd.tot, RiskAggregate == "Factor Group") %>%
            #   ggvis(y = ~ Variance,
            #         x = ~ Date,
            #         stroke = ~ factor(RiskType),
            #         key  := ~ id)%>%
            #   layer_lines() %>%
            #   layer_points() %>%
            #   add_tooltip(tip, on = "hover")
                      # %>%
            # scale_numeric("x", domain = input_slider(min(vd.tot$Date), max(vd.tot$Date),
            #               c(min(vd.tot$Date), max(vd.tot$Date))))




            mctv_fg_df <- merge(mctv_fg_df, unique(port[c("Date", "InstrumentID", "Weight")]))

            mctv_fg_df <- mctv_fg_df[mctv_fg_df$ACV != 0.0 & mctv_fg_df$MCV != 0,]

            mctr_df.top10 <- arrange_(mctv_fg_df, "Date", "RiskType", "desc(ACV)")


            mctr_df.top10 <- Reduce(rbind,by(mctr_df.top10, mctr_df.top10[c("Date", "RiskType")], function(x){head(x, 10)}))

            mctr_df.top10 <- aggregate( cbind(ACV, MCR, PMCR) ~ Date + RiskType, data = mctr_df.top10, sum)

            plt_data <- rbind(data.frame(Date        = vd_df$Date,
                                         RiskMeasure = "Risk (%/Year)",
                                         RiskType    = vd_df$RiskType,
                                         Value       = 1600 * abs_sqrt(vd_df$Variance) ),
                              data.frame(Date        = mctr_df.top10$Date,
                                         RiskMeasure = "T10 ACR by ACV (%/Year)",
                                         RiskType    = mctr_df.top10$RiskType,
                                         Value       = 1600 * abs_sqrt(mctr_df.top10$ACV)),
                              data.frame(Date        = mctr_df.top10$Date,
                                         RiskMeasure = "T10 PMCR by ACV (%)",
                                         RiskType    = mctr_df.top10$RiskType,
                                         Value       = abs_sqrt(mctr_df.top10$PMCR))
                              )


            mctr_df.top10 <- arrange_(mctv_fg_df, "Date", "RiskType", "desc(MCV)")
            mctr_df.top10 <- Reduce(rbind,by(mctr_df.top10, mctr_df.top10[c("Date", "RiskType")], function(x){head(x, 10)}))

            mctr_df.top10 <- aggregate( cbind(ACV, MCR, PMCR) ~ Date + RiskType, data = mctr_df.top10, sum)



            plt_data <- rbind(plt_data,
                              data.frame(Date        = mctr_df.top10$Date,
                                         RiskMeasure = "T10 PMCR by MCV (%)",
                                         RiskType    = mctr_df.top10$RiskType,
                                         Value       = abs_sqrt(mctr_df.top10$PMCR))
            )

            plt_risk <- ggplot(data=plt_data,aes_string(x="Date",y="Value",colour="RiskType")) +
              geom_line(size = 1) +
              facet_grid( RiskMeasure ~ ., scales = "free_y") +
              xlab("Date") +
              labs(fill="Risk type")


            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, mctv_fg_df)
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

#' @describeIn setPortfolioDataObject
#' Set portfolio object in object slot
#' @inheritParams setPortfolioDataObject
#'
# ' @rdname setPortfolioDataObject-IndexPortfolioInstrumentMCTR-method
# ' @param object object of class "IndexPortfolioInstrumentMCTRAnalysisBlock"
# ' @param portfolio object of class "VirtualIndexPortfolio"
# ' @return \code{object} object of class "IndexPortfolioInstrumentMCTRAnalysisBlock""
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "IndexPortfolioInstrumentMCTRAnalysisBlock",
                    portfolio = "VirtualIndexPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)
