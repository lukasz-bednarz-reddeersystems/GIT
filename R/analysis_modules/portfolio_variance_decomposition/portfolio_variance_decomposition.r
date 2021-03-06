sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model/risk_model_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/portfolio_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/instrument_betas_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/factor_correlation_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/factor_variance_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../MBAMsupport/risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)



################################################################################
#
# PortfolioVarianceDecompositionAnalysisBlock Class
# 
# Computation block class to pull data required for portfolio variance decomposition
# Pulls data required for computation and adds required columns.
###############################################################################


portfolio_decomposition_market_factors <- c('Earnings','Growth','PriceMomentum12M','PriceMomentum1M','Size','StreetSentiment','Strength','TrendExtension','Value','Volatility')
portfolio_decomposition_currency_factors <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')
portfolio_decomposition_commodity_factors <- c('WTI')
portfolio_decomposition_sector_factors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')

portfolio_decomposition_all_factors <- c(portfolio_decomposition_market_factors,
                                         portfolio_decomposition_currency_factors,
                                         portfolio_decomposition_commodity_factors,
                                         portfolio_decomposition_sector_factors)


portfolio_decomposition_composition_factors <- c('TotalSystematic', 
                                                 'MarketFactor',
                                                 'Currency',
                                                 'Commodity',
                                                 'Sector')

portfolio_decomposition_factor_groups <- list(Composition = portfolio_decomposition_composition_factors,
                                              Market = portfolio_decomposition_market_factors,
                                              Currency = portfolio_decomposition_currency_factors,
                                              Commodity = portfolio_decomposition_commodity_factors,
                                              Sector= portfolio_decomposition_sector_factors)


setClass(
  Class             = "PortfolioVarianceFactorDecompositionData",
  prototype         = list(
    required_colnms = c("Date", portfolio_decomposition_all_factors)
  ),
  contains          = c("VirtualFactorVarianceData")
)

setClass(
  Class             = "PortfolioVarianceDecompositionAnalysisBlock",
  slots             = c(
    portfolio          = "StrategyPortfolio",
    instrument_betas   = "InstrumentBetasData",
    factor_correlation = "FactorCorrelationData",
    factor_variance    = "FactorVarianceData",
    output             = "PortfolioVarianceFactorDecompositionData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), 
                           c("id", "start", "end")),
    portfolio       = new("StrategyPortfolio"),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150"),
    instrument_betas = new("InstrumentBetasData"),
    factor_correlation = new("FactorCorrelationData"),
    factor_variance = new("FactorVarianceData"),
    output          = new("PortfolioVarianceFactorDecompositionData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualRiskModelHandler",
                        "VirtualInstrumentBetasDataHandler",
                        "VirtualFactorCorrelationDataHandler",
                        "VirtualFactorVarianceDataHandler"
  )
)


setMethod("setRiskModelObject",  
          signature(object = "PortfolioVarianceDecompositionAnalysisBlock",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- .setRiskModelObject(object, risk_model)
            return(object)
          }
)



setMethod("dataRequest",
          signature(object = "PortfolioVarianceDecompositionAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
           
            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)
            
            portf_data <- getPortfolioDataObject(object)
            
            
            # retrieve trade reference data for query key_values
            portf_data <- tryCatch({
              dataRequest(portf_data, key_values)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
            })
            
            object <- .setPortfolioDataObject(object, portf_data)
            
            # retrieve risk model instrument betas
            query_keys <- getReferenceData(portf_data)[c("Date", "InstrumentID")]
            
            # getting Instrument Betas data 
            betas_data <- getInstrumentBetasDataObject(object)
            # important step to copy risk_model info
            risk_model <- getRiskModelObject(object)
            betas_data <- .setRiskModelObject(betas_data, risk_model)
            
            betas_data <- tryCatch({
              dataRequest(betas_data, query_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(betas_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
            })
            
            browser()
            
            object <- .setInstrumentBetasDataObject(object, betas_data)
            
            
            # getting Factor Correlation data 
            factor_corr <- getFactorCorrelationDataObject(object)
            # important step to copy risk_model info
            factor_corr <- .setRiskModelObject(factor_corr, risk_model)
            
            query_keys <- unique(query_keys["Date"])
            factor_corr <- tryCatch({
              dataRequest(factor_corr, query_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_corr)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_corr), cond))
            })
            
            object <- .setFactorCorrelationDataObject(object, factor_corr)
            
            
            # getting Factor Variance data 
            factor_var <- getFactorVarianceDataObject(object)
            # important step to copy risk_model info
            factor_var <- .setRiskModelObject(factor_var, risk_model)
            
            factor_var <- tryCatch({
              dataRequest(factor_var, query_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_var)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_var), cond))
            })
            
            object <- .setFactorVarianceDataObject(object, factor_var)
            
            return(object)
          }
)



setMethod("Process",  
          signature(object = "PortfolioVarianceDecompositionAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)
            
            betas_data <- getInstrumentBetasDataObject(object)
            betas <- getReferenceData(betas_data)
            
            factor_corr <- getFactorCorrelationDataObject(object)
            all_fct_cor <- getReferenceData(factor_corr)
            
            factor_var <- getFactorVarianceDataObject(object)
            all_fct_sd <- getReferenceData(factor_var)
            
            rm_date_end <- max(port$Date)
            rm_date_start <- min(port$Date)
            
            # compute output
            
            first <- TRUE
            for(rm_date in sort(unique(port$Date))){
              
              rm_date <- as.Date(rm_date)
              
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
                    365*3/5*factor_covariance(fct_cor,fct_sd)
                  }, error = function(cond){
                    message(paste("I have encountered an error in cov_matrix on", rm_date, ". Skipping this date."))
                    NULL
                  })
                  if (is.null(fct_cov)) next()
                  #fct_cov <- 365*3/5*factor_covariance(fct_cor, sqrt(fct_sd))/150
                  market_risk <- portfolio_variance_decomposition(wt,bt,fct_cov)
                  total_sys_var <- sum(market_risk)
                  factor_var <- sum(market_risk[portfolio_decomposition_market_factors,])
                  currency_var <- sum(market_risk[portfolio_decomposition_currency_factors,])
                  commodity_var <- sum(market_risk[portfolio_decomposition_commodity_factors,])
                  sector_var <- sum(market_risk[portfolio_decomposition_sector_factors,])
                  
                  # total_sys_var <- portfolio_variance_decomposition(wt,bt,fct_cov)
                  # factor_var <- portfolio_variance_decomposition(wt,bt,fct_cov,portfolio_decomposition_market_factors)
                  # currency_var <- portfolio_variance_decomposition(wt,bt,fct_cov,portfolio_decomposition_currency_factors)
                  # commodity_var <- portfolio_variance_decomposition(wt,bt,fct_cov,portfolio_decomposition_commodity_factors)
                  # sector_var <- portfolio_variance_decomposition(wt,bt,fct_cov,portfolio_decomposition_sector_factors)
                  vd <- data.frame(Date=rm_date,TotalSystematicVar=total_sys_var[1],MarketFactorVar=factor_var[1],CurrencyVar=currency_var[1],CommodityVar=commodity_var[1],SectorVar=sector_var[1])
                  vd.tot <- cbind(data.frame(Date = rm_date), as.data.frame(t(market_risk)/sum(market_risk)*100))
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
            
            risk_plot_data <- rbind(data.frame(Date=variance_decomposition$Date,RiskType='TotalSystematic',Value=sqrt(variance_decomposition$TotalSystematicVar)*10000),
                                    data.frame(Date=variance_decomposition$Date,RiskType='MarketRiskFactor',Value=sqrt(variance_decomposition$MarketFactorVar)*10000),
                                    data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=sqrt(variance_decomposition$CurrencyVar)*10000),
                                    data.frame(Date=variance_decomposition$Date,RiskType='Commodity',Value=sqrt(variance_decomposition$CommodityVar)*10000),
                                    data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=sqrt(variance_decomposition$CurrencyVar)*10000),
                                    data.frame(Date=variance_decomposition$Date,RiskType='Sector',Value=sqrt(variance_decomposition$SectorVar)*10000))
            # risk_plot_data <- rbind(data.frame(Date=variance_decomposition$Date,RiskType='TotalSystematic',Value=variance_decomposition$TotalSystematicVar*100),
            #                         data.frame(Date=variance_decomposition$Date,RiskType='MarketRiskFactor',Value=variance_decomposition$MarketFactorVar*100),
            #                         data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=variance_decomposition$CurrencyVar*100),
            #                         data.frame(Date=variance_decomposition$Date,RiskType='Commodity',Value=variance_decomposition$CommodityVar*100),
            #                         data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=variance_decomposition$CurrencyVar*100),
            #                         data.frame(Date=variance_decomposition$Date,RiskType='Sector',Value=variance_decomposition$SectorVar*100))
            plt_risk <- ggplot(data=risk_plot_data,aes(x=Date,y=Value,colour=RiskType)) +
              geom_line(size=1) + ylab("Daily risk attribution (bps)") + xlab("Date") + labs(fill="Risk type") 
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, variance_decomposition.tot)
            object <- .setOutputObject(object, outp_object)
            
            object <- .setOutputGGPlotData(object, risk_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            
            return(object)
          }
)
