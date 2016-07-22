sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model/risk_model_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/portfolio_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/instrument_betas_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/implied_factor_returns_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../MBAMsupport/risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(RColorBrewer)


################################################################################
#
# PortfolioFactorReturnsAnalysisBlock Class
# 
# Computation block class to pull data required for portfolio variance decomposition
# Pulls data required for computation and adds required columns.
###############################################################################

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
  Class             = "PortfolioFactorReturnsData",
  prototype         = list(
    required_colnms = c("Date", portfolio_decomposition_all_factors)
  ),
  contains          = c("VirtualImpliedFactorReturnsData")
)

setClass(
  Class             = "PortfolioFactorReturnsAnalysisBlock",
  slots             = c(
    portfolio              = "StrategyPortfolio",
    instrument_betas       = "InstrumentBetasData",
    implied_factor_returns = "ImpliedFactorReturnsData",
    output                 = "PortfolioFactorReturnsData"
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
    implied_factor_returns = new("ImpliedFactorReturnsData"),
    output          = new("PortfolioFactorReturnsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualRiskModelHandler",
                        "VirtualInstrumentBetasDataHandler",
                        "VirtualImpliedFactorReturnsDataHandler"
  )
)


setMethod("setRiskModelObject",  
          signature(object = "PortfolioFactorReturnsAnalysisBlock",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- .setRiskModelObject(object, risk_model)
            return(object)
          }
)


setMethod("setPortfolioDataObject",  
          signature(object = "PortfolioFactorReturnsAnalysisBlock", portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- .setPortfolioDataObject(object, portfolio)
            return(object)
          }
)

setMethod("setInstrumentBetasDataObject",  
          signature(object = "PortfolioFactorReturnsAnalysisBlock", instrument_betas = "InstrumentBetasData"),
          function(object, instrument_betas){
            object <- .setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)


setMethod("dataRequest",
          signature(object = "PortfolioFactorReturnsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
           
            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)
            
            portf_data <- getPortfolioDataObject(object)
            
            
            # retrieve portfolio data for query key_values
            if (getStoredNRows(portf_data) == 0) {
              portf_data <- tryCatch({
                dataRequest(portf_data, key_values)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
              })
              
              object <- .setPortfolioDataObject(object, portf_data)
            }
            
            
            # retrieve risk model instrument betas
            query_keys <- getReferenceData(portf_data)[c("Date", "InstrumentID")]
            
            # getting Instrument Betas data 
            betas_data <- getInstrumentBetasDataObject(object)
            risk_model <- getRiskModelObject(object)
            
            if (getStoredNRows(betas_data) == 0) {
              # important step to copy risk_model info
              betas_data <- .setRiskModelObject(betas_data, risk_model)
              
              betas_data <- tryCatch({
                dataRequest(betas_data, query_keys)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(betas_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
              })
              
              object <- .setInstrumentBetasDataObject(object, betas_data)
            }
            
            # getting Implied Factor Returns data 
            factor_ret <- getImpliedFactorReturnsDataObject(object)
            # important step to copy risk_model info
            factor_ret <- .setRiskModelObject(factor_ret, risk_model)
            
            query_keys <- unique(query_keys["Date"])
            factor_ret <- tryCatch({
              dataRequest(factor_ret, query_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_ret)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_ret), cond))
            })
            
            object <- .setImpliedFactorReturnsDataObject(object, factor_ret)

            return(object)
          }
)



setMethod("Process",  
          signature(object = "PortfolioFactorReturnsAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)
            
            betas_data <- getInstrumentBetasDataObject(object)
            betas <- getReferenceData(betas_data)
            
            factor_ret <- getImpliedFactorReturnsDataObject(object)
            all_fct_ir <- getReferenceData(factor_ret)

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
                fct_ir <- all_fct_ir[all_fct_ir$Date==rm_date,setdiff(colnames(all_fct_ir),'Date')]
                if(nrow(fct_ir)>0){
                  # The variance of log returns is equal to the variance of returns upto second order.
                  # 3/5 adjustment factor is derived from 3rd order term of the Taylor series expansion of log(1+x)^2
                  
                  market_ret <- portfolio_returns_decomposition(wt,bt,fct_ir)
                  total_sys_ret <- sum(market_ret)
                  factor_ret <- sum(market_ret[portfolio_decomposition_market_factors,])
                  currency_ret <- sum(market_ret[portfolio_decomposition_currency_factors,])
                  commodity_ret <- sum(market_ret[portfolio_decomposition_commodity_factors,])
                  sector_ret <- sum(market_ret[portfolio_decomposition_sector_factors,])
                  
                  rd <- data.frame(Date=rm_date,TotalSystematic=total_sys_ret[1],
                                                MarketFactor=factor_ret[1],
                                                Currency=currency_ret[1],
                                                Commodity=commodity_ret[1],
                                                Sector=sector_ret[1])
                  rd.tot <- cbind(rd, as.data.frame(t(market_ret)))
                  
                  if (!first){
                    rd.tot[,-1] <- returns_decomposition.tot[nrow(returns_decomposition.tot),-1] + rd.tot[,-1]
                  }
                  
                  plot_data <- stack(rd.tot, select = c(portfolio_decomposition_all_factors,
                                                        'TotalSystematic', 
                                                        'MarketFactor',
                                                        'Currency',
                                                        'Commodity',
                                                        'Sector')
                  )
                  
                  colnames(plot_data) <- c("Value", "RiskType")
                  plot_data$RiskGroup <- plot_data$RiskType
                  levels(plot_data$RiskGroup) <- portfolio_decomposition_factor_groups
                  plot_data <- data.frame(Date = rm_date, plot_data)
                  
                  if(first){
                    returns_decomposition <- rd
                    returns_decomposition.tot <- rd.tot
                    
                    ret_plot_data <- plot_data
                    first <- FALSE
                  }
                  else{
                    returns_decomposition.tot <- rbind(returns_decomposition.tot, rd.tot)
                    returns_decomposition <- rbind(returns_decomposition,rd)
                    ret_plot_data <- rbind(ret_plot_data, plot_data)
                    
                  } 
                }
              }
            }
            
            
            # x <- stack(returns_decomposition.tot, select = c(portfolio_decomposition_all_factors,
            #                                                  'TotalSystematic', 
            #                                                  'MarketFactor',
            #                                                  'Currency',
            #                                                  'Commodity',
            #                                                  'Sector')
            #            )
            # 
            # ret_plot_data <- rbind(data.frame(Date=returns_decomposition$Date,RiskType='TotalSystematic',Value=sqrt(returns_decomposition$TotalSystematicVar)*100),
            #                         data.frame(Date=returns_decomposition$Date,RiskType='MarketRiskFactor',Value=sqrt(returns_decomposition$MarketFactorVar)*100),
            #                         data.frame(Date=returns_decomposition$Date,RiskType='Currency',Value=sqrt(returns_decomposition$CurrencyVar)*100),
            #                         data.frame(Date=returns_decomposition$Date,RiskType='Commodity',Value=sqrt(returns_decomposition$CommodityVar)*100),
            #                         data.frame(Date=returns_decomposition$Date,RiskType='Sector',Value=sqrt(returns_decomposition$SectorVar)*100))
            
            
            for( group in names(portfolio_decomposition_factor_groups)) {
              ret_plot_data$Colour[ret_plot_data$RiskGroup == group] <- as.integer(as.factor(as.character(ret_plot_data$RiskType[ret_plot_data$RiskGroup == group] )))
            }
            
            browser()
            
            #Create a custom color scale
            myColors <- brewer.pal(brewer.pal.info["Set1", "maxcolors"],"Set1")
            myColors <- c(myColors, brewer.pal(brewer.pal.info["Set2", "maxcolors"],"Set2"))
            myColors <- c(myColors, brewer.pal(brewer.pal.info["Set3", "maxcolors"],"Set3"))
            myColors <- unique(myColors)
            myColors <- myColors[seq(length(unique(ret_plot_data$Colour)))]
            names(myColors) <- unique(ret_plot_data$Colour)
            
            col_pal <- merge(unique(ret_plot_data[c("RiskType", "RiskGroup", "Colour")]), data.frame(Colour = names(myColors), ColorVal = myColors))
            rownames(col_pal) <- col_pal$RiskType
            
            col_pal <- col_pal[order(col_pal$RiskGroup, col_pal$RiskType),]
            
            col_vals <- col_pal$ColorVal
            names(col_vals) <- col_pal$RiskType
            
            line_vals <- col_pal$RiskGroup
            names(line_vals) <- col_pal$RiskType
            
            colScale <- scale_colour_manual(name = "RiskType", values = col_vals, breaks = col_pal$RiskType)
            lineScale <- scale_linetype_manual(name = "RiskType", values = line_vals, breaks = col_pal$RiskType)
            
            plt_risk <- ggplot(data=ret_plot_data,aes(x=Date,y=Value,color=RiskType, linetype = RiskType)) +
              geom_line(size=1) + ylab("Cumulative Daily returns attribution (%)") +
              # scale_colour_manual(breaks  = ret_plot_data$RiskType,
              #                     values  = ret_plot_data$)
              colScale + lineScale +
              #guides(linetype=FALSE) +
              facet_grid(RiskGroup ~. ,scales = "free_y") +
              xlab("Date")
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, returns_decomposition.tot)
            object <- .setOutputObject(object, outp_object)
            
            object <- .setOutputGGPlotData(object, ret_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            
            return(object)
          }
)
