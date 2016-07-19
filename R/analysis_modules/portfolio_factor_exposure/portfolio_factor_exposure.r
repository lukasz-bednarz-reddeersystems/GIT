sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/portfolio_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/factor_exposure_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# PortfolioFactorExposureAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of strategy breakdown plot
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "PortfolioFactorExposureAnalysisBlock",
  slots             = c(
    portfolio       = "StrategyPortfolio"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    required_colnms = c('Strategy','InstrumentID','Date','MarketValue','TodayPL','ValueUSD'),
    portfolio       = new("StrategyPortfolio")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualFactorExposureDataHandler"
                        )
)


setMethod("dataRequest",
          signature(object = "PortfolioFactorExposureAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
           
            trader <- unique(key_values$TraderID)[1]
            
            portf_data <- new("StrategyPortfolio")
            
            # retrieve trade reference data for query key_values
            portf_data <- tryCatch({
              dataRequest(portf_data, key_values)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
            })
            
            object <- .setPortfolioDataObject(object, portf_data)
            
            fct_exp_data <- getFactorExposureDataObject(object)
            
            # retrieve event reference data for query key_values
            if (getStoredNRows(fct_exp_data) == 0) {
              
              portf_df <- getReferenceData(portf_data)
              instruments <- unique(portf_df$InstrumentID)
              dates <- unique(portf_df$Date)
              
              instr_data_keys <- expand.grid(InstrumentID = instruments, Date = dates)
              
              fct_exp_data <- tryCatch({
                dataRequest(fct_exp_data, instr_data_keys)
                
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(fct_exp_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(fct_exp_data), cond))
              })
              
              object <- .setFactorExposureDataObject(object, fct_exp_data)
            }
            
            return(object)
          }
)



setMethod("Process",  
          signature(object = "PortfolioFactorExposureAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)
            
            fact_exp_data <- getFactorExposureDataObject(object)
            factor_exposures <- getReferenceData(fact_exp_data)
            
            thisQ <- quarter(max(port$Date), with_year = TRUE)
            
            # compute output
            factor_data <- factor_exposures[c('InstrumentID','Date','FactorName','ZScore')]
            factor_summary <- merge(port,factor_data,by=c('InstrumentID','Date'))
            only_factors <- c('rValue','rStrength','rGrowth','rSize','rSectorTrendExtension','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')
            factor_summary <- factor_summary[factor_summary$FactorName%in%only_factors,]
            factor_summary$TotalExposure <- factor_summary$ZScore*factor_summary$Weight
            factor_summary$Q <- quarter(factor_summary$Date, with_year = TRUE)
            fct_smmry <- aggregate(factor_summary[c('TotalExposure')],list(FactorName=factor_summary$FactorName,Quarter=factor_summary$Q,Date=factor_summary$Date),function(x)sum(x,na.rm=TRUE))
            fct_smmry <- aggregate(fct_smmry[c('TotalExposure')],list(Quarter=(fct_smmry$Q==thisQ),Factor=fct_smmry$FactorName),function(x)mean(x,na.rm=TRUE))
            fct_smmry <- merge(fct_smmry[fct_smmry$Quarter,],fct_smmry[!fct_smmry$Quarter,],by='Factor')
            fct_smmry$Delta <- 100*(fct_smmry$TotalExposure.x - fct_smmry$TotalExposure.y)/abs(fct_smmry$TotalExposure.y)
            fct_smmry$TotalExposure <- fct_smmry$TotalExposure.x
            
            clipper <- function(x)substr(x,2,nchar(x))
            fct_smmry$Factor <- clipper(as.character(fct_smmry$Factor))
            exprs_smmry <- ggplot(data=fct_smmry, aes(x=reorder(Factor,TotalExposure), fill=Factor)) +
              geom_bar(aes(weight=TotalExposure)) +
              ylab("Exposure") + xlab("Factor") + ggtitle('Factor exposure') +
              geom_text(aes(x= Factor, y=TotalExposure, label = paste(round(Delta),"%",sep="")),size=4) +  
              theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
              scale_fill_brewer(palette="Set3")

            
            object <- .setOutputGGPlotData(object, fct_smmry)
            object <- .setOutputGGPlot(object, exprs_smmry)
            
            return(object)
          }
)
