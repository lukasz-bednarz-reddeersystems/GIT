#' @include analysis_block.r
NULL

################################################################################
#
# PortfolioFactorExposureAnalysisBlock Class
#
# Computation block class to pull data required for Computation Portfolio Factor
# Exposure
###############################################################################



#' Analysis Module for extraction Factor Exposure of Portfolio
#'
#' Computation block class to pull data required for Computation Portfolio Factor
# Exposure
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPortfolioDataHandler",
#'               "VirtualFactorExposureDataHandler"
#'
#' @export
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


#' Request data from data source
#'
#' @param object object of class 'PortfolioFactorExposureAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'PortfolioFactorExposureAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "PortfolioFactorExposureAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            id <- unique(key_values[,1])[1]
            start <- min(key_values$start)
            end <- max(key_values$end)

            portf_data <- new("StrategyPortfolio")

            # retrieve trade reference data for query key_values
            portf_data <- tryCatch({
              dataRequest(portf_data, key_values)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(portf_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, stop))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(portf_data), cond))
            })

            object <- TE.RefClasses:::.setPortfolioDataObject(object, portf_data)

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
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(fct_exp_data), cond))
              })

              object <- TE.RefClasses:::.setFactorExposureDataObject(object, fct_exp_data)
            }

            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "PortfolioFactorExposureAnalysisBlock"
#' @return \code{object} object object of class "PortfolioFactorExposureAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "PortfolioFactorExposureAnalysisBlock"),
          function(object){

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

            fct_smmry$Factor <- factor(fct_smmry$Factor,
                                       levels = fct_smmry$Factor[order(fct_smmry$TotalExposure)] )

            fct_smmry$Label <- paste(round(fct_smmry$Delta),"%",sep="")

            exprs_smmry <- ggplot(data=fct_smmry, aes_string(x="Factor", fill="Factor" )) +
              geom_bar(aes_string(weight="TotalExposure")) +
              ylab("Exposure") + xlab("Factor") + ggtitle('Factor exposure') +
              geom_text(aes_string(x= "Factor", y="TotalExposure", label = "Label"),size=4) +
              theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
              scale_fill_brewer(palette="Set3")


            object <- .setOutputGGPlotData(object, fct_smmry)
            object <- .setOutputGGPlot(object, exprs_smmry)
            object <- .setOutputFrontendData(object, data.frame(omit = c("TotalExposure")))

            return(object)
          }
)
