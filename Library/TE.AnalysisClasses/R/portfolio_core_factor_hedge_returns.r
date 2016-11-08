#' @include analysis_block.r
#' @include portfolio_variance_decomposition_functions.r
NULL

################################################################################
#
# PortfolioCoreFactorHedgeReturnsAnalysisBlock Class
#
# Computation block class to pull data required for portfolio returns decomposition
# Pulls data required for computation and adds required columns.
###############################################################################


#' PortfolioCoreFactorHedgeReturnsData Reference Data class.
#'
#' Concrete S4 class storing data Portfolio Returns Decomposition
#' Data. Generated only within PortfolioCoreFactorHedgeReturnsAnalysisBlock
#'
#' Inherits from "VirtualImpliedFactorReturnsData"
#'
#' @export

setClass(
  Class             = "PortfolioCoreFactorHedgeReturnsData",
  prototype         = list(
    required_colnms = c("Date")
  ),
  contains          = c("VirtualImpliedFactorReturnsData")
)


#' Analysis Module for computation of Portfolio Returns Decomposition
#'
#' Computation block class to pull data required for portfolio
#' returns decomposition. Pulls data required for computation
#' and adds required columns. Generates ggplot with returns
#' decomposition to factors.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPortfolioDataHandler",
#'               "VirtualRiskModelHandler",
#'               "VirtualInstrumentBetasDataHandler",
#'               "VirtualImpliedFactorReturnsDataHandler"
#'
#' @export

setClass(
  Class             = "PortfolioCoreFactorHedgeReturnsAnalysisBlock",
  slots             = c(
    portfolio              = "Portfolio",
    instrument_betas       = "InstrumentBetasData",
    implied_factor_returns = "ImpliedFactorReturnsData",
    output                 = "PortfolioCoreFactorHedgeReturnsData"
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
    implied_factor_returns = new("ImpliedFactorReturnsData"),
    output          = new("PortfolioCoreFactorHedgeReturnsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualRiskModelHandler",
                        "VirtualInstrumentBetasDataHandler",
                        "VirtualImpliedFactorReturnsDataHandler"
  )
)


#' Set risk_model object in object slot
#'
#' Public method to set trade_data slot with "VirtualRiskModel"
#' class object
#'
#' @rdname setRiskModelObject-PortfolioCoreFactorHedgeReturns-method
#' @param object object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @param risk_model object of class "VirtualRiskModel"
#' @return \code{object} object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @export

setMethod("setRiskModelObject",
          signature(object = "PortfolioCoreFactorHedgeReturnsAnalysisBlock",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- TE.RiskModel:::.setRiskModelObject(object, risk_model)
            req_factors <- getRiskModelFactorNames(risk_model)
            output_obj <- getOutputObject(object)

            output_obj <- TE.RefClasses:::.setRequiredVariablesNames(output_obj,
                                                                     c("Date",
                                                                       req_factors))
            object <- .setOutputObject(object, output_obj)
            return(object)
          }
)

#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "StrategyPortfolio"
#' class object
#'
#' @rdname setPortfolioDataObject-PortfolioCoreFactorHedgeReturns-method
#' @param object object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @param portfolio object of class "StrategyPortfolio"
#' @return \code{object} object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @export

setMethod("setPortfolioDataObject",
          signature(object = "PortfolioCoreFactorHedgeReturnsAnalysisBlock", portfolio = "StrategyPortfolio"),
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
#' @rdname setInstrumentBetasDataObject-PortfolioCoreFactorHedgeReturns-method
#' @param object object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @param instrument_betas object of class "InstrumentBetasData"
#' @return \code{object} object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @export

setMethod("setInstrumentBetasDataObject",
          signature(object = "PortfolioCoreFactorHedgeReturnsAnalysisBlock", instrument_betas = "InstrumentBetasData"),
          function(object, instrument_betas){
            object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)



#' Request data from data source
#'
#' @rdname dataRequest-PortfolioCoreFactorHedgeReturns-method
#' @param object object of class 'PortfolioCoreFactorHedgeReturnsAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'PortfolioCoreFactorHedgeReturnsAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "PortfolioCoreFactorHedgeReturnsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

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

              object <- TE.RefClasses:::.setPortfolioDataObject(object, portf_data)
            }


            # retrieve risk model instrument betas
            query_keys <- getReferenceData(portf_data)[c("Date", "InstrumentID")]

            # getting Instrument Betas data
            betas_data <- getInstrumentBetasDataObject(object)
            risk_model <- getRiskModelObject(object)
            # important step to copy risk_model info
            betas_data <- setRiskModelObject(betas_data, risk_model)

            if (getStoredNRows(betas_data) == 0) {

              betas_data <- tryCatch({
                dataRequest(betas_data, query_keys)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(betas_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
              })

              object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, betas_data)
            }

            # getting Implied Factor Returns data
            factor_ret <- getImpliedFactorReturnsDataObject(object)
            # important step to copy risk_model info
            factor_ret <- setRiskModelObject(factor_ret, risk_model)

            query_keys <- unique(query_keys["Date"])
            factor_ret <- tryCatch({
              dataRequest(factor_ret, query_keys)

            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_ret)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_ret), cond))
            })

            object <- TE.RefClasses:::.setImpliedFactorReturnsDataObject(object, factor_ret)

            return(object)
          }
)




#' Trigger computation of analysis data.
#'
#' @rdname Process-PortfolioCoreFactorHedgeReturns-method
#' @param object object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @return \code{object} object object of class "PortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "PortfolioCoreFactorHedgeReturnsAnalysisBlock"),
          function(object){


            # retrieve data
            risk_model <- getRiskModelObject(object)
            all_factors <- getRiskModelFactorNames(object)

            core_factors <- c("CHF", "EUR", "JPY",
                              "PriceMomentum12M",
                              "PriceMomentum1M",
                              "Size",
                              "Value",
                              "Volatility")

            names(core_factors) <- core_factors
            factor_groups <- as.list(core_factors)

            factor_df <- Reduce(rbind,
                                lapply(ls(factor_groups),
                                       function(e){data.frame(RiskType = e, FactorName = factor_groups[[e]])}))

            portf_data <- getPortfolioDataObject(object)
            port <- getReferenceData(portf_data)
            strats <- unique(port$Strategy)
            strats <- strats[grepl("HEDGE|HDG", strats)]
            port <- port[port$Strategy %in% strats, ]

            betas_data <- getInstrumentBetasDataObject(object)
            betas <- getReferenceData(betas_data)

            factor_ret <- getImpliedFactorReturnsDataObject(object)
            all_fct_ir <- getReferenceData(factor_ret)

            rm_date_end <- max(port$Date)
            rm_date_start <- min(port$Date)

            # compute output
            first <- TRUE
            for(rm_date in sort(unique(port$Date))){

              rm_date <- as_date(rm_date)

              if(wday(rm_date)!=7&wday(rm_date)!=1){
                bt <- betas[betas$Date==rm_date,setdiff(colnames(betas),'Date')]
                bt[is.na(bt)] <- 0
                wt <- port[port$Date==rm_date,c('InstrumentID','Weight')]
                colnames(wt) <- c('InstrumentID','Weight')
                fct_ir <- all_fct_ir[all_fct_ir$Date==rm_date,setdiff(colnames(all_fct_ir),'Date')]
                if(nrow(fct_ir)>0){
                  # The variance of log returns is equal to the variance of returns upto second order.
                  # 3/5 adjustment factor is derived from 3rd order term of the Taylor series expansion of log(1+x)^2

                  market_ret <- tryCatch({
                    portfolio_returns_decomposition(wt,bt,fct_ir)
                  }, error = function(cond){
                    message(sprintf("Error when calculating portfolio returns  decomposition for day : %s",
                                    rm_date))
                    stop(sprintf("Error when calculating portfolio returns  decomposition for day : %s, error: %s.",
                                 rm_date, cond))
                  })

                  rd <- factor_df

                  rd$Value <- market_ret[as.character(rd$FactorName),]

                  rd <- aggregate(Value ~ RiskType, data = rd, sum)

                  plot_data <- cbind(data.frame(Date = rm_date), rd)

                  rd <- cbind(data.frame(Date = rm_date), t(unstack(rd, Value ~ RiskType)))

                  rd.tot <- cbind(data.frame(Date = rm_date), as.data.frame(t(market_ret)))

                  plot_data$RiskGroup <- plot_data$RiskType
                  levels(plot_data$RiskGroup) <- factor_groups
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

            plt_risk <- ggplot(data=ret_plot_data,aes_string(x="Date",
                                                             y="Value",
                                                             color="RiskType",
                                                             linetype = "RiskType")) +
              geom_line(size=1) + ylab("Hedges Cumulative Daily returns attribution (%)") +
              # scale_colour_manual(breaks  = ret_plot_data$RiskType,
              #                     values  = ret_plot_data$)
              #colScale + lineScale +
              #guides(linetype=FALSE) +
              facet_grid(RiskGroup ~. ,scales = "free_y") +
              xlab("Date")

            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, returns_decomposition.tot)
            object <- .setOutputObject(object, outp_object)

            object <- .setOutputGGPlotData(object, ret_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Value", "Colour")))

            return(object)
          }
)



################################################################################
#
# IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock Class
#
# Computation block class to pull data required for portfolio returns decomposition
# Pulls data required for computation and adds required columns.
###############################################################################

#' Analysis Module for computation of Index Portfolio Returns Decomposition
#'
#' Computation block class to pull data required for portfolio
#' returns decomposition. Pulls data required for computation
#' and adds required columns. Generates ggplot with returns
#' decomposition to factors.
#'
#' Inherits from "IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock"
#'
#' @export

setClass(
  Class             = "IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock",
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
  contains          = c("PortfolioCoreFactorHedgeReturnsAnalysisBlock"
  )
)


#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "VirtualIndexPortfolio"
#' class object
#'
#' @rdname setPortfolioDataObject-IndexPortfolioCoreFactorHedgeReturns-method
#' @param object object of class "IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @param portfolio object of class "VirtualIndexPortfolio"
#' @return \code{object} object of class "IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "IndexPortfolioCoreFactorHedgeReturnsAnalysisBlock", portfolio = "VirtualIndexPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)
