#' @include portfolio_variance_decomposition.r
NULL

################################################################################
#
# PortfolioCoreFactorHedgeExposuresAnalysisBlock Class
#
# Computation block class to pull data required for portfolio factor exposure.
###############################################################################


#' PortfolioCoreFactorHedgeExposuresData Reference Data class.
#'
#' Concrete S4 class storing data of Portfolio factor exposures
#' Generated only by PortfolioCoreFactorHedgeExposuresAnalysisBlock module.
#'
#' Inherits from "VirtualImpliedFactorReturnsData"
#'
#' @export
setClass(
  Class             = "PortfolioCoreFactorHedgeExposuresData",
  prototype         = list(
    required_colnms = c("Date")
  ),
  contains          = c("VirtualImpliedFactorReturnsData")
)


#' Analyser for Portfolio risk factors exposure
#'
#' Computation block class to pull data required for portfolio factor exposure.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPortfolioDataHandler",
#'               "VirtualRiskModelHandler",
#'               "VirtualInstrumentBetasDataHandler"
#'
#' @export
setClass(
  Class             = "PortfolioCoreFactorHedgeExposuresAnalysisBlock",
  slots             = c(
    portfolio              = "Portfolio",
    instrument_betas       = "InstrumentBetasData",
    output                 = "PortfolioCoreFactorHedgeExposuresData"
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
    output          = new("PortfolioCoreFactorHedgeExposuresData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPortfolioDataHandler",
                        "VirtualRiskModelHandler",
                        "VirtualInstrumentBetasDataHandler"
  )
)



#' @describeIn setRiskModelObject
#' Set risk_model object in object slot
#' @inheritParams setRiskModelObject
#'
# ' @rdname setRiskModelObject-PortfolioCoreFactorHedgeExposures-method
# ' @param object object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
# ' @param risk_model object of class "VirtualRiskModel"
# ' @return \code{object} object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
#' @export
setMethod("setRiskModelObject",
          signature(object = "PortfolioCoreFactorHedgeExposuresAnalysisBlock",
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


#' @describeIn setPortfolioDataObject
#' Set portfolio object in object slot
#' @inheritParams setPortfolioDataObject
#'
# ' @rdname setPortfolioDataObject-PortfolioCoreFactorHedgeExposures-method
# ' @param object object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
# ' @param portfolio object of class "StrategyPortfolio"
# ' @return \code{object} object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "PortfolioCoreFactorHedgeExposuresAnalysisBlock", portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)


#' @describeIn setInstrumentBetasDataObject
#' Set portfolio object in object slot
#' @inheritParams setInstrumentBetasDataObject
#'
# ' @rdname setInstrumentBetasDataObject-PortfolioCoreFactorHedgeExposures-method
# ' @param object object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
# ' @param instrument_betas object of class "InstrumentBetasData"
# ' @return \code{object} object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
#' @export
setMethod("setInstrumentBetasDataObject",
          signature(object = "PortfolioCoreFactorHedgeExposuresAnalysisBlock", instrument_betas = "InstrumentBetasData"),
          function(object, instrument_betas){
            object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, instrument_betas)
            return(object)
          }
)

#' @describeIn dataRequest
#'
#' Request data from data source
#'
#' @inheritParams dataRequest
#'
# ' @param object object of class 'PortfolioCoreFactorHedgeExposuresAnalysisBlock'.
# ' @param key_values data.frame with keys specifying data query.
# ' @return \code{object} object of class 'PortfolioCoreFactorHedgeExposuresAnalysisBlock'.
#' @export
setMethod("dataRequest",
          signature(object = "PortfolioCoreFactorHedgeExposuresAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            id <- unique(key_values[,1])[1]
            start <- min(key_values$start)
            end <- max(key_values$end)

            portf_data <- getPortfolioDataObject(object)


            # retrieve portfolio data for query key_values
            if (getStoredNRows(portf_data) == 0) {
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
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(betas_data), cond))
              })

              object <- TE.RefClasses:::.setInstrumentBetasDataObject(object, betas_data)
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
# ' @param object object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
# ' @return \code{object} object object of class "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "PortfolioCoreFactorHedgeExposuresAnalysisBlock"),
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

                market_ret <- portfolio_factor_exposure(wt,bt)

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

            plt_risk <- ggplot(data=ret_plot_data,aes_string(x="Date",
                                                             y="Value",
                                                             color="RiskType",
                                                             linetype = "RiskType")) +
              geom_line(size=1) + ylab("Hedges Daily Factor Exposure") +
              # scale_colour_manual(breaks  = ret_plot_data$RiskType,
              #                     values  = ret_plot_data$)
              #colScale + lineScale +
              #guides(linetype=FALSE) +
              facet_grid(RiskGroup ~. ,scales = "free_y") +
              xlab("Date")


            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, returns_decomposition)
            object <- .setOutputObject(object, outp_object)

            object <- .setOutputGGPlotData(object, ret_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Value", "Colour")))

            return(object)
          }
)


################################################################################
#
# IndexPortfolioCoreFactorHedgeExposuresAnalysisBlock Class
#
# Computation block class to pull data required for portfolio factor exposure.
###############################################################################

#' Analyser for Index Portfolio risk factors exposure
#'
#' Computation block class to pull data required for portfolio factor exposure.
#'
#' Inherits from "PortfolioCoreFactorHedgeExposuresAnalysisBlock"
#'
#' @export
setClass(
  Class             = "IndexPortfolioCoreFactorHedgeExposuresAnalysisBlock",
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
  contains          = c("PortfolioCoreFactorHedgeExposuresAnalysisBlock"
  )
)

#' @describeIn setPortfolioDataObject
#' Set portfolio object in object slot
#' @inheritParams setPortfolioDataObject
#'
# ' @rdname setPortfolioDataObject-IndexPortfolioCoreFactorHedgeExposures-method
# ' @param object object of class "IndexPortfolioCoreFactorHedgeExposuresAnalysisBlock"
# ' @param portfolio object of class "VirtualIndexPortfolio"
# ' @return \code{object} object of class "IndexPortfolioCoreFactorHedgeExposuresAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "IndexPortfolioCoreFactorHedgeExposuresAnalysisBlock", portfolio = "VirtualIndexPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)
