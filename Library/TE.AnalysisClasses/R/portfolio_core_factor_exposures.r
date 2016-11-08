#' @include portfolio_variance_decomposition.r
NULL

################################################################################
#
# PortfolioCoreFactorExposuresAnalysisBlock Class
#
# Computation block class to pull data required for portfolio factor exposure.
###############################################################################


#' PortfolioCoreFactorExposuresData Reference Data class.
#'
#' Concrete S4 class storing data of Portfolio factor exposures
#' Generated only by PortfolioCoreFactorExposuresAnalysisBlock module.
#'
#' Inherits from "VirtualImpliedFactorReturnsData"
#'
#' @export
setClass(
  Class             = "PortfolioCoreFactorExposuresData",
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
  Class             = "PortfolioCoreFactorExposuresAnalysisBlock",
  slots             = c(
    portfolio              = "Portfolio",
    instrument_betas       = "InstrumentBetasData",
    output                 = "PortfolioCoreFactorExposuresData"
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
    output          = new("PortfolioCoreFactorExposuresData")
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
# ' @rdname setRiskModelObject-PortfolioCoreFactorExposuresAnalysisBlock-method
# ' @param object object of class "PortfolioCoreFactorExposuresAnalysisBlock"
# ' @param risk_model object of class "VirtualRiskModel"
# ' @return \code{object} object of class "PortfolioCoreFactorExposuresAnalysisBlock"
#' @export
setMethod("setRiskModelObject",
          signature(object = "PortfolioCoreFactorExposuresAnalysisBlock",
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
# ' @rdname setPortfolioDataObject-PortfolioCoreFactorExposuresAnalysisBlock-method
# ' @param object object of class "PortfolioCoreFactorExposuresAnalysisBlock"
# ' @param portfolio object of class "StrategyPortfolio"
# ' @return \code{object} object of class "PortfolioCoreFactorExposuresAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "PortfolioCoreFactorExposuresAnalysisBlock", portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)


#' @describeIn setInstrumentBetasDataObject
#' Set instrument_betas object in object slot
#' @inheritParams setInstrumentBetasDataObject
#'
# ' @rdname setInstrumentBetasDataObject-PortfolioCoreFactorExposuresAnalysisBlock-method
# ' @param object object of class "PortfolioCoreFactorExposuresAnalysisBlock"
# ' @param instrument_betas object of class "InstrumentBetasData"
# ' @return \code{object} object of class "PortfolioCoreFactorExposuresAnalysisBlock"
#' @export
setMethod("setInstrumentBetasDataObject",
          signature(object = "PortfolioCoreFactorExposuresAnalysisBlock", instrument_betas = "InstrumentBetasData"),
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
# ' @param object object of class 'PortfolioCoreFactorExposuresAnalysisBlock'.
# ' @param key_values data.frame with keys specifying data query.
# ' @return \code{object} object of class 'PortfolioCoreFactorExposuresAnalysisBlock'.
#' @export
setMethod("dataRequest",
          signature(object = "PortfolioCoreFactorExposuresAnalysisBlock", key_values = "data.frame"),
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
# ' @param object object of class "PortfolioCoreFactorExposuresAnalysisBlock"
# ' @return \code{object} object object of class "PortfolioCoreFactorExposuresAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "PortfolioCoreFactorExposuresAnalysisBlock"),
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

            # for( group in names(factor_groups)) {
            #   ret_plot_data$Colour[ret_plot_data$RiskGroup == group] <- as.integer(as.factor(as.character(ret_plot_data$RiskType[ret_plot_data$RiskGroup == group] )))
            # }
            #
            # #Create a custom color scale
            # myColors <- brewer.pal(brewer.pal.info["Set1", "maxcolors"],"Set1")
            # myColors <- c(myColors, brewer.pal(brewer.pal.info["Set2", "maxcolors"],"Set2"))
            # myColors <- c(myColors, brewer.pal(brewer.pal.info["Set3", "maxcolors"],"Set3"))
            # myColors <- unique(myColors)
            # myColors <- myColors[seq(length(unique(ret_plot_data$Colour)))]
            # names(myColors) <- unique(ret_plot_data$Colour)
            #
            # col_pal <- merge(unique(ret_plot_data[c("RiskType", "RiskGroup", "Colour")]), data.frame(Colour = names(myColors), ColorVal = myColors))
            # rownames(col_pal) <- col_pal$RiskType
            #
            # col_pal <- col_pal[order(col_pal$RiskGroup, col_pal$RiskType),]
            #
            # col_vals <- col_pal$ColorVal
            # names(col_vals) <- col_pal$RiskType
            #
            # line_vals <- col_pal$RiskGroup
            # names(line_vals) <- col_pal$RiskType
            #
            # colScale <- scale_colour_manual(name = "RiskType", values = col_vals, breaks = col_pal$RiskType)
            # lineScale <- scale_linetype_manual(name = "RiskType", values = line_vals, breaks = col_pal$RiskType)

            plt_risk <- ggplot(data=ret_plot_data,aes_string(x="Date",
                                                             y="Value",
                                                             color="RiskType",
                                                             linetype = "RiskType")) +
              geom_line(size=1) + ylab("Daily Factor Exposure") +
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
# IndexPortfolioCoreFactorExposuresAnalysisBlock Class
#
# Computation block class to pull data required for portfolio factor exposure.
###############################################################################

#' Analyser for Index Portfolio risk factors exposure
#'
#' Computation block class to pull data required for portfolio factor exposure.
#'
#' Inherits from "PortfolioCoreFactorExposuresAnalysisBlock"
#'
#' @export
setClass(
  Class             = "IndexPortfolioCoreFactorExposuresAnalysisBlock",
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
  contains          = c("PortfolioCoreFactorExposuresAnalysisBlock"
  )
)

#' @describeIn setPortfolioDataObject
#' Set portfolio object in object slot
#' @inheritParams setPortfolioDataObject
#'
# ' @rdname setPortfolioDataObject-IndexPortfolioCoreFactorExposuresAnalysisBlock-method
# ' @param object object of class "IndexPortfolioCoreFactorExposuresAnalysisBlock"
# ' @param portfolio object of class "VirtualIndexPortfolio"
# ' @return \code{object} object of class "IndexPortfolioCoreFactorExposuresAnalysisBlock"
#' @export
setMethod("setPortfolioDataObject",
          signature(object = "IndexPortfolioCoreFactorExposuresAnalysisBlock", portfolio = "VirtualIndexPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)
