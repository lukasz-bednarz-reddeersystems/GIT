#' @include analysis_block.r
NULL



#' PortfolioImpliedFactorReturnsStateProbData Reference Data class.
#'
#' Concrete S4 class storing data Factor state probability
#' conditioned on
#'
#' Inherits from "VirtualFactorVarianceData"
#'
#' @export

setClass(
  Class             = "PortfolioImpliedFactorReturnsStateProbData",
  prototype         = list(
    required_colnms = c("Date")
  ),
  contains          = c("VirtualImpliedFactorReturnsData")
)




#' Analysis Module for visualisting and computing
#' implied factor state probability conditioned on loss of
#' portfolio
#'
#' Generates ggplot of implied factor state.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualRiskModelHandler",
#'               "VirtualImpliedFactorReturnsDataHandler"
#'
#' @export

setClass(
  Class             = "PortfolioFactorStateProbCondLossAnalysisBlock",
  slots             = c(
    implied_factor_returns = "ImpliedFactorReturnsState",
    portfolio      = "StrategyPortfolio",
    output                 = "PortfolioImpliedFactorReturnsStateProbData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"),
                           c("id", "start", "end")),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150.1.1"),
    portfolio       = new("StrategyPortfolio"),
    implied_factor_returns = new("ImpliedFactorReturnsState"),
    output          = new("PortfolioImpliedFactorReturnsStateProbData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualRiskModelHandler",
                        "VirtualPortfolioDataHandler",
                        "VirtualImpliedFactorReturnsDataHandler"
  )
)


#' @describeIn setRiskModelObject
#' Set risk_model object in object slot
#' @inheritParams setRiskModelObject
#'
#' @export

setMethod("setRiskModelObject",
          signature(object = "PortfolioFactorStateProbCondLossAnalysisBlock",
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
#' @export

setMethod("setPortfolioDataObject",
          signature(object = "PortfolioFactorStateProbCondLossAnalysisBlock",
                    portfolio = "StrategyPortfolio"),
          function(object, portfolio){
            object <- TE.RefClasses:::.setPortfolioDataObject(object, portfolio)
            return(object)
          }
)


#' @describeIn setImpliedFactorReturnsDataObject
#' Set portfolio object in object slot
#' @inheritParams setImpliedFactorReturnsDataObject
#'
#' @export

setMethod("setImpliedFactorReturnsDataObject",
          signature(object = "PortfolioFactorStateProbCondLossAnalysisBlock",
                    implied_factor_returns = "ImpliedFactorReturnsState"),
          function(object, implied_factor_returns){
            object <- TE.RefClasses:::.setImpliedFactorReturnsDataObject(object, implied_factor_returns)
            return(object)
          }
)


#' @describeIn dataRequest
#' Request data from data source
#' @inheritParams dataRequest
#'
#' @export

setMethod("dataRequest",
          signature(object = "PortfolioFactorStateProbCondLossAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            start <- min(key_values$start)
            end <- max(key_values$end)
            id <- unique(key_values[,1])[1]


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


            query_keys <- data.frame(Date=seq(ymd(start),ymd(end),by='days'))

            # getting Implied Factor Returns data
            factor_ret <- getImpliedFactorReturnsDataObject(object)

            if (getStoredNRows(factor_ret) == 0) {
              risk_model <- getRiskModelObject(object)
              # important step to copy risk_model info
              factor_ret <- setRiskModelObject(factor_ret, risk_model)


              factor_ret <- tryCatch({
                dataRequest(factor_ret, query_keys)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(factor_ret)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_ret), cond))
              })

              object <- TE.RefClasses:::.setImpliedFactorReturnsDataObject(object, factor_ret)
            }

            return(object)
          }
)

#' @describeIn Process
#' Trigger computation of analysis data.
#' @inheritParams Process
#' @export

setMethod("Process",
          signature(object = "PortfolioFactorStateProbCondLossAnalysisBlock"),
          function(object){

            browser()
            # retrieve data
            factor_rd <- getImpliedFactorReturnsDataObject(object)
            factor_data <- getReferenceData(factor_rd)
            factor_cols <- c("Date",
                             grep("state",
                                  colnames(factor_data),
                                  perl = TRUE,
                                  value = TRUE))


            # risk_model <- getRiskModelObject(object)
            # market_factors    <- getRiskModelMarketFactorNames(risk_model)
            # currency_factors  <- getRiskModelCurrencyFactorNames(risk_model)
            # commodity_factors <- getRiskModelCommodityFactorNames(risk_model)
            # sector_factors    <- getRiskModelSectorFactorNames(risk_model)

            portf_rd <- getPortfolioDataObject(object)
            portf <- getReferenceData(portf_rd)

            # compute potfolio daily returns per strategy
            portf_ret <- aggregate(cbind(TodayPL, MarketValue) ~ Date + Strategy, data = portf, sum)
            portf_ret <- portf_ret[portf_ret$MarketValue != 0,]
            portf_ret$Return <- portf_ret$TodayPL/abs(portf_ret$MarketValue)

            # compute loss state (loss is TRUE when Return < 0.5%)
            portf_ret$Loss <- portf_ret$Return < -0.0005

            foo <- function(x){ x$PLoss    = sum(x$Loss)/nrow(x);x}

            portf_ret <- by(portf_ret, portf_ret$Strategy, foo)
            portf_ret <- Reduce(rbind, portf_ret)

            # merge factor states with portfolio returns
            fct_state <- portf_ret[portf_ret$Loss, c("Date", "Strategy", "PLoss")]
            fct_state <- merge(fct_state,factor_data[factor_cols], by = "Date")

            # compute factor state probability given loss
            fct_levels <- levels(fct_state$AUD_state)
            bar <- function(x){ ret <- data.frame(Strategy      = unique(x$Strategy),
                                                  FactorState   = fct_levels)

                                obs <- (sapply(x[factor_cols[-1]], summary)+1)/(nrow(x)+length(fct_levels))

                                ret <- cbind(ret, obs)
            }

            fct_state_prob <- by(fct_state, fct_state$Strategy, bar)
            fct_state_prob <- Reduce(rbind, fct_state_prob)


            cn <- colnames(factor_data)
            factors <- intersect(getRiskModelFactorNames(object),cn)
            first <- TRUE
            for(fct in factors){
              if(fct%in%market_factors){
                rt <- "Market"
              } else if(fct%in%currency_factors){
                rt <- "FX"
              } else if(fct%in%commodity_factors){
                rt <- "Commodity"
              } else if(fct%in%sector_factors){
                rt <- "Sector"
              } else {
                rt <- NA
              }
              #browser()

              if(first){
                plt_data <- rbind(data.frame(Date=factor_data$Date,
                                             Value=factor_data[[paste(fct,"_cmpnd",sep="")]],
                                             RiskType=rt,
                                             Factor=fct,
                                             Quantity='Cmpd. Return'),

                                  data.frame(Date=factor_data$Date,
                                             Value=factor_data[[paste(fct,"_cmpnd_20_mavg",sep="")]],
                                             RiskType=rt,
                                             Factor=fct,
                                             Quantity='MAVG'),
                                  #data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_50_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),

                                  data.frame(Date=factor_data$Date,
                                             Value=factor_data[[paste(fct,"_ftile",sep="")]],
                                             RiskType=rt,
                                             Factor=fct,
                                             Quantity='Quartile'))
                first <- FALSE
              } else {
                plt_data <- tryCatch({
                  rbind(plt_data,rbind(data.frame(Date=factor_data$Date,
                                                  Value=factor_data[[paste(fct,"_cmpnd",sep="")]],
                                                  RiskType=rt,
                                                  Factor=fct,
                                                  Quantity='Cmpd. Return'),

                                       data.frame(Date=factor_data$Date,
                                                  Value=factor_data[[paste(fct,"_cmpnd_20_mavg",sep="")]],
                                                  RiskType=rt,
                                                  Factor=fct,
                                                  Quantity='MAVG'),
                                       #data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_50_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),

                                       data.frame(Date=factor_data$Date,
                                                  Value=factor_data[[paste(fct,"_ftile",sep="")]],
                                                  RiskType=rt,
                                                  Factor=fct,
                                                  Quantity='Quartile')))
                }, error = function(cond){
                  browser()
                  message(sprintf("Error occured when computing plot data for factor %s",
                                  fct))
                })
              }
            }

            object <- .setOutputGGPlotData(object, plt_data)

            plt_risk <- ggplot(data=plt_data,aes_string(x="Date",y="Value",color="Factor")) +
                               geom_line(size=1) +
                               facet_grid(RiskType~Quantity,scales="free_y")

            object <- .setOutputGGPlot(object, plt_risk)

            return(object)
          }
)



