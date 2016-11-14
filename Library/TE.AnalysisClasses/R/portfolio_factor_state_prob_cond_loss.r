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

            # retrieve data
            factor_rd <- getImpliedFactorReturnsDataObject(object)
            factor_data <- getReferenceData(factor_rd)
            factor_cols <- c("Date",
                             grep("state",
                                  colnames(factor_data),
                                  perl = TRUE,
                                  value = TRUE))


            portf_rd <- getPortfolioDataObject(object)
            portf <- getReferenceData(portf_rd)


            # cumulative entries for all strategies
            portf_tot <- portf
            portf_tot$Strategy <- "ALL"

            portf <- rbind(portf, portf_tot)

            # compute potfolio daily returns per strategy
            portf_ret <- aggregate(cbind(TodayPL, MarketValue) ~ Date + Strategy, data = portf, sum)
            portf_ret <- portf_ret[portf_ret$MarketValue != 0,]
            portf_ret$Return <- portf_ret$TodayPL/abs(portf_ret$MarketValue)

            bar <- function(x){
              x$Loss <- rollapplyr(x$Return,
                                   3,
                                   function(x){ sum(x) < -0.005},
                                   partial = TRUE)
              x
            }

            portf_ret <- by(portf_ret, portf_ret$Strategy, bar)
            portf_ret <- Reduce(rbind, portf_ret)

            # helper functions to compute probabilities
            # compute loss probability
            foo <- function(x){ data.frame(Strategy = unique(x$Strategy),
                                           PLoss    = sum(x$Loss)/nrow(x))}

            # merge factor states with portfolio returns
            fct_state <- portf_ret[c("Date", "Strategy", "Loss")]
            fct_state <- merge(fct_state,factor_data[factor_cols], by = "Date")

            fct_state_gl <- portf_ret[portf_ret$Loss, c("Date", "Strategy")]
            fct_state_gl <- merge(fct_state_gl,factor_data[factor_cols], by = "Date")

            fct_state_gn <- portf_ret[!portf_ret$Loss, c("Date", "Strategy")]
            fct_state_gn <- merge(fct_state_gn,factor_data[factor_cols], by = "Date")

            # factor levels
            fct_levels <- levels(fct_state$AUD_state)

            # computation dates
            days <- sort(unique(fct_state$Date))
            window <- 65 # three month sliding window
            indxs <- seq(window, length(days))

            first <- TRUE
            for(ii in indxs){

              start <- ii -window +1
              #start <- 1
              end <- ii

              fct_state_loc <- fct_state[fct_state$Date >= days[start] & fct_state$Date <= days[end],]

              strat_ploss <- by(fct_state_loc[c("Strategy", "Loss")], fct_state_loc$Strategy, foo)
              strat_ploss <- Reduce(rbind, strat_ploss)

              # fct_state_gl_loc <- fct_state_gl[fct_state_gl$Date >= days[start] & fct_state_gl$Date <= days[end],]
              # fct_state_gn_loc <- fct_state_gn[fct_state_gn$Date >= days[start] & fct_state_gn$Date <= days[end],]

              # compute factor state probability
              fct_state_prob_all <- by(fct_state_loc,
                                       fct_state_loc$Strategy,
                                       factor_state_probs,
                                       strat_ploss,
                                       fct_levels)

              fct_state_prob_all <- Reduce(rbind, fct_state_prob_all)

              fct_state_prob     <- fct_state_prob_all[fct_state_prob_all$Condition == "None",
                                                       setdiff(colnames(fct_state_prob_all), "Condition")]

              # compute factor state probability given loss
              # fct_state_prob_gl <- by(fct_state_gl_loc,
              #                         fct_state_gl_loc$Strategy, bar)
              # fct_state_prob_gl <- Reduce(rbind, fct_state_prob_gl)
              fct_state_prob_gl     <- fct_state_prob_all[fct_state_prob_all$Condition == "Loss",
                                                          setdiff(colnames(fct_state_prob_all), "Condition")]


              #fct_state_prob_gl <- merge(fct_state_prob["Strategy"])

              # compute factor state probability given loss
              # fct_state_prob_gn <- by(fct_state_gn_loc,
              #                         fct_state_gn_loc$Strategy, bar)
              # fct_state_prob_gn <- Reduce(rbind, fct_state_prob_gn)
              fct_state_prob_gnl     <- fct_state_prob_all[fct_state_prob_all$Condition == "NoLoss",
                                                          setdiff(colnames(fct_state_prob_all), "Condition")]

              # normalize probablities

              #compute probability of loss given state
              state <- fct_state[end,factor_cols[-1]]
              p_loss_gs <- prob_of_loss_given_state(state,
                                                    strat_ploss,
                                                    fct_state_prob,
                                                    fct_state_prob_gl,
                                                    fct_state_prob_gnl)


              if(first){

                first <- FALSE

                plt_data <- cbind(data.frame(Date = days[ii]), p_loss_gs)

              } else {
                plt_data <- tryCatch({

                  rbind(plt_data, cbind(data.frame(Date = days[ii]), p_loss_gs))
                }, error = function(cond){
                  browser()
                  message(sprintf("Error occured when computing plot data for factor %s",
                                  fct))
                })
              }
            }

            object <- .setOutputGGPlotData(object, plt_data)

            plt_data <- plt_data[!is.na(plt_data$PLossGS), ]

            plt_risk <- ggplot(data=plt_data,aes_string(x="Date",y="PLossGS",color="Strategy")) +
                               geom_line(size=1) +
                               facet_grid(Strategy~.,scales="free_y")

            object <- .setOutputGGPlot(object, plt_risk)

            return(object)
          }
)



