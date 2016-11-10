#' @include analysis_block.r
NULL

#' Analysis Module for visualisting market wide implied factor
#' returns
#'
#' Generates ggplot of implied factor returns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualRiskModelHandler",
#'               "VirtualImpliedFactorReturnsDataHandler"
#'
#' @export

setClass(
  Class             = "ImpliedFactorReturnsAnalysisBlock",
  slots             = c(
    implied_factor_returns = "ImpliedFactorReturnsData",
    output                 = "ImpliedFactorReturnsData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"),
                           c("id", "start", "end")),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150.1.1"),
    implied_factor_returns = new("ImpliedFactorReturnsData"),
    implied_factor_returns = new("ImpliedFactorReturnsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualRiskModelHandler",
                        "VirtualImpliedFactorReturnsDataHandler"
  )
)


#' @describeIn setRiskModelObject
#' Set risk_model object in object slot
#' @inheritParams setRiskModelObject
#'
# ' @rdname setRiskModelObject-ImpliedFactorReturnsAnalysisBlock-method
# ' @param object object of class "ImpliedFactorReturnsAnalysisBlock"
# ' @param risk_model object of class "VirtualRiskModel"
# ' @return \code{object} object of class "ImpliedFactorReturnsAnalysisBlock"
#' @export

setMethod("setRiskModelObject",
          signature(object = "ImpliedFactorReturnsAnalysisBlock",
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

#' Request data from data source
#'
#' @rdname dataRequest-ImpliedFactorReturns-method
#' @param object object of class 'ImpliedFactorReturnsAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'ImpliedFactorReturnsAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "ImpliedFactorReturnsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            start <- min(key_values$start)
            end <- max(key_values$end)
            id <- unique(key_values$id)[1]
            query_keys <- data.frame(Date=seq(ymd(start),ymd(end),by='days'))

            # getting Implied Factor Returns data
            factor_ret <- getImpliedFactorReturnsDataObject(object)
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

            return(object)
          }
)




#' @describeIn Process
#' Trigger computation of analysis data.
#' @inheritParams Process
#'
# ' @rdname Process-ImpliedFactorReturns-method
# ' @param object object of class "ImpliedFactorReturnsAnalysisBlock"
# ' @return \code{object} object object of class "ImpliedFactorReturnsAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "ImpliedFactorReturnsAnalysisBlock"),
          function(object){

            # retrieve data
            factor_ret <- getImpliedFactorReturnsDataObject(object)
            object <- .setOutputObject(object, factor_ret)
            factor_data <- getReferenceData(factor_ret)
            risk_model <- getRiskModelObject(object)
            market_factors    <- getRiskModelMarketFactorNames(risk_model)
            currency_factors  <- getRiskModelCurrencyFactorNames(risk_model)
            commodity_factors <- getRiskModelCommodityFactorNames(risk_model)
            sector_factors    <- getRiskModelSectorFactorNames(risk_model)

            factor_data <- factor_data[order(factor_data$Date),]
            factors <- setdiff(colnames(factor_data),"Date")
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
              factor_data[[fct]] <- factor_data[[fct]] + 1
              factor_data[[fct]][is.na(factor_data[[fct]])] <- 1
              crtn <- exp(cumsum(log(factor_data[[fct]])))
              if(first){
                plt_data <- data.frame(Date=factor_data$Date,Value=crtn,RiskType=rt,Factor=fct)
                first <- FALSE
              } else {
                plt_data <- rbind(plt_data,data.frame(Date=factor_data$Date,Value=crtn,RiskType=rt,Factor=fct))
              }
            }

            object <- .setOutputGGPlotData(object, plt_data)

            plt_risk <- ggplot(data=plt_data,aes_string(x="Date",y="Value",color="Factor")) +
                               geom_line(size=1) +
                               facet_grid(RiskType~.,scales="free_y")

            object <- .setOutputGGPlot(object, plt_risk)

            return(object)
          }
)



