#' @include analysis_block.r
NULL

#' Analysis Module for visualisting implied factor state
#'
#' Generates ggplot of implied factor state.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualRiskModelHandler",
#'               "VirtualImpliedFactorReturnsDataHandler"
#'
#' @export

setClass(
  Class             = "ImpliedFactorReturnsStateAnalysisBlock",
  slots             = c(
    implied_factor_returns = "ImpliedFactorReturnsState",
    output                 = "ImpliedFactorReturnsState"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"),
                           c("id", "start", "end")),
    risk_model      = new("RiskModel.DevelopedEuropePrototype150.1.1"),
    implied_factor_returns = new("ImpliedFactorReturnsState")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualRiskModelHandler",
                        "VirtualImpliedFactorReturnsDataHandler"
  )
)


#' Set risk_model object in object slot
#'
#' Public method to set trade_data slot with "VirtualRiskModel"
#' class object
#'
#' @rdname setRiskModelObject-ImpliedFactorReturnsStateAnalysisBlock-method
#' @param object object of class "ImpliedFactorReturnsStateAnalysisBlock"
#' @param risk_model object of class "VirtualRiskModel"
#' @return \code{object} object of class "ImpliedFactorReturnsStateAnalysisBlock"
#' @export

setMethod("setRiskModelObject",
          signature(object = "ImpliedFactorReturnsStateAnalysisBlock",
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
#' @rdname dataRequest-ImpliedFactorReturnsStateAnalysisBlock-method
#' @param object object of class 'ImpliedFactorReturnsStateAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'ImpliedFactorReturnsStateAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "ImpliedFactorReturnsStateAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            start <- min(key_values$start)
            end <- max(key_values$end)
            trader <- unique(key_values$TraderID)
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
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(factor_ret), cond))
            })

            factor_ret <- computeImpliedFactorReturnsState(factor_ret)

            object <- TE.RefClasses:::.setImpliedFactorReturnsDataObject(object, factor_ret)

            return(object)
          }
)

#' Trigger computation of analysis data.
#'
#' @rdname Process-ImpliedFactorReturns-method
#' @param object object of class "ImpliedFactorReturnsAnalysisBlock"
#' @return \code{object} object object of class "ImpliedFactorReturnsAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "ImpliedFactorReturnsStateAnalysisBlock"),
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

            cn <- colnames(factor_data)
            cd_factors <- cn[grep('cmpnd',cn)]
            mv_factors <- cn[grep('mavg',cn)]
            qt_factors <- cn[grep('ftile',cn)]
            factors <- setdiff(cn,c(cd_factors,mv_factors,qt_factors,"Date"))
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
              levels <- c(as.character(sort(unique(factor_data[[paste(fct,"_ftile",sep="")]]))),NA)
              factor_data[[paste(fct,"_ftile",sep="")]] <- unlist(Map(function(x)if(is.na(x)){NA}else{which(x==levels)},as.character(factor_data[[paste(fct,"_ftile",sep="")]])))
              if(first){
                plt_data <- rbind(data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd",sep="")]],RiskType=rt,Factor=fct,Quantity='Cmpd. Return'),
                                  data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_20_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),
                                  #data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_50_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),
                                  data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_ftile",sep="")]],RiskType=rt,Factor=fct,Quantity='Quartile'))
                first <- FALSE
              } else {
                plt_data <- rbind(plt_data,rbind(data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd",sep="")]],RiskType=rt,Factor=fct,Quantity='Cmpd. Return'),
                                                 data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_20_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),
                                                 #data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_cmpnd_50_mavg",sep="")]],RiskType=rt,Factor=fct,Quantity='MAVG'),
                                                 data.frame(Date=factor_data$Date,Value=factor_data[[paste(fct,"_ftile",sep="")]],RiskType=rt,Factor=fct,Quantity='Quartile')))
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



