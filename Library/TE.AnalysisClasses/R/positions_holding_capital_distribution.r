#' @include offside_positions.r
NULL

################################################################################
#
# PositionsHoldingCapitalDistributionAnalysisBlock Class
#
# Computation block class to pull data required for Computation of position
# holding capital distribution
###############################################################################



#' Analysis Module for computation of capital distribution over time.
#'
#' Computation block class that computes capital age curve.
#' Pools all required data and generates gg plot with capital age curve.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPositionDataHandler"
#' @export

setClass(
  Class             = "PositionsHoldingCapitalDistributionAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionData"
  ),
  prototype         = list(
    required_colnms = c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL',
                        'MarketRelPL','MinDate','MarketValue','PsnAge'),
    position_data   = new("OffsidePositionData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler"
  )
)


#' Set position_data object in object slot
#'
#' Public method to set position_data slot with "OffsidePositionData"
#' class object
#'
#' @rdname setPositionDataObject-PositionsHoldingCapitalDistribution-method
#' @param object object of class "PositionsHoldingCapitalDistributionAnalysisBlock"
#' @param position_data object of class "OffsidePositionData"
#' @return \code{object} object of class "PositionsHoldingCapitalDistributionAnalysisBlock"
#' @export

setMethod("setPositionDataObject",
          signature(object = "PositionsHoldingCapitalDistributionAnalysisBlock", position_data = "OffsidePositionData"),
          function(object, position_data){
            TE.RefClasses:::.setPositionDataObject(object, position_data)
          }
)


#' Request data from data source
#'
#' @rdname dataRequest-PositionsHoldingCapitalDistribution-method
#' @param object object of class 'PositionsHoldingCapitalDistributionAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'PositionsHoldingCapitalDistributionAnalysisBlock'.
#' @export

setMethod("dataRequest",
          signature(object = "PositionsHoldingCapitalDistributionAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)

            req_key_vals <- data.frame(id = trader, start = start, end = end)

            # retrieve position reference data for query key_values
            position_data <- getPositionDataObject(object)

            if (getStoredNRows(position_data) == 0) {

              # using AverageDownTradesAnalysisBlock to retrieve and process input data
              offside.pos.an <- new("OffsidePositionsAnalysisBlock")
              offside.pos.an <- dataRequest(offside.pos.an, key_values)
              offside.pos.an <- Process(offside.pos.an)
              offside.pos.rd <- getOutputObject(offside.pos.an)
              object <- TE.RefClasses:::.setPositionDataObject(object, offside.pos.rd)
            }

            return(object)
          }
)



#' Trigger computation of analysis data.
#'
#' @rdname Process-PositionsHoldingCapitalDistribution-method
#' @param object object of class "PositionsHoldingCapitalDistributionAnalysisBlock"
#' @return \code{object} object object of class "PositionsHoldingCapitalDistributionAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "PositionsHoldingCapitalDistributionAnalysisBlock"),
          function(object){

            pos_data <- getPositionDataObject(object)

            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)

            pl_hd <- unique(history_data[c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','PsnAge')])

            object <- setReferenceData(object, pl_hd)

            pl_hd <- pl_hd[pl_hd$PsnAge<70,]

            pl_hd$Indicator <- 1
            pl_hd$Quarter <- quarter(pl_hd$Date, with_year = TRUE)
            pl_by_age <- aggregate(pl_hd[c('TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','Indicator')],list(Age=pl_hd$PsnAge, Quarter = pl_hd$Quarter),function(x)sum(x,na.rm=TRUE))
            pl_by_age <- merge(pl_by_age,aggregate(pl_hd['MarketValue'],list(Age=pl_hd$PsnAge),function(x)mean(abs(x),na.rm=TRUE)),by='Age')
            pl_by_age <- clean_df_column_data(pl_by_age, c("TodayPL", "PassiveTodayPL", "ActiveTodayPL", "MarketRelPL"))
            pl_by_age[is.na(pl_by_age)] <- 0

            plt_cum_pl_data <- rbind(cbind(Type='Av. Capital distribution',
                                           Quantity=as.character(pl_by_age$Quarter),
                                           data.frame(Age=pl_by_age$Age,
                                                      PL=pl_by_age$MarketValue*(pl_by_age$Indicator/252))))

            cum_tpl_smmry <- ggplot(plt_cum_pl_data,aes_string(x="as.numeric(Age)",
                                                               y="PL",
                                                               group="Quantity",
                                                               colour="Quantity")) +
              geom_line(size=1) +
              ylab("") +
              xlab("Position Age") +
              labs(colour="") +
              ggtitle('Capital age curve') +
              theme(legend.position = "bottom") +
              #theme(text = element_text(size=15)) +
              facet_grid(Type~.,scales="free_y")

            # set processed data as an output

            object <- .setOutputGGPlotData(object, plt_cum_pl_data)
            object <- .setOutputGGPlot(object, cum_tpl_smmry)


            return(object)
          }
)
