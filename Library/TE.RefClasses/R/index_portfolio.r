#' @include portfolio.r
#' @include rodbc_client.r
NULL


####################################
#
# VirtualIndexPortfolio Class
#
####################################


setClass(
  Class     = "IndexPortfolioSQLQuery",
  prototype = list(
    db_name        = RISK_MODEL_DB(),
    db_schema      = "Research",
    key_cols       = c("IndexTicker", "start", "end"),
    key_values     = data.frame(IndexName = character(),
                                start = as.Date(character()),
                                DateEnd = as.Date(character())),
    results_parser = TE.SQLQuery:::convert_column_class,
    arguments    = c("@sIndexTicker", "@dtDateStart", "@dtDateEnd"),
    procedure    = "prMultiFactorRisk_IndexMemberHolding_SelectByTickerStartEndDate"
  ),
  contains  = c("VirtualSQLProcedureCall")
)



#' Virtual S4 class handling IndexPortfolio Data
#'
#'
#' Inherits from
#'  "Portfolio"
#'  "VirtualRODBCClient"
#'
#' @slot index_ticker "character" name of the index ticker as by BBG
#' @export

setClass(
  Class             = "VirtualIndexPortfolio",
  slots             = c(
    index_ticker    = "character"
  ),
  prototype      = list(
    key_cols           = c("IndexTicker", "start", "end"), # query keys column names
    key_values         = data.frame(IndexTicker = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character())),
    values             = c('Date','InstrumentID', 'InstrumentName', 'Weight'), # columns that neeed to be returned from datastore
    column_name_map    = hash::hash(c('dtDate','lInstrumentID', 'sInstrumentName', 'dblWeight'),
                                    c('Date','InstrumentID', 'InstrumentName', 'Weight')),
    required_colnms = c('Date','InstrumentID', 'InstrumentName', 'Weight'),
    non_na_cols     = c('InstrumentID'),
    sql_query       = new("IndexPortfolioSQLQuery")
  ),

  contains = c("Portfolio", "VirtualRODBCClient", "VIRTUAL")
)

#' Returns IndexTicker name
#'
#' @param object object of class 'VirtualIndexPortfolio'.
#' @export

setGeneric("getIndexTicker", function(object){standardGeneric("getIndexTicker")})

#' @describeIn getIndexTicker
#' Returns TraderID
#'
#' @inheritParams getIndexTicker
#' @return \code{index_ticker} integer.
#' @export
setMethod("getIndexTicker",
          signature(object = "VirtualIndexPortfolio"),
          function(object){
            return(object@index_ticker)
          }
)

#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'IndexPortfolio'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualIndexPortfolio'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualIndexPortfolio", key_values = "data.frame"),
          function(object, key_values){

            index_ticker <- getIndexTicker(object)

            if (!all(unique(key_values$IndexTicker) %in% index_ticker) ){
              message(sprintf("Object of class %s is only accepting querries for %s index",
                              class(object), index_ticker))
              stop(sprintf("Invalid query passed to dataRequest() of %s class",
                           class(object)))
            }


            object <- callNextMethod()

            data <- getReferenceData(object)

            # recompute weights for missing instruments

            adj <- aggregate(Weight ~ Date, data = data, function(x){1/sum(x)})

            colnames(adj) <- c("Date", "Weight.Adj")

            data_adj <- merge(data, adj, by = "Date")

            data_adj$Weight <- data_adj$Weight*data_adj$Weight.Adj

            object <- setReferenceData(object, data_adj)

            object <- setStartDate(object,start = min(key_values$start))
            object <- setEndDate(object, end = max(key_values$end))

            return(object)
          }
)


#' Pool data required for portfolio
#'
#' Pools data from data source specific to derived class implementation
#' Also computes portfolio weights if necessary
#'
#' @param object object of class 'VirtualIndexPortfolio'.
#' @param start Date start of data range.
#' @param end Date end of data range.
#' @return \code{object} object of class 'VirtualIndexPortfolio'.
#' @export
setMethod("buildPortfolioHistory",
          signature(object = "VirtualIndexPortfolio"),
          function(object,start,end){

            index_ticker <- getIndexTicker(object)
            object <- setStartDate(object,start = start)
            object <- setEndDate(object, end = end)

            holdings_data <- getReferenceData(object)

            if (nrow(holdings_data) == 0) {
              key_vals <- data.frame(IndexTicker = index_ticker, start = start, end = end)
              object <- dataRequest(object, key_vals)

            }

            return(object)

          }
)

####################################
#
# IndexPortfolio.BE500 Class
#
####################################

#' Concrete S4 class handling BE500 Index Portfolio Data
#'
#' Inherits from "VirtualIndexPortfolio"
#'
#' @export
setClass(
  Class             = "IndexPortfolio.BE500",
  prototype      = list(
    index_ticker    = "BE500 Index"
  ),

  contains = c("VirtualIndexPortfolio")
)
