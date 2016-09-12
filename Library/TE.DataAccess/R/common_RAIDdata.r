#' @include urldatareader.r global_configs.r
NULL

setClass(
  Class          = "PositionHistoryURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    user_id      = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@positions_url
  ),contains = c('URLQuery')
)

#' Initialize method for "PositionHistoryURL" class
#'
#' @param .Object, object of class "PositionHistoryURL"
#' @param user_id "integer" trader ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "PositionHistoryURL"

setMethod("initialize", "PositionHistoryURL",
          function(.Object,user_id,start,end){
            .Object@user_id <- user_id
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@user_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "TradeHistoryURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    user_ids     = "integer"
  ),
  prototype      = prototype(
    fields       = c("ids","start","end"),
    root_url     = middleware_urls@trade_hist_url
  ), contains = c('URLQuery')
)

#' Initialize method for "TradeHistoryURL" class
#'
#' @param .Object, object of class "TradeHistoryURL"
#' @param user_ids "integer" trader ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "TradeHistoryURL"

setMethod("initialize", "TradeHistoryURL",
          function(.Object,user_ids,start,end){
            .Object@user_ids <- user_ids
            .Object@start <- start
            .Object@end <- end
            idchr <- paste(as.character(.Object@user_ids),collapse=',')
            .Object@values <- c(idchr,as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)


#Data pulled for each instrument, a list of URLs
#is created if multiple instruments present.
setClass(
  Class          = "InstrumentHistoryURL",
  representation = representation(
    start        = "Date",
    end        = "Date",
    instruments  = "numeric",
    url_list     = "character"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@instr_hist_url
  ), contains = c("URLQuery")
)

#' Initialize method for "InstrumentHistoryURL" class
#'
#' @param .Object, object of class "InstrumentHistoryURL"
#' @param instrument_ids "integer" vector of instrument ID's
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "InstrumentHistoryURL"

setMethod("initialize", "InstrumentHistoryURL",
          function(.Object,instrument_ids,start,end){
            .Object@instruments <- c(instrument_ids)
            .Object@start <- start
            .Object@end <- end
            for(i in 1:length(.Object@instruments))
            {
              idchr <- as.character(.Object@instruments[i])
              .Object@values <- c(idchr,as.character(.Object@start),as.character(.Object@end))
              .Object <- buildURL(.Object)
              .Object@url_list <- .Object@url
            }
            .Object
          }
)

setClass(
  Class          = "StaticHistoryURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    instrument   = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@static_factors1
  ),contains = c('URLQuery')
)

#' Initialize method for "StaticHistoryURL" class
#'
#' @param .Object, object of class "StaticHistoryURL"
#' @param instrument "integer" instrument ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "StaticHistoryURL"

setMethod("initialize", "StaticHistoryURL",
          function(.Object,instrument,start,end){
            .Object@instrument <- instrument
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@user_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "Static2HistoryURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    instrument   = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@static_factors2
  ),contains = c('URLQuery')
)

#' Initialize method for "Static2HistoryURL" class
#'
#' @param .Object, object of class "Static2HistoryURL"
#' @param instrument "integer" instrument ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "Static2HistoryURL"

setMethod("initialize", "Static2HistoryURL",
          function(.Object,instrument,start,end){
            .Object@instrument <- instrument
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@user_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "DynamicHistoryURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    instrument   = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@dynamic_factors
  ),contains = c('URLQuery')
)


#' Initialize method for "DynamicHistoryURL" class
#'
#' @param .Object, object of class "DynamicHistoryURL"
#' @param instrument "integer" instrument ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "DynamicHistoryURL"

setMethod("initialize", "DynamicHistoryURL",
          function(.Object,instrument,start,end){
            .Object@instrument <- instrument
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@user_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "EventsTypesURL",
  representation = representation(
  ),
  prototype      = prototype(
    root_url     = middleware_urls@event_types_url
  ),contains = c('URLQuery')
)


#' Initialize method for "EventsTypesURL" class
#'
#' @param .Object, object of class "EventsTypesURL"
#' @return \code{.Object} object of class "EventsTypesURL"

setMethod("initialize", "EventsTypesURL",
          function(.Object){
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "EventsURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    instrument   = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@events_url
  ),contains = c('URLQuery')
)

#' Initialize method for "EventsURL" class
#'
#' @param .Object, object of class "EventsURL"
#' @param instrument "integer" instrument ID
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "EventsURL"

setMethod("initialize", "EventsURL",
          function(.Object,instrument,start,end){
            .Object@instrument <- instrument
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@user_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "ExtendedPositionDataURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    strategy     = "character"
  ),
  prototype      = prototype(
    fields       = c("strategy","start","end"),
    root_url     = middleware_urls@ext_psns_url
  ),contains = c('URLQuery')
)

#' Initialize method for "ExtendedPositionDataURL" class
#'
#' @param .Object, object of class "ExtendedPositionDataURL"
#' @param strategy "character" strategy name
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "ExtendedPositionDataURL"

setMethod("initialize", "ExtendedPositionDataURL",
          function(.Object,strategy,start,end){
            .Object@strategy <- strategy
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@strategy),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "DealingBookURL",
  representation = representation(
    start        = "Date",
    end          = "Date",
    trader_id    = "integer"
  ),
  prototype      = prototype(
    fields       = c("id","start","end"),
    root_url     = middleware_urls@dealing_url
  ),contains = c('URLQuery')
)

#' Initialize method for "DealingBookURL" class
#'
#' @param .Object, object of class "DealingBookURL"
#' @param trader_id "integer" trader id
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "DealingBookURL"

setMethod("initialize", "DealingBookURL",
          function(.Object,trader_id,start,end){
            .Object@trader_id <- trader_id
            .Object@start <- start
            .Object@end <- end
            .Object@values <- c(as.character(.Object@trader_id),as.character(.Object@start),as.character(.Object@end))
            .Object <- buildURL(.Object)
            .Object
          }
)

setClass(
  Class          = "PositionDiaryURL",
  representation = representation(
    position_id  = "integer"
  ),
  prototype      = prototype(
    fields       = c("id"),
    root_url     = middleware_urls@psn_diary_url
  ),contains = c('URLQuery')
)

#' Initialize method for "PositionDiaryURL" class
#'
#' @param .Object, object of class "PositionDiaryURL"
#' @param trader_id "integer" trader id
#' @param start "Date" start date
#' @param end "Date" end date
#' @return \code{.Object} object of class "PositionDiaryURL"

setMethod("initialize", "PositionDiaryURL",
          function(.Object,trader_id,start,end){
            .Object@position_id <- trader_id
            .Object@values <- c(as.character(.Object@position_id))
            .Object <- buildURL(.Object)
            .Object
          }
)

