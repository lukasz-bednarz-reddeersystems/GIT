sourceTo("../lib/urldatareader.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

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
setMethod("initialize", "PositionDiaryURL",
          function(.Object,trader_id,start,end){
            .Object@position_id <- id
            .Object@values <- c(as.character(.Object@position_id))
            .Object <- buildURL(.Object)
            .Object
          }
)

