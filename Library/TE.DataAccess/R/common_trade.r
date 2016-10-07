#' @include functions.r
#' @include common_dataset.r
#' @include common_RAIDdata.r
#' @include common_composite_datasets.r
#' @include global_configs.r
#' @include features_trade_feature.r
NULL

#Fill position information and populate the return of the trade
#Fill factor information and join to the price dataset
setClass(
  Class          = "TradePriceDataSet",
  prototype      = prototype(
    key_cols     = c("DateTime"),
    data_cols    = c("ClosePrice","OutstandingShares","TodayPL","StopLoss","ProfitTarget")
  ), contains = c("DataSet")
)


#ToDo add other data to the daily data panel,
#first define with a dataset object and then
#create and bind to the daily data dataset.
#Will need to write a fill method for each new
#type to bind.
#Only primary key is DateTime field.

# @exportClass NullableDate
setClassUnion("NullableDate",c('NULL','Date'))

#' An S4 class for storing trade info.
#'
#' Stores information about trade leg together with
#' all necessary "features" that can be attached
#'
#'
#' @slot trade_id      "integer"
#' @slot order_id      "integer"
#' @slot leg_start     "Date"
#' @slot leg_end       "NullableDate"
#' @slot long          "logical"
#' @slot value_usd     "numeric"
#' @slot features      "list"
#' @slot daily_data    "DataSet"
#' @slot strategy      "character"
#' @slot trader        "character"
#' @slot instrument    "integer"
#' @slot consolidation "data.frame"
#' @slot dly_data_pad  "integer"
#' @slot datekey       "character"
#' @slot status        "character"
#'
#' @export

setClass(
  Class          = "VirtualTrade",
  representation = representation(
    trade_id     = "integer",
    order_id     = "integer",
    leg_start    = "Date",
    leg_end      = "NullableDate",
    long         = "logical",
    buysell      = "character",
    value_usd    = "numeric",
    features     = "list",
    daily_data   = "DataSet",
    strategy     = "character",
    trader       = "character",
    trader_id    = "integer",
    instrument   = "integer",
    consolidation= "data.frame",
    dly_data_pad = "integer",
    datekey      = "character",
    status       = "character"
  ),
  prototype      = prototype(
    dly_data_pad = warehouse_defaults@default_dly_data_pad,
    datekey      = warehouse_defaults@default_date_key,
    status       = "Open"
  )
)


#' Initialize method for "VirtualTrade" class
#'
#' @param .Object object of class "VirtualTrade"
#' @param order_id "integer" order ID of the trade
#' @param leg_start "Date" date of start of trade leg
#' @param leg_end "NullableDate" date of end of trade leg
#' @param trader "character" integer id if the trader as character
#' @param instrument "integer" ID of the traded instrument
#' @param strategy "character" name of the traded strategy
#' @param long "logical" is Trade Long or Short
#' @param buysell "character" buy or sell c("Buy", "Sell")
#' @param value_usd "numeric" value of trade in USD
#' @param consolidation "data.frame" with data related to trade leg.
#'        should contain columns : c('TradeDate','ValueUSD','Strategy', 'OrderID')
#' @param status, object of class "VirtualTrade"
#' @return \code{.Object} object of class "VirtualTrade"
setMethod("initialize", "VirtualTrade",
          function(.Object,
                   order_id,
                   leg_start,
                   leg_end,
                   trader,
                   trader_id,
                   instrument,
                   strategy,
                   long,
                   buysell,
                   value_usd,
                   consolidation,
                   status
                   ){

            .Object@order_id      <- order_id
            .Object@leg_start     <- leg_start
            .Object@leg_end       <- leg_end
            .Object@trader        <- trader
            .Object@trader_id     <- trader_id
            .Object@instrument    <- instrument
            .Object@strategy      <- strategy
            .Object@long          <- long
            .Object@buysell       <- buysell
            .Object@value_usd     <- value_usd
            .Object@consolidation <- consolidation
            .Object@status        <- status
            .Object@trade_id <- generateTradeID(.Object,
                                                leg_start,
                                                instrument,
                                                trader,
                                                value_usd,
                                                strategy)

            return(.Object)
          }
)


setGeneric("generateTradeID", function(object,
                                       leg_start ,
                                       instrument,
                                       trader,
                                       value_usd,
                                       strategy){standardGeneric("generateTradeID")})
setMethod("generateTradeID",
          signature(object     = "VirtualTrade",
                    leg_start  = "Date",
                    instrument = "integer",
                    trader     = "character",
                    value_usd  = "numeric",
                    strategy   = "character"
                    ),
          function(object,
                   leg_start ,
                   instrument,
                   trader,
                   value_usd,
                   strategy){

            id <- murmur3.32(paste(leg_start,
                                   instrument,
                                   trader,
                                   value_usd,
                                   strategy,sep=""))


            return(id)
          }
)

setGeneric("mergeTradeConsolidation", function(object,stored_trade){standardGeneric("mergeTradeConsolidation")})
setMethod("mergeTradeConsolidation",
          signature(object = "VirtualTrade",
                    stored_trade = "VirtualTrade"),
          function(object, stored_trade){



            this_cons <- getTradeConsolidation(object)
            stored_cons <- getTradeConsolidation(stored_trade)
            this_leg_start <- getTradeLegStartDate(object)
            stored_leg_start <- getTradeLegStartDate(stored_trade)

            merged_cons <- unique(rbind(this_cons,
                                     stored_cons))

            if (nrow(merged_cons) == 0 && this_leg_start != stored_leg_start){
              merged_cons <- rbind(data.frame(TradeDate = stored_leg_start,
                                              ValueUsd  = getTradeValueUSD(object),
                                              Strategy  = getTradeStrategy(object),
                                              OrderID   = getTradeOrderID(object)),
                                   data.frame(TradeDate = stored_leg_start,
                                              ValueUsd  = getTradeValueUSD(stored_cons),
                                              Strategy  = getTradeStrategy(stored_cons),
                                              OrderID   = getTradeOrderID(stored_cons)))
            }

            stored_leg_end <- getTradeLegEndDate(stored_trade)
            new_leg_end    <- getTradeLegEndDate(object)

            if (isTradeLegOpen(stored_trade) && isTradeLegClosed(object)){
              # need to close stored trade
              if (stored_leg_end <= new_leg_end ){
                merged_leg_status <- getTradeLegStatus(object)
              }
              else {
                message(sprintf("Inconsistent trade consolidation for stored leg."))
                message(sprintf("stored leg was Open until %s but new trade is Closed earlier date %s.",
                                stored_leg_end,
                                new_leg_end))
                stop(sprintf("Inconsistent trade consolidation for stored trade"))

              }

            }
            else if (isTradeLegClosed(stored_trade) && isTradeLegOpen(object)) {
              # need to close stored trade
              if (new_leg_end <=  stored_leg_end){
                merged_leg_status <- getTradeLegStatus(stored_trade)
              }
              else {
                message(sprintf("Inconsistent trade consolidation for stored leg."))
                message(sprintf("stored leg was Closed on %s but new trade is still open on later date %s.",
                                stored_leg_end,
                                new_leg_end))
                stop(sprintf("Inconsistent trade consolidation for stored trade"))

              }
            }
            else if (isTradeLegClosed(stored_trade) && isTradeLegClosed(object)) {
              # need to close stored trade
              if (new_leg_end ==  stored_leg_end){
                merged_leg_status <- getTradeLegStatus(stored_trade)
              }
              else {
                message(sprintf("Inconsistent trade consolidation for stored leg."))
                message(sprintf("stored leg was Closed on %s but new trade is Closed on %s.",
                                stored_leg_end,
                                new_leg_end))
                stop(sprintf("Inconsistent trade consolidation for stored trade"))

              }
            }
            else{
              merged_leg_status <- getTradeLegStatus(stored_trade)
            }

            merged_trade <- .setTradeConsolidation(stored_trade, merged_cons)
            merged_trade <- .setTradeLegStatus(merged_trade, merged_leg_status)

            return(merged_trade)
          }
)

setGeneric("getTradeOrderID", function(object){standardGeneric("getTradeOrderID")})
setMethod("getTradeOrderID",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@order_id)
          }
)

setGeneric(".setTradeOrderID", function(object, order_id){standardGeneric(".setTradeOrderID")})
setMethod(".setTradeOrderID",
          signature(object = "VirtualTrade",
                    order_id = "integer"),
          function(object, order_id){
            object@order_id <- order_id
            return(object)
          }
)


setGeneric("getTradeValueUSD", function(object){standardGeneric("getTradeValueUSD")})
setMethod("getTradeValueUSD",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@value_usd)
          }
)

setGeneric(".setTradeValueUSD", function(object, value_usd){standardGeneric(".setTradeValueUSD")})
setMethod(".setTradeValueUSD",
          signature(object = "VirtualTrade",
                    value_usd = "numeric"),
          function(object, value_usd){
            object@value_usd <- value_usd
            return(object)
          }
)


setGeneric("getTradeStrategy", function(object){standardGeneric("getTradeStrategy")})
setMethod("getTradeStrategy",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@strategy)
          }
)

setGeneric(".setTradeStrategy", function(object, strategy){standardGeneric(".setTradeStrategy")})
setMethod(".setTradeStrategy",
          signature(object = "VirtualTrade",
                    strategy = "character"),
          function(object, strategy){
            object@strategy <- strategy
            return(object@strategy)
          }
)

setMethod(".setTradeStrategy",
          signature(object = "VirtualTrade",
                    strategy = "missing"),
          function(object){
            object@strategy <- character(NA)
            return(object@strategy)
          }
)



setGeneric("getTradeLegStartDate", function(object){standardGeneric("getTradeLegStartDate")})
setMethod("getTradeLegStartDate",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@leg_start)
          }
)

setGeneric(".setTradeLegStartDate", function(object, leg_start){standardGeneric(".setTradeLegStartDate")})
setMethod(".setTradeLegStartDate",
          signature(object = "VirtualTrade",
                    leg_start = "Date"),
          function(object, leg_start){
            object@leg_start <- leg_start
            return(object)
          }
)


setGeneric("getTradeLegEndDate", function(object){standardGeneric("getTradeLegEndDate")})
setMethod("getTradeLegEndDate",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@leg_end)
          }
)

setGeneric(".setTradeLegEndDate", function(object, leg_end){standardGeneric(".setTradeLegEndDate")})
setMethod(".setTradeLegEndDate",
          signature(object = "VirtualTrade",
                    leg_end = "Date"),
          function(object, leg_end){
            object@leg_end <- leg_end
            return(object)
          }
)

setGeneric("isTradeLong", function(object){standardGeneric("isTradeLong")})
setMethod("isTradeLong",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@long)
          }
)


setGeneric("getTradeLegStatus", function(object){standardGeneric("getTradeLegStatus")})
setMethod("getTradeLegStatus",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@status)
          }
)

setGeneric(".setTradeLegStatus", function(object, status){standardGeneric(".setTradeLegStatus")})
setMethod(".setTradeLegStatus",
          signature(object = "VirtualTrade",
                    status = "character"),
          function(object, status){
            object@status <- status
            return(object)
          }
)

setGeneric("isTradeLegOpen", function(object){standardGeneric("isTradeLegOpen")})
setMethod("isTradeLegOpen",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@status == "Open")
          }
)

setGeneric(".setTradeLegOpen", function(object){standardGeneric(".setTradeLegOpen")})
setMethod(".setTradeLegOpen",
          signature(object = "VirtualTrade"),
          function(object){
            object@status <- "Open"
            return(object)
          }
)

setGeneric("isTradeLegClosed", function(object){standardGeneric("isTradeLegClosed")})
setMethod("isTradeLegClosed",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@status == "Closed")
          }
)

setGeneric(".setTradeLegClosed", function(object){standardGeneric(".setTradeLegClosed")})
setMethod(".setTradeLegClosed",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@status == "Closed")
          }
)

setGeneric("getTradeInstrument", function(object){standardGeneric("getTradeInstrument")})
setMethod("getTradeInstrument",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@instrument)
          }
)

setGeneric("getTrader", function(object){standardGeneric("getTrader")})
setMethod("getTrader",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@trader)
          }
)


setGeneric("getTraderID", function(object){standardGeneric("getTraderID")})
setMethod("getTraderID",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@trader_id)
          }
)


setGeneric("getTradeDailyDataPad", function(object){standardGeneric("getTradeDailyDataPad")})
setMethod("getTradeDailyDataPad",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@dly_data_pad)
          }
)

setGeneric("getTradeConsolidation", function(object){standardGeneric("getTradeConsolidation")})
setMethod("getTradeConsolidation",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@consolidation)
          }
)

setGeneric(".setTradeConsolidation", function(object, consolidation){standardGeneric(".setTradeConsolidation")})
setMethod(".setTradeConsolidation",
          signature(object = "VirtualTrade",
                    consolidation = "data.frame"),
          function(object, consolidation){

            if (nrow(consolidation) > 0) {
              object@consolidation <- consolidation
            }
            return(object)
          }
)


setGeneric("getTradeFeaturesList", function(object){standardGeneric("getTradeFeaturesList")})
setMethod("getTradeFeaturesList",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@features)
          }
)

setGeneric(".setTradeFeaturesList", function(object, features){standardGeneric(".setTradeFeaturesList")})
setMethod(".setTradeFeaturesList",
          signature(object = "VirtualTrade",
                    features = "list"),
          function(object, features){
            object@features <- features
            return(object)
          }
)

setGeneric("updateTradeConsolidation", function(object, consolidation){standardGeneric("updateTradeConsolidation")})
setMethod("updateTradeConsolidation",
          signature(object = "VirtualTrade",
                    consolidation = "data.frame"),
          function(object, consolidation){

            if (nrow(consolidation) > 0) {
              # sort consolidation
              consolidation <- consolidation[order(consolidation$TradeDate,
                                                   consolidation$OrderID),]

              object <- .setTradeLegStartDate(object, min(consolidation$TradeDate))
              object <- .setTradeLegEndDate(object, max(consolidation$TradeDate))
              object <- .setTradeOrderID(object, min(consolidation$OrderID))
              object <- .setTradeValueUSD(object, consolidation$ValueUSD[1])

              object <- .setTradeID(generateTradeID(object,
                                                    getTradeLegStartDate(object),
                                                    getTradeInstrument(object),
                                                    getTrader(object),
                                                    getTradeValueUSD(object),
                                                    getTradeStrategy(object)))

              object <- .setTradeConsolidation(object, consolidation)
            }
            return(object)
          }
)

setGeneric("getTradeID", function(object){standardGeneric("getTradeID")})
setMethod("getTradeID",
          signature(object = "VirtualTrade"),
          function(object){
            return(object@trade_id)
          }
)

setGeneric(".setTradeID", function(object, trade_id){standardGeneric(".setTradeID")})
setMethod(".setTradeID",
          signature(object = "VirtualTrade",
                    trade_id = "integer"),
          function(object, trade_id){
            object@trade_id <- trade_id
            return(object)
          }
)

setGeneric("bindData", function(object,dataset,aliases=NULL,keep_incoming=NULL,joinmode='inner',overlap_data=FALSE){standardGeneric("bindData")})
setMethod("bindData",
          signature(object = "VirtualTrade",
                    dataset = "DataSet"),
          function(object,dataset,aliases=NULL,keep_incoming=NULL,joinmode='inner',overlap_data=FALSE){
            if(length(dataset@data)>0){
              start <- object@leg_start - object@dly_data_pad
              if(is.null(object@leg_end)==FALSE)
              {
                end <- object@leg_end + object@dly_data_pad
              }
              else
              {
                end <- object@leg_start + object@dly_data_pad
              }

              dk <- object@datekey
              if(is.null(aliases)==FALSE)
              {
                for(k in dataset@key_cols){
                  if(is.null(aliases[[k]])==FALSE){
                    if(aliases[[k]]==object@datekey)dk<-k
                  }
                }
              }
              datasubset <- dataset@data[(dataset@data[dk][[1]]>start&dataset@data[dk][[1]]<end),]

              if(is.null(keep_incoming)==FALSE)
              {
                datasubset <- datasubset[,keep_incoming]
                dataset@key_cols <- intersect(dataset@key_cols,keep_incoming)
                dataset@data_cols <- intersect(dataset@data_cols,keep_incoming)
              }

              dataset <- setData(dataset,datasubset)
              if(length(object@daily_data@data)==0){
                object@daily_data <- dataset
              }
              else
              {
                icols <- intersect(object@daily_data@data_cols,dataset@data_cols)
                if(length(icols)>0){
                  if(overlap_data){
                    old_data <- object@daily_data@data
                    #overwrite existing data with new data
                    for(col in icols){
                      new_data <- dataset@data
                      new_data <- new_data[!is.na(new_data[col]),]
                      if(nrow(new_data)>0){
                        old_data[[col]] <- as.numeric(as.character(old_data[[col]]))
                        for(r in 1:nrow(new_data)){
                          locs <- old_data[,object@datekey]==new_data[r,dk]
                          if(length(locs)>0){
                            if(!is.na(new_data[r,col]))old_data[locs,col] <- new_data[r,col]
                          }
                        }
                      }
                    }
                    object@daily_data <- setData(object@daily_data,old_data)
                  }
                  else{
                    message("Bind data blocked attempt to add existing data column to trade, use overlap_data flag to overwrite.")
                  }
                  rm_icols <- dataset@data[c(dataset@key_cols,setdiff(dataset@data_cols,icols))]
                  if(ncol(rm_icols)>0){
                    dataset <- resetData(dataset,rm_icols)
                    object@daily_data <- innerJoin(object@daily_data,dataset,dataset@key_cols,aliases=aliases,joinmode=joinmode)
                  }
                }
                else{
                  object@daily_data <- innerJoin(object@daily_data,dataset,dataset@key_cols,aliases=aliases,joinmode=joinmode)
                }
              }
            }
            object@daily_data <- setData(object@daily_data,object@daily_data@data[!is.na(object@daily_data@data[object@datekey]),])
            return(object)
          }
)

setGeneric("insertFeature", function(object,feature){standardGeneric("insertFeature")})
setMethod("insertFeature",
          signature(object = "VirtualTrade",
                    feature = "TradeFeature"),
          function(object,feature){
            nme <- class(feature)[[1]]
            if(nme %in% names(object@features)){
              message(paste("Feature",nme,"already found in",object@trade_id,"replacing..."))
              object@features[[nme]] <- feature
            }
            else{
              fnmes <- c(names(object@features),nme)
              object@features[[length(object@features)+1]] <- feature
              names(object@features) <- fnmes
            }
            return(object)
          }
)

setGeneric("isFeaturePresent", function(object,feature){standardGeneric("isFeaturePresent")})
setMethod("isFeaturePresent",
          signature(object = "VirtualTrade",
                    feature = "character"),
          function(object,feature){
            return(feature %in% names(object@features))
          }
)

setGeneric("getFeatureValue", function(object,feature,date){standardGeneric("getFeatureValue")})
setMethod("getFeatureValue",
          signature(object = "VirtualTrade",
                    feature = "character",
                    date    = "Date"),
          function(object,feature,date){
            value <- getOutPut(object@features[[feature]])
            value <- value[object@datekey==date,2]
            return(value)
          }
)

setGeneric("getValueUSD", function(object,date){standardGeneric("getValueUSD")})
setMethod("getValueUSD",
          signature(object = "VirtualTrade",
                    date   = "Date"),
          function(object,date){
            if(date==object@leg_start){
              value <- object@value_usd
            }
            else{
              value <- sum(object@consolidation[object@consolidation$TradeDate==date,'ValueUSD'],na.rm=TRUE)
            }
            return(value)
          }
)

setGeneric("isPsnLong", function(object,date){standardGeneric("isPsnLong")})
setMethod("isPsnLong",
          signature(object = "VirtualTrade"),
          function(object){
            mv <- sum(c(object@daily_data@data$MarketValue[object@daily_data@data[[object@datekey]]==(object@leg_start-1)],
                        object@daily_data@data$MarketValue[object@daily_data@data[[object@datekey]]==object@leg_start],
                        object@daily_data@data$MarketValue[object@daily_data@data[[object@datekey]]==(object@leg_start+1)]),na.rm=TRUE)
            if(length(mv)>0 && !is.na(mv)){
              if(mv<0){
                is_long <- FALSE
              }
              else if(mv>0){
                is_long <- TRUE
              }
              else{
                if(nrow(object@consolidation)>0){
                  mv <- sum(object@consolidation$MarketValue)
                  if(mv<0){
                    is_long <- FALSE
                  }
                  else if(mv>0){
                    is_long <- TRUE
                  }
                  else{
                    is_long <- object@long
                  }
                }
                else{
                  is_long <- object@long
                }
              }
            }
            else{
              is_long <- object@long
            }
            return(is_long)
          }
)


setClass(
  Class    = "VirtualRemoteStoredTrade",
  contains = c("VirtualTrade")
)

setGeneric("getRemoteTradeStoreKey", function(object){standardGeneric("getRemoteTradeStoreKey")})
setMethod("getRemoteTradeStoreKey",
          signature(object = "VirtualRemoteStoredTrade"),
          function(object){

            key <- data.frame(id         = as.character(object@trader_id),
                              instrument = object@instrument,
                              buysell    = object@buysell,
                              strategy   = object@strategy,
                              leg_start  = object@leg_start,
                              leg_end    = object@leg_end,
                              status     = object@status,
                              stringsAsFactors = FALSE
                              )
            return(key)
          }
)


setGeneric("saveTradeInRemoteStore", function(object){standardGeneric("saveTradeInRemoteStore")})
setMethod("saveTradeInRemoteStore",
          signature(object = "VirtualRemoteStoredTrade"),
          function(object){

            key <- getRemoteTradeStoreKey(object)

            trdstr <- trade_objectstore_factory(key)

            trdstr <- updateTradeStore(trdstr, object, key, TRUE)

            ret <- commitTradeStore(trdstr)

            if (!ret){
              message(sprintf("Trade: %s couldn't be commited.", getTradeID(object)))
            }

            return(ret)
          }
)

#' Initialize method for "VirtualTrade" class
#'
#' @param .Object object of class "VirtualTrade"
#' @param order_id "integer" order ID of the trade
#' @param leg_start "Date" date of start of trade leg
#' @param leg_end "NullableDate" date of end of trade leg
#' @param trader "character" integer id if the trader as character
#' @param instrument "integer" ID of the traded instrument
#' @param strategy "character" name of the traded strategy
#' @param long "logical" is Trade Long or Short
#' @param buysell "character" buy or sell c("Buy", "Sell")
#' @param value_usd "numeric" value of trade in USD
#' @param consolidation "data.frame" with data related to trade leg.
#'        should contain columns : c('TradeDate','ValueUSD','Strategy', 'OrderID')
#' @param status, object of class "VirtualTrade"
#' @return \code{.Object} object of class "VirtualTrade"
setMethod("initialize", "VirtualRemoteStoredTrade",
          function(.Object,
                   order_id,
                   leg_start,
                   leg_end,
                   trader,
                   trader_id,
                   instrument,
                   strategy,
                   long,
                   buysell,
                   value_usd,
                   consolidation,
                   status
          ){

            .Object <- callNextMethod()

            if (TRUE) {
              # generate key to query remote store
              key <- data.frame(id         = as.character(trader_id),
                                instrument = instrument,
                                buysell    = buysell,
                                strategy   = strategy,
                                leg_start  = leg_start,
                                leg_end    = leg_end,
                                status     = status,
                                stringsAsFactors = FALSE
              )

              # query remote store
              trdstr <- trade_objectstore_factory(key)
              query <- getObjectStoreQuery(trdstr)

              is_known <- isTradeStored(query, key)

              if (is_known){
                stored_trd <- queryTradeStore(trdstr, key)

                .Object <- mergeTradeConsolidation(.Object, stored_trd)
              }

            }

            return(.Object)
          }
)

#' Concrete S4 class for storing trade info.
#'
#' Stores information about trade leg together with
#' all necessary "features" that can be attached
#'
#' @export

setClass(
  Class          = "Trade",

  contains = c("VirtualRemoteStoredTrade")
)
