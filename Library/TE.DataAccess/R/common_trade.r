#' @include functions.r
#' @include common_dataset.r
#' @include common_RAIDdata.r
#' @include common_composite_datasets.r
#' @include global_configs.r
#' @include features_virtual_feature.r
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
#' @slot trade_id      "numeric",
#' @slot leg_start     "Date",
#' @slot leg_end       "NullableDate",
#' @slot long          "logical",
#' @slot value_usd     "numeric",
#' @slot features      "list",
#' @slot daily_data    "DataSet",
#' @slot strategy      "character",
#' @slot trader        "character",
#' @slot instrument    "numeric",
#' @slot consolidation "data.frame",
#' @slot dly_data_pad  "integer",
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
    value_usd    = "numeric",
    features     = "list",
    daily_data   = "DataSet",
    strategy     = "character",
    trader       = "character",
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
#' @param leg_start "Date" date of start of trade leg
#' @param leg_end "NullableDate" date of end of trade leg
#' @param trader "character" integer id if the trader as character
#' @param instrument "integer" ID of the traded instrument
#' @param strategy "character" name of the traded strategy
#' @param long "logical" is Trade Long or Short
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
                   instrument,
                   strategy,
                   long,
                   value_usd,
                   consolidation,
                   status
                   ){
            .Object@order_id      <- order_id
            .Object@leg_start     <- leg_start
            .Object@leg_end       <- leg_end
            .Object@trader        <- trader
            .Object@instrument    <- instrument
            .Object@strategy      <- strategy
            .Object@long          <- long
            .Object@value_usd     <- value_usd
            .Object@consolidation <- consolidation
            .Object@status        <- status

            .Object@trade_id <- murmur3.32(paste(leg_start,instrument,trader,value_usd,strategy,sep=""))

            return(.Object)

          }
)


setGeneric("getTradeID", function(object,feature,date){standardGeneric("getTradeID")})
setMethod("getTradeID","VirtualTrade",
          function(object){
            return(object@trade_id)
          }
)

setGeneric("bindData", function(object,dataset,aliases=NULL,keep_incoming=NULL,joinmode='inner',overlap_data=FALSE){standardGeneric("bindData")})
setMethod("bindData","VirtualTrade",
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
setMethod("insertFeature","VirtualTrade",
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
setMethod("isFeaturePresent","VirtualTrade",
          function(object,feature){
            return(feature %in% names(object@features))
          }
)

setGeneric("getFeatureValue", function(object,feature,date){standardGeneric("getFeatureValue")})
setMethod("getFeatureValue","VirtualTrade",
          function(object,feature,date){
            value <- getOutPut(object@features[[feature]])
            value <- value[object@datekey==date,2]
            return(value)
          }
)

setGeneric("getValueUSD", function(object,date){standardGeneric("getValueUSD")})
setMethod("getValueUSD","VirtualTrade",
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
setMethod("isPsnLong","VirtualTrade",
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

#' Concrete S4 class for storing trade info.
#'
#' Stores information about trade leg together with
#' all necessary "features" that can be attached
#'
#' @export

setClass(
  Class          = "Trade",

  contains = c("VirtualTrade")
)
