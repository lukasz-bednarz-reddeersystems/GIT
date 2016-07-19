#Fill position information and populate the return of the trade
#Fill factor information and join to the price dataset

sourceTo("../common/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/RAIDdata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/composite_datasets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/virtual_feature.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

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

setClassUnion("NullableDate",c('NULL','Date'))
setClass(
  Class          = "Trade",
  representation = representation(
    trade_id     = "numeric",
    leg_start    = "Date",
    leg_end      = "NullableDate",
    long         = "logical",
    value_usd    = "numeric",
    features     = "list",
    daily_data   = "DataSet",
    strategy     = "character",
    trader       = "character",
    instrument   = "numeric",
    consolidation= "data.frame",
    dly_data_pad = "integer",
    datekey      = "character"
  ),
  prototype      = prototype(
    dly_data_pad = warehouse_defaults@default_dly_data_pad,
    datekey      = warehouse_defaults@default_date_key
  )
)

setGeneric("bindData", function(object,dataset,aliases=NULL,keep_incoming=NULL,joinmode='inner',overlap_data=FALSE){standardGeneric("bindData")})
setMethod("bindData","Trade",
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
setMethod("insertFeature","Trade",
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
setMethod("isFeaturePresent","Trade",
  function(object,feature){
    return(feature %in% names(object@features))
  }
)

setGeneric("getFeatureValue", function(object,feature,date){standardGeneric("getFeatureValue")})
setMethod("getFeatureValue","Trade",
  function(object,feature,date){
    value <- getOutPut(object@features[[feature]])
    value <- value[object@datekey==date,2]
    return(value)
  }
)

setGeneric("getValueUSD", function(object,date){standardGeneric("getValueUSD")})
setMethod("getValueUSD","Trade",
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
setMethod("isPsnLong","Trade",
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
  Class          = "TradeWarehouse",
  representation = representation(
    trades       = "environment",
    instruments  = "numeric",
    features     = "character",
    trader_id    = "integer",
    positions    = "PositionComposite",
    psn_summary  = "DataSet",
    start_date   = "Date",
    end_date     = "Date",
    dly_data_pad = "numeric",
    map          = "list",
    fctr_datstr  = "character"
  ),
  prototype(
    dly_data_pad = warehouse_defaults@default_dly_data_pad,
    map          = list(),
    fctr_datstr  = warehouse_defaults@default_fctr_data_str
  )
)

#Prototype is made only once and then copied with each call to 'new' 
#for objects along the same branch in the class hierachy.
#Must ensure new environments created since they are passed
#by reference.
setMethod("initialize", "TradeWarehouse",
          function(.Object){
            .Object@trades <- new.env(parent = emptyenv())
            .Object
          }
)

setGeneric("fillPositionData", function(object){standardGeneric("fillPositionData")})
setMethod("fillPositionData","TradeWarehouse",
          function(object){
              message("Fetching position data...")
              start_date <- object@start_date
              end_date <- object@end_date
              pad <- object@dly_data_pad
              object@positions <- position_composite_factory(object@trader_id,(start_date-pad),(end_date+pad))
            return(object)
          }
)

clean <- function(x){
  if(length(x)>0){
    if(x==Inf || x==-Inf || is.nan(x) || is.na(x) || x==0){
      rval=NA
    }else{
      rval=x
    }
  }
  else{
    rval = NA
  }
  return(rval)
}
cl_mean <- function(x){
  rval <- mean(unlist(Map(clean,x)),na.rm=TRUE)
  if(is.nan(rval)) rval <- NA
  return(rval)
}
cl_gm_mean <- function(x){
  rval <- gm_mean(unlist(Map(clean,x)),na.rm=TRUE)
  if(is.nan(rval)) rval <- NA
  return(rval)
}
cl_sum <- function(x){
  rval <- sum(unlist(Map(clean,x)),na.rm=TRUE)
  if(is.nan(rval)) rval <- NA
  return(rval)
}
cl_sd <- function(x){
  rval <- sd(unlist(Map(clean,x)),na.rm=TRUE)
  if(is.nan(rval)) rval <- NA
  return(rval)
}

long <- function(strat,mval){
  if(strat %in% warehouse_defaults@short_strats){
    rval <- FALSE
  }
  else if(strat %in% warehouse_defaults@long_strats){
    rval <- TRUE
  } 
  else
  {
    message(paste("Warning: Unrecognised strategy",strat))
    rval <- mval>0 
  }
  return(rval)
}

setGeneric("createPositionSummary", function(object){standardGeneric("createPositionSummary")})
setMethod("createPositionSummary","TradeWarehouse",
          function(object){
            
            message("Building position summary...")
            object <- fillPositionData(object)
            keys <- object@positions@data@data[c('Name','Date')]
            colnames(keys) <- c('Strategy','Date')
            ext_pos_data <- data_request('ext_pos_datastore',keys,c('InstrumentID','Date','Quantity','StrategyID','Strategy','Age'))
            ext_pos_data <- tryCatch({
                dataset_factory(c('Date','StrategyID','InstrumentID'),ext_pos_data@data) 
              },error = function(cond){
                message(paste("DataSet creation failed when generating position summary:",cond))
                stop()
              })
            object@positions@data <- innerJoin(object@positions@data,ext_pos_data,c('Date','StrategyID','InstrumentID'),joinmode='left')
            l <- length(object@positions@data@data$TodayPL)
            
            loop_data <- object@positions@data@data
            loop_data <- unique(loop_data)
            # aggregate Quantity and Market Value
            loop_data <- aggregate(loop_data['Quantity'], loop_data[setdiff(colnames(loop_data), "Quantity")], function(x){sum(x, na.rm = TRUE)})
            
            loop_data <- loop_data[order(loop_data$Date),]
            
            # compute PsnReturn
            strategies <- unique(loop_data$StrategyID)
            loop_data$PsnReturn <- NA
            
            for(strat in strategies) {
              ind_strat <- loop_data$StrategyID == strat
              instruments <- unique(loop_data$InstrumentID[ind_strat])
              
              for (instr in instruments) {
                ind_instr <- which(ind_strat & (loop_data$InstrumentID == instr))
                
                div <- abs(loop_data$MarketValue[ind_instr[1:(length(ind_instr) -1)]])
                div[div == 0.0] <- NA
                
                loop_data$PsnReturn[ind_instr] <- c(NA,loop_data$TodayPL[ind_instr[-1]]/div) + 1
                
              }
            }
              
            object@positions@data@data <- loop_data
            object@positions@data@data_cols <- c(object@positions@data@data_cols,'PsnReturn')
            data_in_interval <- resetData(object@positions@data,subset(object@positions@data@data,(object@positions@data@data$Date>=object@start_date&object@positions@data@data$Date<=object@end_date)))
            avg_frame <- aggregateGroup(data_in_interval,c('Age','TodayPL','MarketValue','Quantity','PsnReturn'),c('StrategyID','Strategy','InstrumentID'),cl_mean)
            sum_frame <- aggregateGroup(data_in_interval,c('Age','TodayPL','MarketValue','Quantity','PsnReturn'),c('StrategyID','Strategy','InstrumentID'),cl_sum)
            gm_frame <- aggregateGroup(data_in_interval,c('Age','TodayPL','MarketValue','Quantity','PsnReturn'),c('StrategyID','Strategy','InstrumentID'),cl_gm_mean)
            disp_frame <- aggregateGroup(data_in_interval,c('Age','TodayPL','MarketValue','Quantity','PsnReturn'),c('StrategyID','Strategy','InstrumentID'),cl_sd)
            aggregate_frame <- avg_frame
            aggregate_frame$PsnReturn <- (aggregate_frame$PsnReturn-1)*10000
            aggregate_frame$PsnRtnVol <- (disp_frame$PsnReturn)*10000
            aggregate_frame$TotalPL <- sum_frame$TodayPL
            aggregate_frame$GM <- (gm_frame$PsnReturn-1)*10000
            aggregate_frame$Long <- unlist(Map(long,aggregate_frame$Strategy,aggregate_frame$MarketValue))
            aggregate_frame$TTLPsnReturn <- (aggregate_frame$TotalPL/abs(aggregate_frame$MarketValue))*10000
            initial_data <- aggregateGroup(data_in_interval,c('MarketValue'),c('StrategyID','Strategy','InstrumentID','Date'),cl_sum)
            dates <- aggregateGroup(data_in_interval,c('Date'),c('StrategyID','Strategy','InstrumentID'),min)
            dates$Date <- as.Date(dates$Date,origin="1970-01-01")
            initial_data <- merge(initial_data,dates,by=c('StrategyID','Strategy','InstrumentID','Date'))
            initial_data <- initial_data[c('StrategyID','Strategy','InstrumentID','MarketValue')]
            names(initial_data)[names(initial_data)=='MarketValue'] <- 'InitialValue'
            aggregate_frame <- merge(aggregate_frame,initial_data,by=c('StrategyID','Strategy','InstrumentID'),all.x=TRUE)
            aggregate_frame$InitialValue[aggregate_frame$InitialValue==0] <- NA
            rtns <- tryCatch({
                      computeInstrumentReturn(object,unique(aggregate_frame[['InstrumentID']]))
                    },error=function(cond){
                      stop(paste("Error when computing stock returns in createPositionSummary:",cond))
                    })
            aggregate_frame <- merge(aggregate_frame,rtns,by=c('InstrumentID'),all.x=TRUE)
            aggregate_frame$RelativeReturn <- aggregate_frame$PsnReturn - aggregate_frame$StockReturn
            colnames(aggregate_frame) <- c('InstrumentID','StrategyID','Strategy','Av.Age','Av.PL','Av.MarketValue','Av.Quantity','Av.PsnReturn','PsnRtnVol','Total.PL','Gm.PsnReturn','Long','PsnReturn','InitialValue','StockReturn','RelativeReturn')
            object@psn_summary <- dataset_factory(c('StrategyID','Strategy','InstrumentID'),aggregate_frame)
            return(object)
          }
)

setGeneric("computeInstrumentReturn", function(object,instruments){standardGeneric("computeInstrumentReturn")})
setMethod("computeInstrumentReturn","TradeWarehouse",
          function(object,instruments){
            message("Computing underlying stock return for warehouse interval ...")
            first <- TRUE
            for(ins in instruments){        
              prc <- getPriceData(object,ins,object@start_date,object@end_date,0)            
              if(first){
                rtns <- data.frame(InstrumentID=ins,StockReturn=prc@data[[nrow(prc@data),'ClosePrice']]/prc@data[[1,'ClosePrice']])
                first <- FALSE
              }
              else{
                rtns <- rbind(rtns,data.frame(InstrumentID=ins,StockReturn=prc@data[[nrow(prc@data),'ClosePrice']]/prc@data[[1,'ClosePrice']]))
              }
            }
            rtns$StockReturn <- (rtns$StockReturn-1)*10000
            return(rtns)
          }
)

setGeneric("fillPositionDataAndSummarise", function(object){standardGeneric("fillPositionDataAndSummarise")})
setMethod("fillPositionDataAndSummarise","TradeWarehouse",
          function(object){
            object <- fillPositionData(object)
            tryCatch({
                object <- createPositionSummary(object)  
              }, error=function(cond){
                stop(paste("Failed to generate position summary:",cond))
              })
           return(object)
          }
)

setGeneric("tradeFactory", function(object,trade_dataset,fill_price=FALSE,fill_positions=FALSE,fill_levels=FALSE){standardGeneric("tradeFactory")})
setMethod("tradeFactory","TradeWarehouse",
          function(object,trade_dataset,fill_price=FALSE,fill_positions=FALSE,fill_levels=FALSE){
            object@start_date <- trade_dataset@start_date
            object@end_date <- trade_dataset@end_date
            object@trader_id <- trade_dataset@trader_id
            if(class(trade_dataset)!="TradeHistoryDataSet"){
              stop("Can only build trades from an instance of TradeHistoryDataSet.")
            }
            object@instruments <- unique(trade_dataset@data$InstrumentID)
            object <- fillPositionDataAndSummarise(object)
            message("Extracting trade objects from dataset...")
            cnt <- 0
            for(instrument in object@instruments){
              pc <- round(100*cnt/length(object@instruments))
              message(paste("*****>> Building ",instrument," ",pc,"% complete. <<*****",sep=""))
              object <- buildTrades(object,trade_dataset@data[trade_dataset@data$InstrumentID==instrument,])
              if(fill_price==TRUE){object<-fillTradeListPrices(object,instrument)}
              if(fill_positions==TRUE){object<-fillTradeListPosns(object,instrument)}
              if(fill_levels==TRUE){object<-fillTradeLevels(object,instrument)}
              cnt <- cnt + 1
              message(paste("*****>> Object memory profile <<*****",sep=""))
              slts <- slotNames(object)
              rp <- sort(sapply(paste("object@",slts,sep=""),function(x){eval(parse(text=paste("object.size(",x,")")))}))
              message(paste(rp,collapse=', '))
            }
            return (object)
          }
)

setGeneric("fillTradeListPosns",function(object,instrument){standardGeneric("fillTradeListPosns")})
setMethod("fillTradeListPosns","TradeWarehouse",
          function(object,instrument){

            alias <- list()
            alias[['Date']] <- 'DateTime'
            all_dtr <- object@positions@data@data
            
            for(i in 1:length(object@trades[[as.character(instrument)]])){
              trd <- object@trades[[as.character(instrument)]][[i]]
              start <- trd@leg_start-trd@dly_data_pad
              end <- trd@leg_end+trd@dly_data_pad
              if(!is.na(trd@strategy)&length(trd@strategy)>0){
                dtr <- all_dtr[all_dtr$Name==trd@strategy,]
              }
              else{
                dtr <- all_dtr
                message(paste("Warning: trade",trd@trade_id,"is not mapped to a strategy, attempting to assign to matching position..."))
                str <- unique(all_dtr[all_dtr$Date==trd@leg_start&all_dtr$InstrumentID==trd@instrument,]$Name)
                if(length(str)==1){
                  message(paste("Found",str))
                  trd@strategy <- str
                }
                else{
                  message(paste("Instrument",trd@instrument,"resolves to multiple positions on",trd@leg_start,"could not assign to strategy."))
                }
              }
              dtr <- dtr[dtr$Date>=start,]
              dtr <- dtr[dtr$Date<=end,]
              dtr <- dtr[dtr$InstrumentID==instrument,]
              psns <- new("DataSet")
              psns@key_cols <- object@positions@data@key_cols
              psns@data_cols <- object@positions@data@data_cols
              psns <- setData(psns,dtr)
              object@trades[[as.character(instrument)]][[i]] <- bindData(trd,psns,aliases=alias,keep_incoming=c('Date','TodayPL','MarketValue'),joinmode='left',overlap_data=TRUE) 
            }
            return(object)
          }
)

setGeneric("getPriceData",function(object,instrument,start_date,end_date,pad){standardGeneric("getPriceData")})
setMethod("getPriceData","TradeWarehouse",
          function(object,instrument,start_date,end_date,pad){
            #Building objects to hold input data on the fly like this is not the best way, and is 
            #historical. All data requests should be handled by central dataplex objects.
            #(see getLevelData)
            query <- new("InstrumentHistoryURL",instrument_ids=instrument,start=start_date-pad,end=end_date+pad)
            prc_data <- new("URLParser",parser_type="XMLToFrame")
            prc_data <- runURLs(prc_data,c(query@url))
            dataset  <- tryCatch({
              extract_entities(getURLData(prc_data,1))
            }, error=function(cond){
              message(paste("Failed to fill price data on",instrument,":",cond))
              d <- new("TradePriceDataSet")
              d <- setData(d,cbind(data.frame(DateTime=as.Date(object@start_date-pad:object@end_date+pad,origin="1970-01-01")),ClosePrice=NA,OutstandingShares=NA,TodayPL=NA,StopLoss=NA,ProfitTarget=NA))
              return(d)
            })
            return(dataset)
          }
)

setGeneric("fillTradeListPrices",function(object,instrument){standardGeneric("fillTradeListPrices")})
setMethod("fillTradeListPrices","TradeWarehouse",
          function(object,instrument){
            message(paste("Attempting to fill price data for instrument",instrument))
            start_date <- object@trades[[as.character(instrument)]][[1]]@leg_start
            end_date <- object@trades[[as.character(instrument)]][[1]]@leg_end
            pad <- object@trades[[as.character(instrument)]][[1]]@dly_data_pad
            dataset <- getPriceData(object,instrument,start_date,end_date,pad)
            if(length(dataset)>0){
              for(i in 1:length(object@trades[[as.character(instrument)]])){
                trd <- object@trades[[as.character(instrument)]][[i]]
                object@trades[[as.character(instrument)]][[i]] <- bindData(trd,dataset,overlap_data=TRUE) 
              }
            }
            return(object)
          }
)

setGeneric("getLevelData",function(object,instrument,start_date,end_date,pad){standardGeneric("getLevelData")})
setMethod("getLevelData","TradeWarehouse",
          function(object,instrument,start_date,end_date,pad){
            lvl_data <- data_request("trade_levels",data.frame(lInstrumentID=instrument,
                                                               dtDateFrom=as.Date((start_date-pad):(end_date+pad),'1970-01-01')),
                                     c("dtDateTo","dblStopLoss","dblProfitTarget","lTraderID","lStrategyID","lPositionLevelTypeID"))
            lvl_data <- lvl_data@data
            lvl_data <- subset(lvl_data,lvl_data$lTraderID==object@trader_id)
            lvl_data <- subset(lvl_data,lvl_data$lPositionLevelTypeID==1)#Get price levels only
            strat_key <- unique(object@psn_summary@data[c('StrategyID','Strategy')])
            colnames(strat_key)[colnames(strat_key)=='StrategyID'] <- 'lStrategyID'
            lvl_data <- merge(lvl_data,strat_key,by='lStrategyID')
            return(lvl_data)
          }
)

setGeneric("fillTradeLevels",function(object,instrument){standardGeneric("fillTradeLevels")})
setMethod("fillTradeLevels","TradeWarehouse",
          function(object,instrument){
            message(paste("Attempting to fill trade level data for instrument",instrument))
            lvl_data <- tryCatch({
                            getLevelData(object,instrument,object@start_date,object@end_date,object@trades[[as.character(instrument)]][[1]]@dly_data_pad)
                        }, error=function(cond){
                            stop(paste("Error when getting trade level data on instrument",instrument,":",cond))
                        })
            for(row in 1:nrow(lvl_data)){
              instrument_level <- lvl_data[row,]
              if(length(instrument_level$lInstrumentID)>0 && !is.na(instrument_level$lInstrumentID)){
                trades <- getInstrumentTrades(object,instrument_level$lInstrumentID)
                for(trd in trades){
                  trade_level <- subset(instrument_level,instrument_level$Strategy == trd@strategy)
                  if(nrow(trade_level) > 0 && !is.na(trade_level$dtDateFrom) && !is.na(trade_level$dtDateTo)){
                    stop_frame <- data.frame(DateTime=as.Date(trade_level$dtDateFrom:trade_level$dtDateTo,'1970-01-01'),StopLoss=trade_level$dblStopLoss,ProfitTarget=trade_level$dblProfitTarget)
                    stop_frame <- dataset_factory(c('DateTime'),stop_frame)
                    trd <- tryCatch({
                                bindData(trd,stop_frame,joinmode='left',overlap_data=TRUE)
                           }, error=function(cond){
                                stop(paste("Error binding trade level data for trade",trd@trade_id,":",cond))
                           })
                    object <- setTrade(object,trd)  
                  }
                }
              }
            }
            return(object)
          }
)

get_trade_dates <- function(trade){
  if(length(trade@consolidation$TradeDate)>0){
    dates <- c(trade@leg_start,trade@consolidation$TradeDate)
  } 
  else{
    dates <- c(trade@leg_start)
  }
  return(dates)
}

setGeneric("blockFill",function(object,features,block_fill=TRUE){standardGeneric("blockFill")})
setMethod("blockFill","TradeWarehouse",
  #Needs to be more intelligently done: Currently chews up memory
  function(object){
    instruments <- listInstruments(object)
    message(paste("Block filling",object@fctr_datstr,"for",length(instruments),"from",object@start_date-object@dly_data_pad,"to",object@end_date+object@dly_data_pad,"..."))
    cnt  <- 0
    for(instrument in instruments){
      keys <- data.frame(lInstrumentID=instrument,dtDateTime=object@start_date)
      keys <- rbind(keys,data.frame(lInstrumentID=instrument,dtDateTime=object@end_date))
      dtr  <- data_request(object@fctr_datstr,keys,c('lInstrumentID'))
      message(paste(round(100*(cnt/length(instruments))),"% complete.",sep=""))
      cnt <- cnt + 1
    }
  }
)

setGeneric("attachFeatures",function(object,features,replace_features=TRUE,block_fill=FALSE){standardGeneric("attachFeatures")})
setMethod("attachFeatures","TradeWarehouse",
  function(object,features,replace_features=TRUE,block_fill=FALSE){
    cnt <- 0
    instruments <- listInstruments(object)
    if(block_fill==TRUE)blockFill(object)
    for(instrument in instruments){
      message(paste("*****>> Attaching features to ",instrument," ",round(100*(cnt/length(instruments))),"% complete. <<*****",sep=""))
      if(!replace_features)message("Retaining existing features, replace_features flag is FALSE.")
      trades <- getInstrumentTrades(object,instrument)
      for(trade_id in names(trades)){
        for(feature in features){ 
          feature_present <- isFeaturePresent(trades[[trade_id]],feature)
          if(feature_present == FALSE || replace_features == TRUE){
            dates <- get_trade_dates(trades[[trade_id]])
            daily_data <- trades[[trade_id]]@daily_data
            strategy <- trades[[trade_id]]@strategy
            f <- new(feature)
            tryCatch({
                  eval(parse(text=paste("f <- update",feature,"(f,dates,instrument,strategy,daily_data)",sep="")))      
                },error=function(cond){
                  message(paste("Failed to update state of",feature,":",cond))
                })
            f <- updateCompute(f)
            f <- tearDownTradeFeature(f)
            trades[[trade_id]] <- insertFeature(trades[[trade_id]],f)            
          }
        }
      }
      object <- setInstrumentTrades(object,trades,instrument)
      object@features <- unique(c(object@features,features))
      cnt <- cnt + 1
    }
  return(object)    
  }
)

setGeneric("resetAllTradeFeatures",function(object,trade_id){standardGeneric("resetAllTradeFeatures")})
setMethod("resetAllTradeFeatures","TradeWarehouse",
          function(object){
            trades <- listTrades(object)
            message(paste("Resetting features for",length(trades),"trades..."))
            for(trade in trades){
              object <- resetTradeFeatures(object,trade)
            }
            return(object)
          }
)

setGeneric("resetTradeFeatures",function(object,trade_id){standardGeneric("resetTradeFeatures")})
setMethod("resetTradeFeatures","TradeWarehouse",
          function(object,trade_id){
            tid <- as.character(trade_id)
            instrument <- object@map[[tid]]['InstrumentID']
            index <- object@map[[tid]]['Index']
            object@trades[[as.character(instrument)]][[index]]@features <- list()
            return (object)                        
          }
)

setGeneric("buildFeatureList",function(object){standardGeneric("buildFeatureList")})
setMethod("buildFeatureList","TradeWarehouse",
          function(object){
            trades <- listTrades(object)
            f <- object@features
            for(trade in trades){
              trd <- getTrade(object,trade)
              f <- unique(c(f,names(trd@features)))
            }
            object@features <- f
            names(object@features) <- f
           return(object)
          }
)

setGeneric("getInstrumentTrades",function(object,instrument){standardGeneric("getInstrumentTrades")})
setMethod("getInstrumentTrades","TradeWarehouse",
          function(object,instrument){
            return (object@trades[[as.character(instrument)]])                        
          }
)

setGeneric("getTrade",function(object,trade_id){standardGeneric("getTrade")})
setMethod("getTrade","TradeWarehouse",
          function(object,trade_id){
            tid <- as.character(trade_id)
            instrument <- object@map[[tid]]['InstrumentID']
            index <- object@map[[tid]]['Index']
            return (object@trades[[as.character(instrument)]][[as.numeric(index)]])                        
          }
)

setGeneric("getPriceSnapshot",function(object,trade_id,window=10,scale_rebase=TRUE,exposure=FALSE){standardGeneric("getPriceSnapshot")})
setMethod("getPriceSnapshot","TradeWarehouse",
          function(object,trade_id,window=10,scale_rebase=TRUE,exposure=FALSE){
            trd   <- getTrade(object,trade_id)
            dates <- get_trade_dates(trd)
            nrows <- nrow(trd@daily_data@data)
            for(date in dates){
              rw    <- which(trd@daily_data@data$DateTime==date)
              if(length(rw)>0){
                if((rw-window)<=0){
                  start     <- 1
                  start_pad <- window-rw
                }
                else{
                  start     <- rw-window
                  start_pad <- 0
                }
                if((rw+window)>nrows){
                  end     <- nrows
                  end_pad <- (rw+window)-nrows
                }
                else{
                  end     <- rw+window
                  end_pad <- 0 
                }
                if(!exposure){
                  raw <- trd@daily_data@data$ClosePrice
                  if(isFeaturePresent(trd,'MidOnEntry')){
                    v <- getFeatureValue(trd,'MidOnEntry',date)
                  }
                  else{
                    v <- mean(raw[(rw-1):rw])
                  }
                  if(length(v)>0&&!is.na(v)){
                    raw[rw] <- v
                  }
                  else{
                    v <- raw[rw]
                  }
                  row <- tryCatch({
                                  as.numeric(c(rep(NA,start_pad),as.numeric(raw[start:end]),rep(NA,end_pad)))
                                }, error = function(cond){
                                  stop(paste("Error creating price snapshot on trade",trade_id,":",cond))
                               })
                }
                else{
                  raw <- trd@daily_data@data$MarketValue
                  if(raw[rw-1]==0||is.na(raw[rw-1])){
                    v <- getValueUSD(trd,date)
                  }
                  else{
                    v <- raw[rw]+((-1)^(1+!trd@long))*getValueUSD(trd,date)
                  }
                  if(length(v)>0&&!is.na(v)){
                    raw[rw] <- v
                  } 
                  else{
                    v <- raw[rw]
                  }
                  row <- tryCatch({
                                  as.numeric(c(rep(NA,start_pad),as.numeric(raw[start:end]/trd@daily_data@data$ClosePrice[start:end]),rep(NA,end_pad)))
                                }, error = function(cond){
                                  stop(paste("Error creating exposure snapshot on trade",trade_id,":",cond))
                               }) 
                }
                if(scale_rebase&&(!exposure)){
                  row <- row-v
                  row <- row/v
                  if(!isPsnLong(trd))row <- -1*row
                }
                else if(scale_rebase&&exposure){
                  row <- row-(v/trd@daily_data@data$ClosePrice[rw])
                  row <- row/abs(v/trd@daily_data@data$ClosePrice[rw])
                  if(!isPsnLong(trd))row <- -1*row
                }
                if(date==dates[1]){
                  snapshot <- as.data.frame(t(as.data.frame(row)))
                  rownames(snapshot) <- c(1)
                  nmes <- unlist(Map(function(x)paste("t",x,sep="_"),-window:window))
                  nmes <- gsub("-","m",nmes) #Minus symbol screws up formulas involving columns names
                  colnames(snapshot) <- nmes
                }
                else{
                  snapshot <- rbind(snapshot,row)
                }
              }
              else{
                stop(paste("No row found for",as.character(date),"on trade",trade_id))
              }
            }
            return(snapshot)
          }
)

setGeneric("getRawPositionData",function(object){standardGeneric("getRawPositionData")})
setMethod("getRawPositionData","TradeWarehouse",
          function(object){
            return(object@positions@data)
          }
)

setGeneric("getRawPositionSummary",function(object){standardGeneric("getRawPositionSummary")})
setMethod("getRawPositionSummary","TradeWarehouse",
          function(object){
            return(object@psn_summary@data)
          }
)

setGeneric("getNumberLegs",function(object,instrument){standardGeneric("getNumberLegs")})
setMethod("getNumberLegs","TradeWarehouse",
          function(object,instrument){
            return(length(object@trades[[as.character(instrument)]]))
          }
)

setGeneric("getLegSpan",function(object,instrument){standardGeneric("getLegSpan")})
setMethod("getLegSpan","TradeWarehouse",
          function(object,instrument){
            start_dates <- unlist(Map(function(x)x@leg_start,object@trades[[as.character(instrument)]]))
            end_dates   <- unlist(Map(function(x)x@leg_end,object@trades[[as.character(instrument)]]))
            return(as.numeric(max(end_dates)-min(start_dates)))
          }
)

setGeneric("getTradeFeatures",function(object,trade_id,features){standardGeneric("getTradeFeatures")})
setMethod("getTradeFeatures","TradeWarehouse",
          function(object,trade_id,features){
            
            message(paste("Collecting features for trade",trade_id))
            trd <- getTrade(object,trade_id)
            features <- trd@features[features]
            rtn_frm <- data.frame(DateTime=get_trade_dates(trd))
            dropped <- c()
            for(feature in features){
              fd <- getOutPut(feature)
              if(length(fd)>0){
                rtn <- tryCatch(
                  {
                    if(nrow(fd[!is.na(fd[,1]),]) > 0){
                      if ("PassThruComputation" %in% colnames(fd)){
                        colnames(fd)[[match("PassThruComputation", colnames(fd))]] <- paste0(class(feature), "PassThruComputation")
                      }
                      unique(merge(rtn_frm,fd,by.x="DateTime", by.y=colnames(fd)[[1]]))
                    } else {
                      message(paste("Feature",class(feature)[[1]],"on trade",trade_id,"contains no data."))
                      feature
                    }
                  }, error = function(cond)
                  {
                    stop(paste("Could not bind data for",class(feature)[[1]],"on trade",trade_id))
                  })
                if(class(rtn)[[1]]==class(feature)[[1]])dropped <- c(dropped,class(rtn)[[1]])
                else if(class(rtn)=="data.frame")rtn_frm <- rtn
                else stop("Unknown feature merge state")
              }
              else{
                dropped <- c(dropped,class(feature)[[1]])
              }
            }
            
            rtn_frm <- rtn_frm[!(duplicated(lapply(rtn_frm, c))&&unlist(lapply(rtn_frm, function(x)class(x)[[1]]=="Date")))]
            err <- tryCatch(
              {
                rtn_frm <- cbind(data.frame(TradeID=trd@trade_id),rtn_frm) 
                FALSE 
              },error = function(cond)
              {
                if(nrow(rtn_frm)==0){
                  message(paste("No feature data found for trade",trd@trade_id,":",cond))
                  return(TRUE)
                }
                else{
                  stop(paste("Could not bind trade id",trd@trade_id,"to feature data"))
                }
              })
            if(err)dropped <- names(Map(class,features))
            tryCatch(
              {
                colnames(rtn_frm) <- c("TradeID","DateTime",setdiff(names(Map(class,features)),dropped))          
              },error = function(cond)
              {
                stop(paste("Could not set feature data columns for trade",trd@trade_id))
              })
            return(rtn_frm)
          }
)

setGeneric("getTradeInformation",function(object,trade_id){standardGeneric("getTradeInformation")})
setMethod("getTradeInformation","TradeWarehouse",
          function(object,trade_id){
            trade <- getTrade(object,trade_id)
            parent <- data.frame(Long=trade@long,
                                 TradeID=trade@trade_id,
                                 Instrument=trade@instrument,
                                 Trader=trade@trader,
                                 TradeDate=trade@leg_start,
                                 ValueUSD=trade@value_usd,
                                 Strategy=trade@strategy)
            if(nrow(trade@consolidation)==0){
              info <- parent
            }
            else
            {
              info <- cbind(data.frame(Trader=trade@trader),trade@consolidation[!is.na(trade@consolidation$TradeDate), ])
              info <- cbind(data.frame(Instrument=trade@instrument),info)
              info <- cbind(data.frame(TradeID=trade@trade_id),info)
              info <- cbind(data.frame(Long=trade@long),info)
              info <- rbind(parent,info)
            }
            return (info)                        
          }
)

setGeneric("getPositionSummary",function(object,trade_id){standardGeneric("getPositionSummary")})
setMethod("getPositionSummary","TradeWarehouse",
          function(object,trade_id){
            trade_info <- getTradeInformation(object,trade_id)
            psn_info <- object@psn_summary@data
            colnames(psn_info)[colnames(psn_info)=="InstrumentID"] <- "Instrument"
            colnames(psn_info)[colnames(psn_info)=="Long"] <- "PsnLong"
            psn_summary <- merge(psn_info,trade_info,by=c('Instrument','Strategy'))
            return(psn_summary)           
          }
)

setGeneric("getFullTradeInformation",function(object,trade_id){standardGeneric("getFullTradeInformation")})
setMethod("getFullTradeInformation","TradeWarehouse",
          function(object,trade_id){
            base <- getTradeInformation(object,trade_id)
            colnames(base)[colnames(base)=='TradeDate'] <- "DateTime"
            trade <- getTrade(object,trade_id)
            info <- merge(x=trade@daily_data@data,y=base,by="DateTime",all.x=TRUE)
            return(info)
          }
)

setGeneric("setInstrumentTrades",function(object,trades,instrument){standardGeneric("setInstrumentTrades")})
setMethod("setInstrumentTrades","TradeWarehouse",
          function(object,trades,instrument){
            object@trades[[as.character(instrument)]] <- trades
            return(object)                        
          }
)

setGeneric("setTrade",function(object,trade){standardGeneric("setTrade")})
setMethod("setTrade","TradeWarehouse",
          function(object,trade){
            if(class(trade)[[1]]!="Trade")stop("Attempt to set a warehouse trade to an object not of type Trade.")
            tid <- as.character(trade@trade_id)
            if(!(tid %in% listTrades(object)))stop(paste("Trade",tid,"not found in warehouse."))
            instrument <- object@map[[tid]]['InstrumentID']
            index <- object@map[[tid]]['Index']
            object@trades[[as.character(instrument)]][[as.numeric(index)]]<-trade
            return(object)
          }
)

setGeneric("listInstruments",function(object){standardGeneric("listInstruments")})
setMethod("listInstruments","TradeWarehouse",
          function(object){
            return (object@instruments)
          }
)

setGeneric("listTrades",function(object){standardGeneric("listTrades")})
setMethod("listTrades","TradeWarehouse",
          function(object){
            return(names(object@map))
          }
)

setGeneric("updateMap",function(object,trade_id,instrument,index){standardGeneric("updateMap")})
setMethod("updateMap","TradeWarehouse",
          function(object,trade_id,instrument,index){
            if(trade_id %in% names(object@map)){
              message(paste("WARNING: Attempting to add duplicate trade",trade_id,"to warehouse map."))
            }
            else{
              val <- c(instrument,index)
              names(val) <- c("InstrumentID","Index")
              new_names <- names(object@map)
              object@map[[length(object@map)+1]] <- val
              new_names[[length(new_names)+1]]   <- trade_id
              names(object@map) <- new_names  
            }
            return(object)
          }
)

setGeneric("buildTrades",function(object,trade_panel){standardGeneric("buildTrades")})
setMethod("buildTrades","TradeWarehouse",
          function(object,trade_panel){
            n_trades <- nrow(trade_panel)
            trades <- list()
            trade_id <- c()
            i <- 1
            cnt <- 1 
            while(i <= n_trades){
              
              leg_start     = trade_panel[i,'TradeDate']
              buysell       = trade_panel[i,'BuySell']
              value_usd     = trade_panel[i,'ValueUSD']
              strategy      = trade_panel[i,'Strategy']
              trader        = trade_panel[i,'Trader']
              instrument    = trade_panel[i,'InstrumentID']
              leg_end_index = find_trade_leg_end_index(i,buysell,trade_panel)
              
              if(leg_end_index > 0)
              {
                consolidation = trade_panel[(i+1):(i+leg_end_index),c('TradeDate','ValueUSD','Strategy')]
                leg_end = trade_panel[(i+leg_end_index),'TradeDate']
              }
              else
              {
                consolidation = data.frame()
                leg_end = leg_start
              }
              tid <- murmur3.32(paste(leg_start,instrument,trader,value_usd,strategy,sep=""))
              object <- updateMap(object,tid,instrument,cnt)
              trade_id <- c(trade_id,tid)
              trades[cnt] <- new("Trade",
                               trade_id = tid,
                               leg_start = leg_start,
                               leg_end = leg_end,
                               long = test_long(buysell),
                               value_usd =value_usd,
                               strategy = strategy,
                               trader = trader,
                               instrument = instrument,
                               consolidation = consolidation)
              
              i <- i+ leg_end_index + 1
              cnt <- cnt + 1
            }
            names(trades) <- trade_id
            object@trades[[as.character(instrument)]] <- trades
            return (object)
          }
)

build_warehouse <- function(trader,start,end){
  trd_url_query <- new("TradeHistoryURL",user_ids=trader,start=start,end=end)
  trd_data <- new("URLParser",parser_type = "XMLToFrame")
  trd_data <- runURLs(trd_data,c(trd_url_query@url))

  trd_dataset <- new("TradeHistoryDataSet",trader_id=trd_url_query@user_ids,start_date=trd_url_query@start,end_date=trd_url_query@end)
  trd_dataset <- setData(trd_dataset,getURLData(trd_data,1))

  warehouse <- new("TradeWarehouse")
  warehouse <- tradeFactory(warehouse,trd_dataset,fill_price=TRUE,fill_positions=TRUE,fill_levels=TRUE)
  return(warehouse)
}

extract_entities <- function(raw_data){
    return_data <- new("TradePriceDataSet")
    entities <- c(return_data@data_cols)
    if(length(raw_data>0)){
      for(i in 1:length(entities)){
        if(i>1){
          data <- unique(merge(data,extract_entity(raw_data,entities[i],return_data@key_cols),by.x=return_data@key_cols,by.y=return_data@key_cols))
        } 
        else {
          data <- unique(extract_entity(raw_data,entities[i],return_data@key_cols))
        }
      }
      return_data <- setData(return_data,data)
    }
    return(return_data)
}

extract_entity <- function(raw_data,entity,data_key){
  entity_name <- unique(raw_data$DataElement[regexpr(paste("*",entity,sep=""),raw_data$DataElement)>-1])
  if(length(entity_name) > 1){
    message(paste("Warning multiple entity matches on",entity))
    entity_name <- entity_name[1]
  }
  else if(length(entity_name)==0){
    message(paste("No entity matches on",entity))
    frame <- data.frame(data_key=as.Date(raw_data[,data_key],format='%d/%m/%Y'),entity=c('NA'))
  }
  else{
    frame <- raw_data[raw_data$DataElement==entity_name,c(data_key,'Value')]
    if(substr(entity_name,start=1,stop=3)=="dbl" || substr(entity_name,start=1,stop=1)=="l")
    {
      frame[,'Value'] <- as.numeric(frame[,'Value'])
    }
    else 
    {
      message(paste("Warning no type conversion for entity name",entity_name))
    }
    frame[,data_key] <- as.Date(frame[,data_key],format='%d/%m/%Y')
  }
  colnames(frame) <- c(data_key,entity)
  return(frame)
}

find_trade_leg_end_index <- function(row,side,transactions){
  data_rows <- nrow(transactions)-row
  leg_end_index <- 0
  if(data_rows > 1)
  {
    same_side <- transactions[(row+1):data_rows,'BuySell'] == side
    for(i in 1:length(same_side)){
      if(same_side[i]==TRUE)
      {
        if (leg_end_index < nrow(transactions)) {
          leg_end_index <- leg_end_index+1
        }
      } 
      else 
      {
        break  
      }
    }
  }
  return(leg_end_index)
}

test_long <- function(x){
  lx = length(x)
  if(lx >0 && x == 'Buy'){
    rval = TRUE
  }else if(lx >0 && x=='Sell'){
    rval = FALSE
  }else{
    message("Error when parsing trade long/short")
    rval = NA
  }
  return (rval)
}
 

