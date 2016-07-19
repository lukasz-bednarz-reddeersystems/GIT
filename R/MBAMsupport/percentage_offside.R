setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
library(zoo)
library(ggplot2)
library(RColorBrewer) 
library(plotly)
library(sets)
library(RODBC)

load <- TRUE

traders   <- c(101,11,70)
dates <- c("2015-01-01","2016-05-01","2013-07-01")#,"2011-01-01")

if(!load){
  first <- TRUE
  for(t in traders){
    kf <- function()dated_eighteen_monthly_lookback(t,dates[1])
    trader_data <- tryCatch({
      analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    }, error = function(cond){message(paste("Load error on",t,":",cond))})
    if(length(trader_data)>0){
      if(first){
        history_data <- cbind(Trader=t,trader_data)    
        first <- FALSE
      }
      else{
        history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
      } 
    } else {
      message(paste("Load error on",t,": No rows."))
    }
  }
  exclude <- c('BA_SHEDGE','BA_LHEDGE','BA_HEDGE','DK_SHDG','DK_LHDG','JS_SHEDGE','JS_LHEDGE','FX_ARB')
  history_data <- history_data[!history_data$Strategy%in%exclude,]
  history_data <- history_data[!duplicated(history_data[c('Trader','Instrument','TradeDate','Strategy','MarketValue','MidOnEntry','ValueUSD')]),]
  
  history_data <- market_rel_pl(history_data)
  #saveRDS(history_data,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history.rds")  
} else {
  history_data <- readRDS("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history.rds")
}

#Get bulk price lows data
get_abs_lows <- function(start_date,end_date){
  SQL <- paste("select dtDateTime, lInstrumentID, rDaysSince3MLow, rDaysSince3MHigh, rDaysSince6MLow, rDaysSince6MHigh from tInstrumentMatrixHist where dtDateTime >= '",start_date,"' and dtDateTime <= '",end_date,"'",sep="")
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  sqlQuery(cn,'use Razor')
  rel_lows <- sqlQuery(cn,SQL)
  close(cn)
  return(rel_lows)
}
get_rel_lows <- function(start_date,end_date){
  SQL <- paste("select dtDateTime, lInstrumentID, rDaysSinceRel3MLow, rDaysSinceRel3MHigh, rDaysSinceRel6MLow, rDaysSinceRel6MHigh from tInstrumentMatrixStaticHist where dtDateTime >= '",start_date,"' and dtDateTime <= '",end_date,"'",sep="")
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  sqlQuery(cn,'use Razor')
  rel_lows <- sqlQuery(cn,SQL)
  close(cn)
  return(rel_lows)
}
if(!load){
  lows_data <- get_abs_lows(min(history_data$TradeDate,na.rm=TRUE),max(history_data$TradeDate,na.rm=TRUE))
  lows_data <- merge(lows_data,get_rel_lows(min(history_data$TradeDate,na.rm=TRUE),max(history_data$TradeDate,na.rm=TRUE)),by=c("dtDateTime", "lInstrumentID"))
  colnames(lows_data) <- c('TradeDate','Instrument','DaysSince3MLow','DaysSince3MHigh','DaysSince6MLow','DaysSince6MHigh','DaysSinceRel3MLow','DaysSinceRel3MHigh','DaysSinceRel6MLow','DaysSinceRel6MHigh')
  lows_data$TradeDate <- as.Date(lows_data$TradeDate)  
}

#Prepare data: Compute cumulative/realtive PL since position flat
abslimit <- -0.1
rellimit <- -0.1
if(!load){
  cols <- c('TradeID','Trader','Strategy','Instrument','TradeDate','MinDate','MarketValue','TodayPL','CumulativePL','CumulativeMarketRelPL','Long','ActiveTodayPL','MarketRelPL','PsnAge','EarliestMarketValue')
  first <- TRUE
  history_data$PsnAge <- NA
  for(t in traders){
    trader_strategies <- unique(history_data[history_data$Trader==t,]$Strategy)
    for(strat in trader_strategies){
      trader_instruments <- unique(history_data[history_data$Trader==t&history_data$Strategy==strat,]$Instrument)
      for(ins in trader_instruments){
        hd <- unique(history_data[history_data$Trader==t&history_data$Instrument==ins&history_data$Strategy==strat,cols])
        hd$DaysOffAbs <- NA
        hd$DaysOffRel <- NA
        min_date <- unique(hd$MinDate)
        fd <- count_flats(hd)[[1]]
        hd <- hd[order(hd$TradeDate),]
        if(!is.na(fd$FlatDate)){
          for(f in fd$FlatDate){
            flat_date <- as.Date(f)
            hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$CumulativePL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$TodayPL)  
            hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$CumulativeMarketRelPL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$MarketRelPL)
            hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$PsnAge <- hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$TradeDate - min_date 
            if(nrow(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,])>1){
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue <- hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$MarketValue[2]  
            }
            abs <- (hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$CumulativePL/abs(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue))<abslimit
            abs[is.na(abs)] <- 0
            rel <- (hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$CumulativeMarketRelPL/abs(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue))<rellimit
            rel[is.na(rel)] <- 0
            hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$DaysOffAbs <- cumsum(abs)-1
            hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$DaysOffRel <- cumsum(rel)-1
            min_date <- flat_date
          }
          hd[hd$TradeDate>=min_date,]$CumulativePL <- cumsum(hd[hd$TradeDate>=min_date,]$TodayPL)  
          hd[hd$TradeDate>=min_date,]$CumulativeMarketRelPL <- cumsum(hd[hd$TradeDate>=min_date,]$MarketRelPL)
          hd[hd$TradeDate>=min_date,]$PsnAge <- hd[hd$TradeDate>=min_date,]$TradeDate - min_date 
          if(nrow(hd[hd$TradeDate>=min_date,])>1){
            hd[hd$TradeDate>=min_date,]$EarliestMarketValue <- hd[hd$TradeDate>=min_date,]$MarketValue[2]  
            hd[hd$TradeDate>=min_date,]$EarliestMarketValue[is.na(hd[hd$TradeDate>=min_date,]$EarliestMarketValue)] <- 0
          }
          abs <- (hd[hd$TradeDate>=min_date,]$CumulativePL/abs(hd[hd$TradeDate>=min_date,]$EarliestMarketValue))<abslimit
          abs[is.na(abs)] <- 0
          rel <- (hd[hd$TradeDate>=min_date,]$CumulativeMarketRelPL/abs(hd[hd$TradeDate>=min_date,]$EarliestMarketValue))<rellimit
          rel[is.na(rel)] <- 0
          hd[hd$TradeDate>=min_date,]$DaysOffAbs <- cumsum(abs)-1
          hd[hd$TradeDate>=min_date,]$DaysOffRel <- cumsum(rel)-1
        } else {
          hd$PsnAge <- hd$TradeDate - min_date 
          abs <- (hd$CumulativePL/abs(hd$EarliestMarketValue))<abslimit
          abs[is.na(abs)] <- 0
          rel <- (hd$CumulativeMarketRelPL/abs(hd$EarliestMarketValue))<rellimit
          rel[is.na(rel)] <- 0
          hd$DaysOffAbs <- cumsum(abs)
          hd$DaysOffRel <- cumsum(rel)
        }
        if(first){
          all_hd <- hd
          first <- FALSE
        } else {
          all_hd <- rbind(all_hd,hd)
        }
      }
    }
  }
  all_hd <- merge(all_hd,lows_data,by=c('TradeDate','Instrument'),all.x=TRUE)
  all_hd$DaysOffAbs[all_hd$DaysOffAbs==-1] <- 0 
  all_hd$DaysOffRel[all_hd$DaysOffRel==-1] <- 0 
  all_hd$PsnLong <- all_hd$MarketValue > 0 
  all_hd$DaysSince3MLow[is.na(all_hd$DaysSince3MLow)] <- 5000
  all_hd$DaysSince6MLow[is.na(all_hd$DaysSince6MLow)] <- 5000
  all_hd$DaysSince3MHigh[is.na(all_hd$DaysSince3MHigh)] <- 5000
  all_hd$DaysSince6MHigh[is.na(all_hd$DaysSince6MHigh)] <- 5000
  all_hd$DaysSinceRel3MLow[is.na(all_hd$DaysSinceRel3MLow)] <- 5000
  all_hd$DaysSinceRel6MLow[is.na(all_hd$DaysSinceRel6MLow)] <- 5000
  all_hd$DaysSinceRel3MHigh[is.na(all_hd$DaysSinceRel3MHigh)] <- 5000
  all_hd$DaysSinceRel6MHigh[is.na(all_hd$DaysSinceRel6MHigh)] <- 5000
  #saveRDS(all_hd,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history_offside.rds")    
} else {
  all_hd <- readRDS("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history_offside.rds")
}

#Bucket OffSideBucket is offside conditiona without the making lows condition
all_hd$OffSideBucket <- FALSE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30,]$OffSideBucket <- TRUE
#Bucket Lows3MOffSideBucket is offside condition with the making 3M lows condition
all_hd$Lows3MOffSideBucket <- FALSE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSince3MLow<10&all_hd$PsnLong,]$Lows3MOffSideBucket <- TRUE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSince3MHigh<10&!all_hd$PsnLong,]$Lows3MOffSideBucket <- TRUE
#Bucket LowsRel3MOffSideBucket is offside condition with the making 3M relative lows condition
all_hd$LowsRel3MOffSideBucket <- FALSE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel3MLow<10&all_hd$PsnLong,]$LowsRel3MOffSideBucket <- TRUE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel3MHigh<10&!all_hd$PsnLong,]$LowsRel3MOffSideBucket <- TRUE
#Bucket Lows6MOffSideBucket is offside condition with the making 6M relative lows condition
all_hd$Lows6MOffSideBucket <- FALSE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel6MLow<10&all_hd$PsnLong,]$Lows6MOffSideBucket <- TRUE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel6MHigh<10&!all_hd$PsnLong,]$Lows6MOffSideBucket <- TRUE
#Bucket LowsRel6MOffSideBucket is offside condition with the making 6M relative lows condition
all_hd$LowsRel6MOffSideBucket <- FALSE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel6MLow<10&all_hd$PsnLong,]$LowsRel6MOffSideBucket <- TRUE
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSinceRel6MHigh<10&!all_hd$PsnLong,]$LowsRel6MOffSideBucket <- TRUE

#Timeseries of absolute/relative offside
side_timeseries <- function(all_hd,side_type)
{
    tseries_Total <- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
    tseries<- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate,Bucket=all_hd[[side_type]]),function(x)sum(abs(x),na.rm=TRUE))
    tseries<- merge(tseries,tseries_Total,by='TradeDate')
    tseries$PcntOff <- 100*(tseries$x.x/tseries$x.y)
    colnames(tseries) <- c('TradeDate','Bucket','dlr_Offside','dlr_Total','pct_Offside')
    plt_data <- rbind(data.frame(Type='% $ Offside',Date=tseries$TradeDate,Bucket=tseries$Bucket,Value=tseries$pct_Offside),
                      data.frame(Type='$ Invested',Date=tseries$TradeDate,Bucket=NA,Value=tseries$dlr_Total))
    return(plt_data)
}

#$ value expected to cross boundary
stats_in_window <- function(tseries,lookback=20){
  mn <- as.list(rep(NA,lookback-1))
  st <- as.list(rep(NA,lookback-1))
  if(nrow(tseries)>lookback){
    for(i in lookback:nrow(tseries)){
      jumps <- log(1+diff(tseries$Value[(i-lookback+1):i])/100)
      mn[[i]] <- mean(jumps,na.rm=TRUE)
      st[[i]] <- sd(jumps,na.rm=TRUE)
    } 
    tseries$LogJumpMean  <- mn
    tseries$LogJumpStDev <- st
  }
  return(tseries)
}

#Probability of crossing boundary given location and time horizon
pmass_over_limit <- function(b,t,m=mean(jumps$Diff,na.rm=TRUE),st=sd(jumps$Diff,na.rm=TRUE)){
  st <- st*sqrt(t)
  rval <- tryCatch({pnorm(b, mean=m, sd=st, lower.tail=FALSE)},
                    error=function(cond){message(paste("pnorm failed:",cond))
                    return(NA)})
  return(rval)
}
cross_prob <- function(tseries_stats,level,horizon,total_inv,nominal=8,offlevel=abs(abslimit)){
  p <- list()
  for(r in 1:nrow(tseries_stats)){
    p[[r]] <- pmass_over_limit(log(1+0.01*(level-tseries_stats$Value[[r]])),horizon,tseries_stats$LogJumpMean[[r]],tseries_stats$LogJumpStDev[[r]])  
  }
  tseries_stats$Prob <- as.numeric(p)
  tseries_stats <- merge(tseries_stats,total_inv,by='Date')
  tseries_stats$MinLoss <- (level/100)*tseries_stats$MarketValue*(nominal/100)*offlevel
  return(tseries_stats)
}
plot_cross_density <- function(bdary=5,thorizon=100){
  #bdary is the percent invested bucket limit, thorizon is the horizon in days
  pd <- expand.grid(b=0.01*(0:bdary),t=1:thorizon)
  pd$z <- with(pd,pmass_over_limit(b,t))*100
  brks <- cut(pd$z,breaks=seq(0,50,len=10))
  brks <- gsub(","," - ",brks,fixed=TRUE)
  pd$brks <- gsub("\\(|\\]","",brks)
  pd$b <- exp(pd$b)-1
  plt <- ggplot(pd,aes(b,t,z=z)) + 
            geom_raster(aes(fill = z)) +
            geom_contour(colour = "white")  
  return(plt)
}

#Create data for the bucket given a frame of position data
prepare_bucket_data <- function(all_hd,target_bucket,limits){
  tseries <- side_timeseries(all_hd,target_bucket)
  tseries <- tseries[(tseries$Type=='% $ Offside')&(tseries$Bucket),c('Date','Value')]
  tseries_stats <- stats_in_window(tseries)
  total <- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
  colnames(total) <- c('Date','MarketValue')
  first <- TRUE
  #limits is a list of 2-tuples holding the limit in elem 1 and the horizon in elem 2
  for(l in limits){
    lim <- as.numeric(l[1])
    hor <- as.numeric(l[2])
    nme <- paste('limit_',lim,'_horizon_',hor,sep="")
    lim_cross <- cross_prob(tseries_stats,lim,hor,total)
    if(first){
      losses <- cbind(Bucket=target_bucket,Limit=nme,lim_cross)
      first <- FALSE
    } else {
      losses <- tryCatch({rbind(losses,cbind(Bucket=target_bucket,Limit=nme,lim_cross))},
                          error=function(cond){
                            message(paste("prepare rbind failed:",target_bucket,nme))
                            return(losses)
                        })
    }
  }
  return(losses)
}
first <- TRUE
bkts <- list(list('OffSideBucket',list(pair(8,5),pair(10,5),pair(12,5))),
             list('Lows3MOffSideBucket',list(pair(4,5),pair(5,5),pair(6,5))),
             list('Lows6MOffSideBucket',list(pair(4,5),pair(5,5),pair(6,5))),
             list('LowsRel3MOffSideBucket',list(pair(4,5),pair(5,5),pair(6,5))),
             list('LowsRel6MOffSideBucket',list(pair(4,5),pair(5,5),pair(6,5))))
for(trdr in list(pair('All',NA),pair('JS',11),pair('BA',70),pair('DK',101))){
  if(is.na(as.numeric(trdr[2]))){
    dtr <- all_hd
  } else {
    dtr <- all_hd[all_hd$Trader==as.numeric(trdr[2]),]
  }
  for(b in 1:length(bkts)){
    bucket <- bkts[[b]]
    if(first){
      all_bucket_data <- cbind(Trader=as.character(trdr[1]),prepare_bucket_data(dtr,bucket[[1]],bucket[[2]]))
      first <- FALSE
    } else {
      all_bucket_data <- tryCatch({rbind(all_bucket_data,cbind(Trader=as.character(trdr[1]),prepare_bucket_data(dtr,bucket[[1]],bucket[[2]])))},
                                  error=function(cond){
                                    message(paste("plot rbind failed:",bucket[[1]],trdr[1]))
                                    return(all_bucket_data)
                                  }) 
    }
  }
}
all_bucket_data$BucketLabel <- as.character(NA)
all_bucket_data$BucketLabel[all_bucket_data$Bucket=='OffSideBucket'] <- 'Offside'
all_bucket_data$BucketLabel[all_bucket_data$Bucket=='Lows3MOffSideBucket'] <- '+3M lows'
all_bucket_data$BucketLabel[all_bucket_data$Bucket=='Lows6MOffSideBucket'] <- '+6M lows'
all_bucket_data$BucketLabel[all_bucket_data$Bucket=='LowsRel3MOffSideBucket'] <- '+3M rel lows'
all_bucket_data$BucketLabel[all_bucket_data$Bucket=='LowsRel6MOffSideBucket'] <- '+6M rel lows'
all_bucket_data$TraderID <- NA
all_bucket_data$TraderID[all_bucket_data$Trader=='All'] <- 0
all_bucket_data$TraderID[all_bucket_data$Trader=='JS'] <- 11
all_bucket_data$TraderID[all_bucket_data$Trader=='BA'] <- 70
all_bucket_data$TraderID[all_bucket_data$Trader=='DK'] <- 101

plot_trader_buckets <- function(data,traders){
  sub_data <- data[data$Trader%in%traders,]
  plt <- ggplot(data=sub_data,aes(x=Date,y=Value,group=Limit,colour=Limit)) +
                geom_line(size=1) +
                ylab("% Invested in bucket") + guides(colour=FALSE) +  
                facet_grid(BucketLabel~Trader,scales="free_y")
  return(plt)
}
plot_crossing_prob <- function(data,traders){
  sub_data <- data[data$Trader%in%traders,]
  plt <- ggplot(data=sub_data,aes(x=Date,y=Prob,group=Limit,colour=Limit)) +
    geom_line(size=1) +
    ylab("Prob. limit breach") + theme(legend.position="bottom") +
    facet_grid(BucketLabel~Trader,scales="free_y")
  return(plt)
}

bucket_pl <- function(all_hd,history_data,all_bucket_data,traders=c(70,101,11),level=0.05){
  buckets <- unique(all_bucket_data$Bucket)
  sub_data <- all_hd[all_hd$Trader%in%traders,]
  first <-  TRUE
  for(b in buckets){
    if(first){
      df <- merge(sub_data[sub_data[[b]],c('TradeDate','Instrument','Trader','Strategy','TodayPL')],all_bucket_data[all_bucket_data$Bucket==b,c('TraderID','Trader','Limit','Date','Prob','BucketLabel')],by.x=c('TradeDate','Trader'),by.y=c('Date','TraderID'))
      colnames(df) <- c('Date','TraderID','Instrument','Strategy','TodayPL','Trader','Limit','Prob','BucketLabel')
      dates_cross <- df[c('Date','BucketLabel','Limit','Prob')]
      dates_cross$CrossDate <- dates_cross$Date
      dates_cross$CrossDate[dates_cross$Prob>=0.05] <- NA
      dates_cross <- dates_cross[c('Date','BucketLabel','Limit','CrossDate')]
      colnames(dates_cross) <- c('Date','Bucket','Limit','CrossDate')
      df <- df[order(df$Date),]
      if(nrow(df)>0){
        df$TodayPL[is.na(df$TodayPL)] <- 0
        df$TodayPL[df$Prob<level] <- 0
        df <- aggregate(df['TodayPL'],list(Date=df$Date,Limit=df$Limit,Bucket=df$BucketLabel),function(x)sum(x,na.rm=TRUE))
        df$ProbCross <- paste(level*100,'%',sep="")
        df$BucketPL <- cumsum(df$TodayPL)
        df <- merge(df,dates_cross,by=c('Date','Bucket','Limit'))
        first <- FALSE  
      }
    } else {
      tdf <- merge(sub_data[sub_data[[b]],c('TradeDate','Instrument','Trader','Strategy','TodayPL')],all_bucket_data[all_bucket_data$Bucket==b,c('TraderID','Trader','Limit','Date','Prob','BucketLabel')],by.x=c('TradeDate','Trader'),by.y=c('Date','TraderID'))
      colnames(tdf) <- c('Date','TraderID','Instrument','Strategy','TodayPL','Trader','Limit','Prob','BucketLabel')
      dates_cross <- tdf[c('Date','BucketLabel','Limit','Prob')]
      dates_cross$CrossDate <- dates_cross$Date
      dates_cross$CrossDate[dates_cross$Prob>=0.05] <- NA
      dates_cross <- dates_cross[c('Date','BucketLabel','Limit','CrossDate')]
      colnames(dates_cross) <- c('Date','Bucket','Limit','CrossDate')
      tdf <- tdf[order(tdf$Date),]
      tdf$TodayPL[is.na(tdf$TodayPL)] <- 0
      tdf$TodayPL[tdf$Prob<level] <- 0
      if(nrow(tdf)>0){
        tdf <- aggregate(tdf['TodayPL'],list(Date=tdf$Date,Limit=tdf$Limit,Bucket=tdf$BucketLabel),function(x)sum(x,na.rm=TRUE))
        tdf$ProbCross <- paste(level*100,'%',sep="")
        tdf$BucketPL <- cumsum(tdf$TodayPL)
        tdf <- merge(tdf,dates_cross,by=c('Date','Bucket','Limit'))
        df <- rbind(df,tdf)  
      }
    }
  }
  return(df)
}
all_pl <- bucket_pl(all_hd,history_data,all_bucket_data)
all_pl$Trader <- 'All'
all_pl <- rbind(all_pl,
                cbind(Trader='JS', bucket_pl(all_hd,history_data,all_bucket_data,traders=11)),
                cbind(Trader='BA', bucket_pl(all_hd,history_data,all_bucket_data,traders=70)),
                cbind(Trader='DK', bucket_pl(all_hd,history_data,all_bucket_data,traders=101)))
all_pl_50 <- bucket_pl(all_hd,history_data,all_bucket_data,level=0.5)
all_pl_50$Trader <- 'All'
all_pl_50 <- rbind(all_pl_50,
                cbind(Trader='JS', bucket_pl(all_hd,history_data,all_bucket_data,traders=11,level=0.5)),
                cbind(Trader='BA', bucket_pl(all_hd,history_data,all_bucket_data,traders=70,level=0.5)),
                cbind(Trader='DK', bucket_pl(all_hd,history_data,all_bucket_data,traders=101,level=0.5)))


plot_bucket_pl <- function(data,traders){
  sub_data <- data[data$Trader%in%traders,]
  plt <- ggplot(data=sub_data,aes(x=Date,y=BucketPL,group=Limit,colour=Limit)) +
    geom_line(size=1) + geom_vline(xintercept = CrossDate)
    ylab("PL buket") + theme(legend.position="bottom") +
    facet_grid(Bucket~Trader,scales="free_y")
  return(plt)
}

#1. $ value in the buckets
value <- plot_trader_buckets(all_bucket_data,c('All','BA','DK','JS'))
#2. probablility to cross the limit
crossing <- plot_crossing_prob(all_bucket_data,c('All','BA','DK','JS'))
#3. PL in the bucket when limit cross prob greater than 5%
pl <- plot_bucket_pl(all_pl,c('All','BA','DK','JS'))
pl_50 <- plot_bucket_pl(all_pl_50,c('All','BA','DK','JS'))
