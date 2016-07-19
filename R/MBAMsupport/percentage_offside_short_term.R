setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
library(zoo)
library(ggplot2)
library(RColorBrewer) 
library(plotly)
library(RODBC)

traders   <- c(101,11,70)
dates <- c("2016-06-24")

first <- TRUE
for(t in traders){
  kf <- function()dated_three_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_three_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_three_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
  }
}
#history_data <- readRDS("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history.rds")
history_data <- market_rel_pl(history_data)
#saveRDS(history_data,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history.rds")

#Get bulk price lows data
get_rel_lows <- function(start_date,end_date){
  SQL <- paste("select dtDateTime, lInstrumentID, rDaysSince3MLow, rDaysSince3MHigh, rDaysSince6MLow from tInstrumentMatrixHist where dtDateTime >= '",start_date,"' and dtDateTime <= '",end_date,"'",sep="")
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  sqlQuery(cn,'use Razor')
  rel_lows <- sqlQuery(cn,SQL)
  close(cn)
  return(rel_lows)
}
lows_data <- get_rel_lows(min(history_data$TradeDate,na.rm=TRUE),max(history_data$TradeDate,na.rm=TRUE))
colnames(lows_data) <- c('TradeDate','Instrument','DaysSince3MLow','DaysSince3MHigh','DaysSince6MLow')
lows_data$TradeDate <- as.Date(lows_data$TradeDate)

abslimit <- -0.1
rellimit <- -0.1
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
all_hd$OffSideBucket <- NA
all_hd$PsnLong <- all_hd$MarketValue > 0 
all_hd$LowsOffSideBucket <- NA
all_hd$DaysSince3MLow[is.na(all_hd$DaysSince3MLow)] <- 5000
all_hd$DaysSince6MLow[is.na(all_hd$DaysSince6MLow)] <- 5000
all_hd$DaysSince3MHigh[is.na(all_hd$DaysSince3MHigh)] <- 5000
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30,]$OffSideBucket <- 1
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSince3MLow<10&all_hd$PsnLong,]$LowsOffSideBucket <- 2
all_hd[all_hd$DaysOffAbs>30&all_hd$DaysOffRel>30&all_hd$DaysSince3MHigh<10&!all_hd$PsnLong,]$LowsOffSideBucket <- 2

exclude <- c('BA_SHEDGE','BA_LHEDGE','DK_SHDG','DK_LHDG','JS_SHEDGE','JS_LHEDGE')
all_hd <- all_hd[!all_hd$Strategy%in%exclude,]

#Absolute offside distribution
ttl_AbsOff <- aggregate(all_hd$MarketValue,list(DaysOffAbs=all_hd$DaysOffAbs),function(x)sum(abs(x),na.rm=TRUE))
ttl_AbsOff$x <- ttl_AbsOff$x/nrow(ttl_AbsOff)

#Timeseries of absolute/relative offside
side_timeseries <- function(all_hd,side_type)
{
  tseries_Total <- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
  tseries<- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate,Bucket=all_hd[[side_type]]),function(x)sum(abs(x),na.rm=TRUE))
  tseries<- merge(tseries,tseries_Total,by='TradeDate')
  tseries$PcntOff <- 100*(tseries$x.x/tseries$x.y)
  colnames(tseries) <- c('TradeDate','Bucket','dlr_Offside','dlr_Total','pct_Offside')
  plt_data <- rbind(data.frame(Type='% $ Offside',Date=tseries$TradeDate,Bucket=tseries$Bucket,Value=tseries$pct_Offside),
                    data.frame(Type='$ Invested',Date=tseries$TradeDate,Bucket='$',Value=tseries$dlr_Total))
  return(plt_data)
}

pltdata <- side_timeseries(all_hd,'OffSideBucket')
pltdata <- pltdata[pltdata$Type=='% $ Offside',]
lowsdata <- side_timeseries(all_hd,'LowsOffSideBucket')
lowsdata <- lowsdata[lowsdata$Type=='% $ Offside',]
lowsdata$Type <- '% $ Offside (3m Lows)'
pltdata <- rbind(pltdata,lowsdata)

ggplot(data=pltdata,aes(x=Date,y=Value,group=Bucket,colour=Bucket)) +
  geom_line(size=1) +
  facet_grid(Type~.)

#Compute probabilites for reaching limits given current location and timescale forward
tseries <- side_timeseries(all_hd,'LowsOffSideBucket')
tseries <- tseries[(tseries$Type=='% $ Offside')&(tseries$Bucket==2),c('Date','Value')]
jumps <- tseries
jumps$Diff <- c(NA,log(1+diff(jumps$Value)/100))
plt_jump_dist <- ggplot(data=jumps[c('Diff')],aes(x=Diff)) +
  geom_histogram() 

#Probability of crossing boundary given location and time horizon
prob_cross <- function(b,t,m=mean(jumps$Diff,na.rm=TRUE),st=sd(jumps$Diff,na.rm=TRUE)){
  st <- st*sqrt(t)
  pnorm(b, mean=m, sd=st, lower.tail=FALSE) 
}
bdary <- 15
thorizon <- 100
pd <- expand.grid(b=0.01*(0:bdary),t=1:thorizon)
pd$z <- with(pd,prob_cross(b,t))*100
brks <- cut(pd$z,breaks=seq(0,50,len=10))
brks <- gsub(","," - ",brks,fixed=TRUE)
pd$brks <- gsub("\\(|\\]","",brks)
pd$b <- exp(pd$b)-1
ggplot(pd,aes(b,t,z=z)) + 
  geom_raster(aes(fill = z)) +
  geom_contour(colour = "white")

#$ value expected to cross boundary
stats_in_window <- function(tseries,lookback=20){
  mn <- as.list(rep(NA,lookback-1))
  st <- as.list(rep(NA,lookback-1))
  for(i in lookback:nrow(tseries)){
    jumps <- log(1+diff(tseries$Value[(i-lookback+1):i])/100)
    mn[[i]] <- mean(jumps,na.rm=TRUE)
    st[[i]] <- sd(jumps,na.rm=TRUE)
  }
  tseries$LogJumpMean  <- mn
  tseries$LogJumpStDev <- st
  return(tseries)
}
tseries_stats <- stats_in_window(tseries)

cross_prob <- function(tseries_stats,level,horizon,total_inv,nominal=8,offlevel=abs(abslimit)){
  p <- list()
  for(r in 1:nrow(tseries_stats)){
    p[[r]] <- prob_cross(log(1+0.01*(level-tseries_stats$Value[[r]])),horizon,tseries_stats$LogJumpMean[[r]],tseries_stats$LogJumpStDev[[r]])  
  }
  tseries_stats$Prob <- as.numeric(p)
  tseries_stats <- merge(tseries_stats,total_inv,by='Date')
  tseries_stats$MinLoss <- (level/100)*tseries_stats$MarketValue*(nominal/100)*offlevel
  return(tseries_stats)
  
}

total <- aggregate(all_hd$MarketValue,list(TradeDate=all_hd$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
colnames(total) <- c('Date','MarketValue')
losses_13_5 <- cross_prob(tseries_stats,3,5,total)
losses_15_2 <- cross_prob(tseries_stats,4,2,total)
losses_15_5 <- cross_prob(tseries_stats,4,5,total)
losses_18_2 <- cross_prob(tseries_stats,5,2,total)
losses_18_5 <- cross_prob(tseries_stats,5,5,total)
losses_20_2 <- cross_prob(tseries_stats,10,2,total)
losses_20_5 <- cross_prob(tseries_stats,10,5,total)

tseries_simple <- side_timeseries(all_hd,'OffSideBucket')
tseries_simple_stats <- stats_in_window(tseries_simple)
simple_losses_5 <- cross_prob(tseries_simple_stats,5,5,total)
simple_losses_4 <- cross_prob(tseries_simple_stats,4,5,total)

simple_losses_5[is.nan(simple_losses_5$Prob),]$Prob <- NA
simple_losses_4[is.nan(simple_losses_4$Prob),]$Prob <- NA


#Build frame to show the names in question for each trader
names <- merge(unique(all_hd[(!is.na(all_hd$LowsOffSideBucket))&all_hd$LowsOffSideBucket==2,c('TradeDate','Instrument','Trader','Strategy','MarketValue')]),unique(history_data[c('Name','Instrument')]),by=c('Instrument'))
names <- names[!is.na(names$Name),]
tf <- losses_18_5[c('Date','Prob')]
colnames(tf) <- c('TradeDate','Prob5pcnt')
names <- merge(names,tf,by=c('TradeDate'))
tf <- losses_15_5[c('Date','Prob')]
colnames(tf) <- c('TradeDate','Prob4pcnt')
names <- merge(names,tf,by=c('TradeDate'))
tf <- simple_losses_5[c('Date','Prob')]
colnames(tf) <- c('TradeDate','RawProb5pcnt')
names <- merge(names,tf,by=c('TradeDate'))
tf <- simple_losses_4[c('Date','Prob')]
colnames(tf) <- c('TradeDate','RawProb4pcnt')
names <- merge(names,tf,by=c('TradeDate'))

pnl_5pcnt_names <- unique(names[names$Prob5pcnt>0.02,]$Instrument)
pnl_5pcnt_names <- merge(all_hd,data.frame(Instrument=pnl_5pcnt_names),by='Instrument')
pnl_5pcnt_names <- aggregate(pnl_5pcnt_names$TodayPL,list(Date=pnl_5pcnt_names$TradeDate),sum)
pnl_5pcnt_names <- cbind(Level='5%',Type='PL',pnl_5pcnt_names)
colnames(pnl_5pcnt_names) <- c('Level','Type','Date','MinLoss')
pnl_5pcnt_names$MinLoss <- cumsum(pnl_5pcnt_names$MinLoss)

raw_pnl_5pcnt_names <- unique(names[names$RawProb5pcnt>0.02,]$Instrument)
raw_pnl_5pcnt_names <- merge(all_hd,data.frame(Instrument=raw_pnl_5pcnt_names),by='Instrument')
raw_pnl_5pcnt_names <- aggregate(raw_pnl_5pcnt_names$TodayPL,list(Date=raw_pnl_5pcnt_names$TradeDate),sum)
raw_pnl_5pcnt_names <- cbind(Level='Raw 5%',Type='PL',raw_pnl_5pcnt_names)
colnames(raw_pnl_5pcnt_names) <- c('Level','Type','Date','MinLoss')
raw_pnl_5pcnt_names$MinLoss <- cumsum(raw_pnl_5pcnt_names$MinLoss)

raw_pnl_4pcnt_names <- unique(names[names$RawProb4pcnt>0.05,]$Instrument)
raw_pnl_4pcnt_names <- merge(all_hd,data.frame(Instrument=raw_pnl_4pcnt_names),by='Instrument')
raw_pnl_4pcnt_names <- aggregate(raw_pnl_4pcnt_names$TodayPL,list(Date=raw_pnl_4pcnt_names$TradeDate),sum)
raw_pnl_4pcnt_names <- cbind(Level='Raw 4%',Type='PL',raw_pnl_4pcnt_names)
colnames(raw_pnl_4pcnt_names) <- c('Level','Type','Date','MinLoss')
raw_pnl_4pcnt_names$MinLoss <- cumsum(raw_pnl_4pcnt_names$MinLoss)

pnl_4pcnt_names <- unique(names[names$Prob4pcnt>0.05,]$Instrument)
pnl_4pcnt_names <- merge(all_hd,data.frame(Instrument=pnl_4pcnt_names),by='Instrument')
pnl_4pcnt_names <- aggregate(pnl_4pcnt_names$TodayPL,list(Date=pnl_4pcnt_names$TradeDate),sum)
pnl_4pcnt_names <- cbind(Level='4%',Type='PL',pnl_4pcnt_names)
colnames(pnl_4pcnt_names) <- c('Level','Type','Date','MinLoss')
pnl_4pcnt_names$MinLoss <- cumsum(pnl_4pcnt_names$MinLoss)

plt_losses <- rbind(#data.frame(Level='3%',Type='MinLoss $',Date=losses_13_5$Date,MinLoss=losses_13_5$MinLoss),
  #data.frame(Level='4%',Type='MinLoss $',Date=losses_15_5$Date,MinLoss=losses_15_5$MinLoss),
  data.frame(Level='5%',Type='MinLoss $',Date=losses_18_5$Date,MinLoss=losses_18_5$MinLoss),
  #data.frame(Level='Raw 5%',Type='MinLoss $',Date=simple_losses_5$Date,MinLoss=simple_losses_5$MinLoss),
  #data.frame(Level='Raw 4%',Type='MinLoss $',Date=simple_losses_4$Date,MinLoss=simple_losses_4$MinLoss),
  #data.frame(Level='3%',Type='5DayLikelihood %',Date=losses_13_5$Date,MinLoss=losses_13_5$Prob*100),
  #data.frame(Level='4%',Type='5DayLikelihood %',Date=losses_15_5$Date,MinLoss=losses_15_5$Prob*100),
  data.frame(Level='5%',Type='5DayLikelihood %',Date=losses_18_5$Date,MinLoss=losses_18_5$Prob*100),
  #data.frame(Level='Raw 5%',Type='5DayLikelihood %',Date=simple_losses_5$Date,MinLoss=simple_losses_5$Prob*100),
  #data.frame(Level='Raw 4%',Type='5DayLikelihood %',Date=simple_losses_4$Date,MinLoss=simple_losses_4$Prob*100),
  pnl_5pcnt_names,
  pnl_4pcnt_names,
  raw_pnl_5pcnt_names,
  raw_pnl_4pcnt_names)

ggplot(data=plt_losses,aes(x=Date,y=MinLoss,group=Level,colour=Level)) +
  geom_line(size=1) +
  ylab("")+
  facet_grid(Type~.,scales="free_y")