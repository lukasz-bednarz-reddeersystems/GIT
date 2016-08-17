setwd("C:/Development/TradingEnhancementEngine/R/MBAMsupport")
options(modifiedOnlySource=TRUE)
library(R.utils)
library(scales)
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
source('../scripts/excel_analysis_functions.R')
library(ggplot2)
library(plotly)
library(GGally)

traders   <- c(101,11,70)
dates <- c("2016-08-01")
exclude <- c("DK_SHDG","DK_LHDG","BA_SHEDGE","BA_LHEDGE","JS_SHEDGE","JS_LHEDGE")

first <- TRUE
for(t in traders){
  kf <- function()dated_twelve_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))  
  }
}
history_data <- history_data[!history_data$Strategy%in%exclude,]
history_data <- market_rel_pl(history_data,trade_rel=FALSE)

scale_data <- c('ValueUSD','VolInto','VolOutof','SkewInto','SkewOutof','CompoundReturnInto','CompoundReturnOutof')
cols <- c('Trader','TradeDate','Strategy','Instrument',scale_data,'RSI14','TodayPL','Long','MarketValue','EarliestMarketValue','MarketRelPL','MinDate','PnLOutof')
history_data <- position_age_from_flats(history_data,cols)
history_data=readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data.rds")
#PnlOutof is missing on some trades for some reason... replace it in this case with TodayPL
history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$PnLOutof <- history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$TodayPL
history_data <- trade_typer(history_data)

history_data$PsnLong <- history_data$MarketValue>0
history_data <- switch_direction(history_data,'SkewInto')
history_data <- switch_direction(history_data,'SkewOutof')
history_data <- switch_direction(history_data,'CompoundReturnInto')
history_data <- switch_direction(history_data,'CompoundReturnOutof')
history_data[grepl('New',history_data$TradeType)&(is.na(history_data$ValueUSD)|history_data$ValueUSD==0),]$ValueUSD <- abs(history_data[grepl('New',history_data$TradeType)&(is.na(history_data$ValueUSD)|history_data$ValueUSD==0),]$MarketValue)

#Basic segment PL
#May need to aggregate trades in one day here
#Change trade value sign depending on ADD/REMOVE
history_data$SegmentPL <- NA
history_data$SegmentUSD<- NA
history_data$CumSegmentPL <- NA
history_data$CumSegmentUSD<- NA
history_data$PriorMktVal <- NA
for(strategy in unique(history_data$Strategy)){
  for(instrument in unique(history_data[history_data$Strategy==strategy,]$Instrument)){
    for(visit in unique(history_data[history_data$Strategy==strategy&history_data$Instrument==instrument,]$Visit)){
      df <- history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit,]
      min_date <- min(df[grepl('New',df$TradeType),]$TradeDate,na.rm=TRUE)
      add_date <- min(df[grepl('Add',df$TradeType),]$TradeDate,na.rm=TRUE)
      if(!is.na(as.character(min_date))){
        if(!is.na(as.character(add_date))){
          pl <- sum(df[df$TradeDate<add_date,]$TodayPL,na.rm=TRUE)
          usd<- sum(df[df$TradeDate<add_date,]$ValueUSD,na.rm=TRUE)
          cpl <- df[df$TradeDate<add_date,]$TodayPL
          cpl[is.na(cpl)] <- 0
          cumpl <- cumsum(cpl)
          cusd <- df[df$TradeDate<add_date,]$ValueUSD
          cusd[is.na(cusd)] <- 0
          cumusd<- cumsum(cusd)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate<add_date,]$PriorMktVal <- 0
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentPL <- pl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentUSD <- usd
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate<add_date,]$CumSegmentPL <- cumpl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate<add_date,]$CumSegmentUSD <- cumusd
          pl <- sum(df[df$TradeDate>=add_date,]$TodayPL,na.rm=TRUE)
          usd<- sum(df[df$TradeDate>=add_date,]$ValueUSD,na.rm=TRUE)
          cpl <- df[df$TradeDate>=add_date,]$TodayPL
          cpl[is.na(cpl)] <- 0
          cumpl <- cumsum(cpl)
          cusd <- df[df$TradeDate>=add_date,]$ValueUSD
          cusd[is.na(cusd)] <- 0
          cumusd<- cumsum(cusd)
          mv <- history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate<add_date,]
          N <- nrow(mv)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=add_date,]$PriorMktVal <- abs(mv[N,'MarketValue'])
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==add_date,]$SegmentPL <- pl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==add_date,]$SegmentUSD <- usd
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=add_date,]$CumSegmentPL <- cumpl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=add_date,]$CumSegmentUSD <- cumusd
        } else {
          pl <- sum(df[df$TradeDate>=min_date,]$TodayPL,na.rm=TRUE)
          usd<- sum(df[df$TradeDate>=min_date,]$ValueUSD,na.rm=TRUE)
          cpl <- df[df$TradeDate>=min_date,]$TodayPL
          cpl[is.na(cpl)] <- 0
          cumpl <- cumsum(cpl)
          cusd <- df[df$TradeDate>=min_date,]$ValueUSD
          cusd[is.na(cusd)] <- 0
          cumusd<- cumsum(cusd)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=min_date,]$PriorMktVal <- 0
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentPL <- pl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentUSD <- usd
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=min_date,]$CumSegmentPL <- cumpl
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate>=min_date,]$CumSegmentUSD <- cumusd
        } 
      }
    }
  }
}
#saveRDS(history_data,"//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL_Q2_2016.rds")
history_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL.rds")
#history_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL_Q2_2016.rds")
history_data$TraderTradeType <- paste(history_data$Trader,history_data$TradeType,sep=" ") 

initial_trades <- unique(history_data[!is.na(history_data$TradeID)&(history_data$TradeType=='New Long'|history_data$TradeType=='New Short'),c(cols,'PsnIncreased','TradeType','SegmentPL')])
increases <- unique(history_data[!is.na(history_data$TradeID)&(history_data$TradeType=='Add Long'|history_data$TradeType=='Add Short'),c(cols,'PsnIncreased','TradeType','SegmentPL')])

for(scale_col in scale_data){
  initial_trades[paste(scale_col,"Zscore",sep="")] <- scale_and_clip(initial_trades[[scale_col]])
  increases[paste(scale_col,"Zscore",sep="")] <- scale_and_clip(increases[[scale_col]])
}

initial_plot_data <- rbind(cbind(Quantity='Skewness',Timeframe="Pre-trade",data.frame(Value=initial_trades$SkewIntoZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Skewness',Timeframe="Post-trade",data.frame(Value=initial_trades$SkewOutofZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Pre-trade",data.frame(Value=initial_trades$VolIntoZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Post-trade",data.frame(Value=initial_trades$VolOutofZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Pre-trade",data.frame(Value=initial_trades$CompoundReturnIntoZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Post-trade",data.frame(Value=initial_trades$CompoundReturnOutofZscore),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Pre-trade",data.frame(Value=scale_and_clip(initial_trades$CompoundReturnIntoZscore/initial_trades$VolIntoZscore)),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Post-trade",data.frame(Value=scale_and_clip(initial_trades$CompoundReturnOutofZscore/initial_trades$VolOutofZscore)),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]))
increases_plot_data<- rbind(cbind(Quantity='Skewness',Timeframe="Pre-trade",data.frame(Value=increases$SkewIntoZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Skewness',Timeframe="Post-trade",data.frame(Value=increases$SkewOutofZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Pre-trade",data.frame(Value=increases$VolIntoZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Post-trade",data.frame(Value=increases$VolOutofZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Pre-trade",data.frame(Value=increases$CompoundReturnIntoZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Post-trade",data.frame(Value=increases$CompoundReturnOutofZscore),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Pre-trade",data.frame(Value=scale_and_clip(increases$CompoundReturnIntoZscore/increases$VolIntoZscore)),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Post-trade",data.frame(Value=scale_and_clip(increases$CompoundReturnOutofZscore/increases$VolOutofZscore)),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSDZscore','RSI14')]))

scatter_plot_kernel <- function(data, mapping, method="lm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
       geom_point() + 
       geom_smooth(method=method,mapping = aes_string(colour="TradeType"), ...) +
       scale_colour_manual(values=c("green","red"))
  return(p)
}
density_plot_kernel <- function(data, mapping, ...){
  lnghitrate <- mean(data[grepl("Long",data$TradeType),]$PnLOutof>0,na.rm=TRUE)*100
  lnglosers  <- mean(data[grepl("Long",data$TradeType)&data$PnLOutof<0,]$PnLOutof,na.rm=TRUE)
  lngwinners <- mean(data[grepl("Long",data$TradeType)&data$PnLOutof>0,]$PnLOutof,na.rm=TRUE)
  shthitrate <- mean(data[grepl("Short",data$TradeType),]$PnLOutof>0,na.rm=TRUE)*100
  shtlosers  <- mean(data[grepl("Short",data$TradeType)&data$PnLOutof<0,]$PnLOutof,na.rm=TRUE)
  shtwinners <- mean(data[grepl("Short",data$TradeType)&data$PnLOutof>0,]$PnLOutof,na.rm=TRUE)
  mnsize_lng <- mean(data[grepl("Long",data$TradeType),]$ValueUSD,na.rm=TRUE)
  mnsize_sht <- mean(data[grepl("Short",data$TradeType),]$ValueUSD,na.rm=TRUE)
  mnskew_lng <- mean(data[grepl("Long",data$TradeType),]$SkewInto,na.rm=TRUE)
  mnskew_sht <- mean(data[grepl("Short",data$TradeType),]$SkewInto,na.rm=TRUE)
  if(mapping$x=='CompoundReturnOutofZscore'){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_density(mapping = aes_string(colour="TradeType"), fill=NA) +
      annotate("text",x=2,y=0.5,label=paste("Long:",round(lnghitrate),"%",sep=""),colour="green",size=6) +
      annotate("text",x=2,y=0.4,label=paste("Win:",round(lngwinners),sep=""),size=6) +
      annotate("text",x=2,y=0.3,label=paste("Loss:",round(lnglosers),sep=""),size=6) +
      annotate("text",x=-2.5,y=0.3,label=paste("Short:",round(shthitrate),"%",sep=""),colour="red",size=6) +
      annotate("text",x=-2.5,y=0.2,label=paste("Win:",round(shtwinners),sep=""),size=6) +
      annotate("text",x=-2.5,y=0.1,label=paste("Loss:",round(shtlosers),sep=""),size=6) +
      scale_colour_manual(values=c("green","red"))
  } else if(mapping$x=='ValueUSDZscore') {
      p <- ggplot(data = data, mapping = mapping) + 
        geom_density(mapping = aes_string(colour="TradeType"), fill=NA) +
        annotate("text",x=2,y=1.5,label=paste("Long: $",round(mnsize_lng/1000),"k",sep=""),colour="green",size=6) +
        annotate("text",x=2,y=0.5,label=paste("Short: $",round(mnsize_sht/1000),"k",sep=""),colour="red",size=6) +
        scale_colour_manual(values=c("green","red"))
  } else if(mapping$x=='SkewIntoZscore') {
      p <- ggplot(data = data, mapping = mapping) + 
        geom_density(mapping = aes_string(colour="TradeType"), fill=NA) +
        annotate("text",x=2,y=0.6,label=paste("Long: ",round(mnskew_lng,digits=2),sep=""),colour="green",size=6) +
        annotate("text",x=2,y=0.5,label=paste("Short: ",round(mnskew_sht,digits=2),sep=""),colour="red",size=6) +
        scale_colour_manual(values=c("green","red"))
  } else {
    p <- ggplot(data = data, mapping = mapping) + 
      geom_density(mapping = aes_string(colour="TradeType"), fill=NA) +
      scale_colour_manual(values=c("green","red"))
  }
  return(p)
}

#----------------------------------------------------
#Plot pairwise regressions and correlations

pcols <- c('VolOutofZscore','SkewIntoZscore','CompoundReturnOutofZscore','ValueUSDZscore')
trader <- 11
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("JS opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("JS position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols, 
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS opening trades") +
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18))    
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS position increases") +
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18)) 

trader <- 101
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("DK opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("DK position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK opening trades") +
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18)) 
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK position increases") +
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18)) 

trader <- 70
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("BA opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSDZscore, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("BA position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA opening trades") +
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18))
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA position increases")+
        theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18))

#----------------------------------------------------
#Examine specific trade group performance

initial_trades <- rbind(initial_trades,increases)
initial_trades$Month <- format(initial_trades$TradeDate,"%Y-%m")
initial_trades$Trader <- substr(initial_trades$Strategy,1,2)
initial_trades$TraderTradeType <- paste(initial_trades$Trader,initial_trades$TradeType,sep=" ") 

trade_stats <- data_fractile(initial_trades,'ValueUSD',10,'Decile',c('TraderTradeType','Month'))
trade_stats$StockHit <- trade_stats$CompoundReturnOutof > 0
trade_stats$StockWin <- NA
trade_stats$StockWin[!is.na(trade_stats$CompoundReturnOutof)&trade_stats$CompoundReturnOutof > 0] <- trade_stats$CompoundReturnOutof[!is.na(trade_stats$CompoundReturnOutof)&trade_stats$CompoundReturnOutof > 0]
trade_stats$StockLoss <- NA
trade_stats$StockLoss[!is.na(trade_stats$CompoundReturnOutof)&trade_stats$CompoundReturnOutof < 0] <- trade_stats$CompoundReturnOutof[!is.na(trade_stats$CompoundReturnOutof)&trade_stats$CompoundReturnOutof < 0]
trade_stats$PsnHit <- trade_stats$CompoundReturnOutof > 0
ext <- trade_stats$PnLOutof/abs((trade_stats$CompoundReturnOutof/10000)*trade_stats$ValueUSD)
trade_stats$Extraction <- sign(ext)*log(abs(ext))
trade_stats$PsnWin <- NA
trade_stats$PsnWin[trade_stats$PnLOutof>0] <- trade_stats$PnLOutof[trade_stats$PnLOutof>0]
trade_stats$PsnLoss <- NA
trade_stats$PsnLoss[trade_stats$PnLOutof<0] <- trade_stats$PnLOutof[trade_stats$PnLOutof<0]
#saveRDS(trade_stats,"//MBAM/main/MBAM/Trading Enhancements/data/sizing_trade_stats.rds")

#1. Overall

trade_stat_package <- c('VolInto','SkewInto','VolOutof','SkewOutof','SegmentPL','ValueUSD','CompoundReturnOutof','PnLOutof','RSI14','StockHit','PsnHit','Extraction','ValueUSDDecile_N')
agg_stats <- aggregate(trade_stats[trade_stat_package],list(TradeDate=trade_stats$TradeDate,TraderTradeType=trade_stats$TraderTradeType),function(x)mean(x,na.rm=TRUE))
first <- TRUE
for(clm in trade_stat_package){
  sub_data <- agg_stats[c('TradeDate','TraderTradeType',clm)]
  colnames(sub_data) <- c('TradeDate','TraderTradeType','Value')
  if(first){
    plot_data <- cbind(Metric=clm,sub_data)
    first <- FALSE
  } else{
    plot_data <- rbind(plot_data,cbind(Metric=clm,sub_data))
  }
}
size_smmry <- ggplot(plot_data,aes(x=TradeDate,y=Value,group=Metric,colour=Metric)) +
              geom_point() +
              geom_smooth() +
              facet_grid(Metric~TraderTradeType,scales="free_y")

#2. Examine stats of top and bottom size quartile trades (compared to others)

trade_stat_package <- c('VolInto','SkewInto','VolOutof','ValueUSD','CompoundReturnOutof','PsnWin','StockLoss','StockWin','PsnLoss','RSI14','PsnHit','Extraction','ValueUSDDecile_N','PnLOutof','SegmentPL')
agg_stats <- aggregate(trade_stats[trade_stat_package],list(TradeDate=trade_stats$TradeDate,TraderTradeType=trade_stats$TraderTradeType,Decile=ifelse(trade_stats$TraderTradeTypeMonthGroupDecile_N<3,'1-2',ifelse(trade_stats$TraderTradeTypeMonthGroupDecile_N>8,'9-10','3-8'))),function(x)mean(x,na.rm=TRUE))
agg_stats_ovr <- cbind(Decile='All',aggregate(trade_stats[trade_stat_package],list(TradeDate=trade_stats$TradeDate,TraderTradeType=trade_stats$TraderTradeType),function(x)mean(x,na.rm=TRUE)))
agg_stats <- rbind(agg_stats,agg_stats_ovr)
agg_stats$PayOff <- agg_stats$StockWin*agg_stats$PsnHit+(1-agg_stats$PsnHit)*agg_stats$StockLoss
oncols <- c(trade_stat_package,'PayOff')
withcols <- c('TradeDate','TraderTradeType','Decile')

unroll <- function(agg_stats,oncols,withcols){
  first <- TRUE
  for(clm in oncols){
    sub_data <- agg_stats[c(withcols,clm)]
    colnames(sub_data) <- c(withcols,'Value')
    if(first){
      plot_data <- cbind(Metric=clm,sub_data)
      first <- FALSE
    } else{
      plot_data <- rbind(plot_data,cbind(Metric=clm,sub_data))
    }
  }
  plot_data$Value[is.infinite(plot_data$Value)] <- NA
  plot_data$Value[is.nan(plot_data$Value)] <- NA
  return(plot_data)
}

plot_data <- unroll(agg_stats,oncols,withcols)
plot_data$Month <- as.Date(paste(format(plot_data$TradeDate,"%Y-%m"),"-01",sep=""))
maxday <- plot_data[c('Month','TradeDate')]
maxday <- aggregate(maxday['TradeDate'],list(Month=maxday$Month),function(x)max(x,na.rm=TRUE))

monthly_agg <- aggregate(plot_data['Value'],list(Month=plot_data$Month,Metric=plot_data$Metric,TraderTradeType=plot_data$TraderTradeType,Decile=plot_data$Decile),function(x)mean(x,na.rm=TRUE))
monthly_agg$Value[is.nan(monthly_agg$Value)] <- NA
monthly_agg$Side <- substr(monthly_agg$TraderTradeType,8,12)
#colnames(monthly_agg)[colnames(monthly_agg)=='Value'] <- 'AggValue'
#monthly_agg <- merge(monthly_agg,maxday,by='Month')
#monthly_agg <- merge(monthly_agg[c('Month','TraderTradeType','Metric','AggValue','TradeDate')],plot_data,by=c('TradeDate','TraderTradeType','Metric'))
#monthly_agg <- aggregate(monthly_agg[c('Value','AggValue')],list(Month=monthly_agg$Month,TraderTradeType=monthly_agg$TraderTradeType,Metric=monthly_agg$Metric,TradeDate=monthly_agg$TradeDate),function(x)mean(x,na.rm=TRUE))
#monthly_agg$Value <- 1.5*monthly_agg$AggValue

trader='BA'
vars <- c('VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','SkewInto','SegmentPL','PayOff')
qtile_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Side=='Long'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Decile,scales="free_y")
all_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Decile=='All'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric','Side')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  ylab("") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Side,scales="free_y")

trader='JS'
vars <- c('VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','SkewInto','SegmentPL','PayOff')
qtile_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Side=='Long'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Decile,scales="free_y")
all_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Decile=='All'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric','Side')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Side,scales="free_y")

trader='DK'
vars <- c('VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','SkewInto','SegmentPL','PayOff')
qtile_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Side=='Long'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  theme(axis.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Decile,scales="free_y")
all_smmry <- ggplot(monthly_agg[substr(monthly_agg$TraderTradeType,1,2)==trader&monthly_agg$Decile=='All'&monthly_agg$Metric%in%vars,c('Month','Decile','Value','TraderTradeType','Metric','Side')],aes(x=Month,y=Value,group=TraderTradeType,colour=TraderTradeType)) +
  geom_smooth() +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~Side,scales="free_y")

#----------------------------------------------------
#Reduced PM/recent time market/stock colour view

segment_pl_plot <- aggregate(history_data['SegmentPL'],list(Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)sum(x,na.rm=TRUE))
df <- history_data[year(history_data$TradeDate)=='2016',]
segment_2016_pl_plot <- aggregate(df['SegmentPL'],list(Trader=df$Trader,TradeType=df$TradeType),function(x)sum(x,na.rm=TRUE))
#plot for whole data set and just for 2016
types <- c('New Long','New Short','Add Long','Add Short')
segment_pl_plot <- segment_pl_plot[segment_pl_plot$TradeType%in%types,]
segment_2016_pl_plot <- segment_2016_pl_plot[segment_2016_pl_plot$TradeType%in%types,]
segment_pl_plot <- rbind(cbind(Start=paste('Since',min(history_data$TradeDate)),segment_pl_plot),
                         cbind(Start='Since 2016-01-01',segment_2016_pl_plot))
segment_pl_plot[segment_pl_plot$Trader==11,]$Trader <- 'JS'
segment_pl_plot[segment_pl_plot$Trader==70,]$Trader <- 'BA'
segment_pl_plot[segment_pl_plot$Trader==101,]$Trader<- 'DK'
seg_plot <- ggplot(segment_pl_plot,aes(x=TradeType,fill=TradeType)) +
            geom_bar(aes(weight=SegmentPL),position="dodge") +
            ylab("PL $") +
            scale_y_continuous(labels=comma) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
            facet_grid(Start~Trader,scales="free_y")

#Monthly comparison of trade quantities
compare_frame <- merge(agg_stats[agg_stats$TraderTradeType=='BA New Long',],agg_stats[agg_stats$TraderTradeType=='BA Add Long',],by=c('TradeDate','Decile'))
compare_frame$ValueRatio <- compare_frame$ValueUSD.x/compare_frame$ValueUSD.y
compare_frame$WinRatio <- compare_frame$PsnWin.x/compare_frame$PsnWin.y
compare_frame$LossRatio <- compare_frame$PsnLoss.x/compare_frame$PsnLoss.y
compare_frame$HitRatio <- compare_frame$PsnHit.x/compare_frame$PsnHit.y
compare_frame$PLRatio <- compare_frame$PnLOutof.x/compare_frame$PnLOutof.y
plot_data <- unroll(compare_frame,c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio'),c('TradeDate','Decile'))

vars <- c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio')
compare_smmry <- ggplot(plot_data[plot_data$Metric%in%vars,c('TradeDate','Decile','Value','Metric')],aes(x=TradeDate,y=Value,group=Decile,colour=Decile)) +
  geom_smooth(method="loess",method.args = list(family = "symmetric")) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~.,scales="free_y")

compare_frame <- merge(agg_stats[agg_stats$TraderTradeType=='JS New Long',],agg_stats[agg_stats$TraderTradeType=='JS Add Long',],by=c('TradeDate'))
compare_frame$ValueRatio <- compare_frame$ValueUSD.x/compare_frame$ValueUSD.y
compare_frame$WinRatio <- compare_frame$PsnWin.x/compare_frame$PsnWin.y
compare_frame$LossRatio <- compare_frame$PsnLoss.x/compare_frame$PsnLoss.y
compare_frame$HitRatio <- compare_frame$PsnHit.x/compare_frame$PsnHit.y
compare_frame$PLRatio <- compare_frame$PnLOutof.x/compare_frame$PnLOutof.y
plot_data <- unroll(compare_frame,c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio'),c('TradeDate'))

vars <- c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio')
compare_smmry <- ggplot(plot_data[plot_data$Metric%in%vars,c('TradeDate','Value','Metric')],aes(x=TradeDate,y=Value)) +
  geom_smooth(method="loess",method.args = list(family = "symmetric")) +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~.,scales="free_y")

compare_frame <- merge(agg_stats[agg_stats$TraderTradeType=='DK New Long',],agg_stats[agg_stats$TraderTradeType=='DK Add Long',],by=c('TradeDate'))
compare_frame$ValueRatio <- compare_frame$ValueUSD.x/compare_frame$ValueUSD.y
compare_frame$WinRatio <- compare_frame$PsnWin.x/compare_frame$PsnWin.y
compare_frame$LossRatio <- compare_frame$PsnLoss.x/compare_frame$PsnLoss.y
compare_frame$HitRatio <- compare_frame$PsnHit.x/compare_frame$PsnHit.y
compare_frame$PLRatio <- compare_frame$PnLOutof.x/compare_frame$PnLOutof.y
plot_data <- unroll(compare_frame,c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio'),c('TradeDate'))

vars <- c('ValueRatio','WinRatio','LossRatio','HitRatio','PLRatio')
compare_smmry <- ggplot(plot_data[plot_data$Metric%in%vars,c('TradeDate','Value','Metric')],aes(x=TradeDate,y=Value)) +
  geom_smooth(method="loess",method.args = list(family = "symmetric")) + geom_point() +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~.,scales="free_y")

#----------------------------------------------------
#How trading could be adjusted? (trade level bucket risk/return)

history_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL_Q2_2016.rds")
history_data <- history_data[grepl('New|Add',history_data$TradeType),]
history_data$TradeRtn <- history_data$CumSegmentPL/(history_data$CumSegmentUSD+history_data$PriorMktVal)
history_data$TradeRtn[is.infinite(history_data$TradeRtn)] <- NA
history_data$TradeRtn[is.nan(history_data$TradeRtn)] <- NA
sub_data <- aggregate(history_data['TradeRtn'],list(Trader=history_data$Trader,TradeDate=history_data$TradeDate,TradeType=history_data$TradeType,TraderTradeType=history_data$TraderTradeType),function(x)mean(x,na.rm=TRUE))
sub_data$TradeRtn[is.nan(sub_data$TradeRtn)] <- NA

#Compute rolling Bayes estimiation of open/increase weights
window = 20
Trade_sharpe <- compute_rolling_fn(sub_data[sub_data$TradeType!='NA',c('Trader','TradeType','TradeDate','TradeRtn')],'TradeSharpe','TradeRtn',window,function(x)mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))
Trade_sharpe <- merge(Trade_sharpe,
                        aggregate(Trade_sharpe['TradeSharpe'],list(Trader=Trade_sharpe$Trader,TradeDate=Trade_sharpe$TradeDate),function(x)max(x,na.rm=TRUE)),
                        by=c('Trader','TradeDate'))
Trade_sharpe$Max <- 0
Trade_sharpe$Max[Trade_sharpe$TradeSharpe.x==Trade_sharpe$TradeSharpe.y] <- 1
Trade_sharpe$PreviousValue <- NA
Trade_sharpe <- Trade_sharpe[order(Trade_sharpe$TradeDate),]
for(trader in unique(Trade_sharpe$Trader)){
  for(tradet in unique(Trade_sharpe$TradeType)){
    Trade_sharpe[Trade_sharpe$Trader==trader&Trade_sharpe$TradeType==tradet,]$PreviousValue <- c(NA,Trade_sharpe[Trade_sharpe$Trader==trader&Trade_sharpe$TradeType==tradet,]$TradeSharpe.x[1:(length(Trade_sharpe[Trade_sharpe$Trader==trader&Trade_sharpe$TradeType==tradet,]$TradeSharpe.x)-1)]) 
  }
}
colnames(Trade_sharpe) <- c('Trader','TradeDate','TradeType','TradeRtn','TradeSharpe','MaxTradeSharpe','IsMax','PreviousSegSharpe')
Trade_sharpe <- Trade_sharpe[c('Trader','TradeDate','TradeType','TradeSharpe','MaxTradeSharpe','IsMax','PreviousSegSharpe')]

#This might work better is the time was aligned with position age rather than the absolute calendar date and then
#allocation to increases occured with respect to position age
bayes_window = 90
first <- TRUE
for(trader in unique(Trade_sharpe$Trader)){
  sub_data <- Trade_sharpe[Trade_sharpe$Trader==trader,]
  dates <- unique(sub_data$TradeDate)[unique(sub_data$TradeDate)>(min(sub_data$TradeDate,na.rm=TRUE)+bayes_window+window)]
  for(d in dates){
    dte <- as.Date(d)
    df <- sub_data[sub_data$TradeDate<=dte&sub_data$TradeDate>(dte-bayes_window),]
    if(nrow(df)>0){
      df$PPrevGvnHighest <- NA
      df$PHighest <- NA
      df$HigestSharpeGvnPrevious <- NA
      prev <- 0 
      p_highest_sharpe <- aggregate(unique(df[c('TradeDate','TradeType','IsMax')])['IsMax'],list(TradeType=unique(df[c('TradeDate','TradeType','IsMax')])$TradeType),function(x)sum(x,na.rm=TRUE))
      p_highest_sharpe$IsMax <- p_highest_sharpe$IsMax/sum(p_highest_sharpe$IsMax)
      sd <- sd(df$PreviousSegSharpe,na.rm=TRUE)
      p_previoussharpe <- density(as.numeric(df$PreviousSegSharpe[!is.na(df$PreviousSegSharpe)]),bw=sd)
      p_previoussharpe$y <- p_previoussharpe$y/sum(p_previoussharpe$y)
      df$PPrev <- p_previoussharpe$y[unlist(Map(function(x)ifelse(is.na(x),NA,which(abs(p_previoussharpe$x-x)==min(abs(p_previoussharpe$x-x)))),df$PreviousSegSharpe))]
      for(tt in c('New Long','Add Long','New Short','Add Short')){
        sd <- sd(df[df$TradeType==tt,]$PreviousSegSharpe,na.rm=TRUE)
        if(nrow(df[!is.na(df$PreviousSegSharpe)&df$TradeType==tt&df$IsMax==1,])>0){
          p_previoussharpe_gvn_highestsharpe <- density(as.numeric(df[!is.na(df$PreviousSegSharpe)&df$TradeType==tt&df$IsMax==1,]$PreviousSegSharpe),bw=sd)
          p_previoussharpe_gvn_highestsharpe$y <- p_previoussharpe_gvn_highestsharpe$y/sum(p_previoussharpe_gvn_highestsharpe$y)
          df[df$TradeType==tt,]$PPrevGvnHighest <- p_previoussharpe_gvn_highestsharpe$y[unlist(Map(function(x)ifelse(is.na(x),NA,which(abs(p_previoussharpe_gvn_highestsharpe$x-x)==min(abs(p_previoussharpe_gvn_highestsharpe$x-x)))),df[df$TradeType==tt,]$PreviousSegSharpe))]
          df[df$TradeType==tt,]$PHighest <- p_highest_sharpe[p_highest_sharpe$TradeType==tt,]$IsMax
          df[df$TradeType==tt,]$HigestSharpeGvnPrevious <- df[df$TradeType==tt,]$PPrevGvnHighest*df[df$TradeType==tt,]$PHighest/df[df$TradeType==tt,]$PPrev 
        } else {
          df[df$TradeType==tt,]$PPrevGvnHighest <- 0
          df[df$TradeType==tt,]$PHighest <- 0
          df[df$TradeType==tt,]$HigestSharpeGvnPrevious <- 0  
        }
        if(nrow(df[df$TradeDate==dte&df$TradeType==tt,])==0){
          blnk <- data.frame(t(rep(NA,ncol(df))))
          colnames(blnk) <- colnames(df)
          blnk$Trader <- trader
          blnk$TradeType <- tt
          blnk$TradeDate <- dte
          df <- rbind(df,blnk)
        }
      } 
      #not properly normalised because binning not handled properly -> hack
      df <- merge(df,aggregate(df$HigestSharpeGvnPrevious,list(Trader=df$Trader,TradeDate=df$TradeDate),function(x)sum(x)),by=c('Trader','TradeDate'))
      colnames(df)[colnames(df)=='x'] <- 'HigestSharpeGvnPreviousNorm'
      df$PHigestSharpeGvnPrevious <- df$HigestSharpeGvnPrevious/df$HigestSharpeGvnPreviousNorm
      if(first){
        bayes_data <- df[df$TradeDate==dte,]
        first <- FALSE
      } else {
        bayes_data <- rbind(bayes_data,df[df$TradeDate==dte,])
      } 
    }
  }
}
#saveRDS(bayes_data,"//MBAM/main/MBAM/Trading Enhancements/data/bayes_data_Q2_2016.rds")

#----------------------------------------------------
#Asses impact on performance

q2_only <- FALSE
if(q2_only){
  bayes_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/bayes_data_Q2_2016.rds")  
} else {
  bayes_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/bayes_data.rds")  
}

trade_weight <- compute_rolling_fn(bayes_data[c('Trader','TradeType','TradeDate','PHigestSharpeGvnPrevious')],'Weight','PHigestSharpeGvnPrevious',3,function(x)mean(x,na.rm=TRUE))
trade_weight$Weight[is.na(trade_weight$Weight)] <- 0.25
weight_norm <- aggregate(trade_weight$Weight,list(Trader=trade_weight$Trader,TradeDate=trade_weight$TradeDate),function(x)sum(x,na.rm=TRUE))
weight_num <- aggregate(trade_weight$TradeType,list(Trader=trade_weight$Trader,TradeDate=trade_weight$TradeDate),function(x)length(x))
trade_weight <- merge(trade_weight,weight_norm,by=c('Trader','TradeDate'))
trade_weight <- merge(trade_weight,weight_num,by=c('Trader','TradeDate'))
#renomalise to account for missing trade types: weight a flat prior
trade_weight$Weight <- 0.25+0.25*(((trade_weight$Weight/trade_weight$x.x)*trade_weight$x.y)-1)
trade_weight <- trade_weight[c('Trader','TradeDate','TradeType','Weight')]
                                       
skew_const <- 0 #non zero to modify by skew
variable_alloc <- 1 #non zero to apply Bayesian reweighting
if(q2_only){
  history_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL_Q2_2016.rds")  
} else {
  history_data <- readRDS("//MBAM/main/MBAM/Trading Enhancements/data/history_data_segmentPL.rds") 
}
history_data <- history_data[history_data$TradeType!='NA',]
history_data$TraderTradeType <- paste(history_data$Trader,history_data$TradeType,sep=" ") 
history_data <- merge(history_data,trade_weight[c('Trader','TradeType','TradeDate','Weight')],by=c('Trader','TradeType','TradeDate'),all.x=TRUE)
history_data$Weight[is.na(history_data$Weight)] <- 0.25
history_data$skew_coeff <- 0
history_data[history_data$Trader==11&grepl("Add Long",history_data$TradeType),]$skew_coeff <- skew_const
history_data[history_data$Trader==11&grepl("Add Short",history_data$TradeType),]$skew_coeff <- -skew_const
history_data[history_data$Trader==11&grepl("New Long",history_data$TradeType),]$skew_coeff <- -skew_const 
history_data[history_data$Trader==11&grepl("New Short",history_data$TradeType),]$skew_coeff <- skew_const
history_data[history_data$Trader==101,]$skew_coeff <- sign(history_data[history_data$Trader==101,]$SkewInto)*skew_const
history_data[history_data$Trader==70&grepl("New Long",history_data$TradeType),]$skew_coeff <- skew_const
history_data[history_data$Trader==70&grepl("New Short",history_data$TradeType),]$skew_coeff <- -skew_const
history_data[history_data$Trader==70&grepl("Add Long",history_data$TradeType),]$skew_coeff <- -skew_const
history_data[history_data$Trader==70&grepl("Add Short",history_data$TradeType),]$skew_coeff <- skew_const
history_data$skew_coeff[is.na(history_data$skew_coeff)] <- 0
history_data$SwingSize <- history_data$ValueUSD
history_data$Rebal <- (1-variable_alloc)*history_data$ValueUSD+((variable_alloc*history_data$ValueUSD)*(1+4*(history_data$Weight-0.25)))
history_data$skew_rb <- history_data$skew_coeff*sign(history_data$SkewInto)*(abs(history_data$SkewInto)^0.3)*(history_data$VolInto/10000)
history_data$skew_rb[history_data$skew_rb < -0.9] <- -0.9
history_data$skew_rb[history_data$skew_rb > 0.9] <- 0.9
history_data$skew_rb[is.na(history_data$skew_rb)] <- 0
history_data$SkewSize <- history_data$Rebal*(1+history_data$skew_rb)
total_trd_capital <- aggregate(history_data[c('SkewSize','SwingSize')],list(TradeType=history_data$TradeType,TradeDate=history_data$TradeDate,Trader=history_data$Trader),function(x)sum(x,na.rm=TRUE))
global_trd_capital <- aggregate(history_data[c('SkewSize','SwingSize')],list(TradeDate=history_data$TradeDate,Trader=history_data$Trader),function(x)sum(x,na.rm=TRUE))
colnames(total_trd_capital) <- c('TradeType','TradeDate','Trader','TotalSkewSize','TotalSwingSize')
colnames(global_trd_capital) <- c('TradeDate','Trader','GlobalSkewSize','GlobalSwingSize')
history_data <- merge(history_data,total_trd_capital,by=c('TradeType','TradeDate','Trader'))
history_data <- merge(history_data,global_trd_capital,by=c('TradeDate','Trader'))
if(variable_alloc==0){
  history_data$SkewSize <- history_data$TotalSwingSize*(history_data$SkewSize/history_data$TotalSkewSize)  
}
history_data$SwingSizePL <- history_data$SwingSize*(history_data$CompoundReturnOutof/10000)
history_data$SkewSizePL <- history_data$SkewSize*(history_data$CompoundReturnOutof/10000)
if(variable_alloc==0){
  history_data$SwingRtn <- history_data$SwingSizePL/history_data$TotalSwingSize
  history_data$SkewRtn <- history_data$SkewSizePL/history_data$TotalSwingSize  
} else {
  history_data$SwingRtn <- history_data$SwingSizePL/history_data$GlobalSwingSize
  history_data$SkewRtn <- history_data$SkewSizePL/history_data$GlobalSkewSize
}
history_data$SwingSize[history_data$SwingSize==0] <- NA
history_data$SkewSize[history_data$SkewSize==0] <- NA
history_data$SwingSize[is.nan(history_data$SwingSize)] <- NA
history_data$SkewSize[is.nan(history_data$SkewSize)] <- NA
history_data$SkewRtn[history_data$SkewRtn==0] <- NA
history_data$SwingRtn[is.nan(history_data$SwingRtn)] <- NA
history_data$SwingRtn[history_data$SwingRtn==0] <- NA
history_data$SkewRtn[is.nan(history_data$SkewRtn)] <- NA

if(variable_alloc>0){
  sharpe_str <- "Av.Sharpe (fund)"
  type_str <- "Bayes"
} else {
  sharpe_str <- "Av.Sharpe (bucket)"
  type_str <- "skew"
}

rtn_stats <- aggregate(history_data[c('SkewSize','SwingSize')],list(Trader=history_data$Trader,TradeDate=history_data$TradeDate,TradeType=history_data$TradeType),function(x)mean(x,na.rm=TRUE))
rtn_stats <- rbind(cbind(data.frame(Adjusted='Not adjusted',Quantity='Av.Size',Value=rtn_stats$SwingSize),rtn_stats[c('Trader','TradeDate','TradeType')]),
                   cbind(data.frame(Adjusted='Adjusted',Quantity='Av.Size',Value=rtn_stats$SkewSize),rtn_stats[c('Trader','TradeDate','TradeType')]))

daily_rtn <- aggregate(history_data[c('SkewRtn','SwingRtn')],list(TradeDate=history_data$TradeDate,Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)mean(x,na.rm=TRUE))
rtn_stats <- rbind(rtn_stats,
                   cbind(Adjusted='Adjusted',Quantity=sharpe_str,compute_rolling_fn(daily_rtn[c('Trader','TradeType','TradeDate','SkewRtn')],'Value','SkewRtn',20,function(x)mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))[c('Trader','TradeType','TradeDate','Value')]),
                   cbind(Adjusted='Not adjusted',Quantity=sharpe_str,compute_rolling_fn(daily_rtn[c('Trader','TradeType','TradeDate','SwingRtn')],'Value','SwingRtn',20,function(x)mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))[c('Trader','TradeType','TradeDate','Value')]))

if(nrow(rtn_stats[rtn_stats$Quantity==sharpe_str&!is.na(rtn_stats$Value)&abs(rtn_stats$Value)>30,])>0){
  rtn_stats[rtn_stats$Quantity==sharpe_str&!is.na(rtn_stats$Value)&abs(rtn_stats$Value)>30,]$Value <- NA  
} 
if(q2_only){
  rtn_stats <- rtn_stats[rtn_stats$TradeDate>='2016-04-01',]
}

sp <- 60/length(unique(rtn_stats[rtn_stats$Trader==11&grepl("Add|New",rtn_stats$TradeType),]$TradeDate))
size_plot_js <- ggplot(rtn_stats[rtn_stats$Trader==11&grepl("Add|New",rtn_stats$TradeType),],aes(x=TradeDate,y=Value,group=Adjusted,colour=Adjusted)) +
                geom_smooth(method="loess",span=sp) +
                geom_smooth(method="gam",se=FALSE) +
                #geom_point() +
                ylab("") +
                ggtitle(paste("JS adjusted trades - ",type_str,sep="")) +
                scale_y_continuous(labels=comma) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
                facet_grid(Quantity~TradeType,scales="free_y")

sp <- 60/length(unique(rtn_stats[rtn_stats$Trader==70&grepl("Add|New",rtn_stats$TradeType),]$TradeDate))
size_plot_ba <- ggplot(rtn_stats[rtn_stats$Trader==70&grepl("Add|New",rtn_stats$TradeType),],aes(x=TradeDate,y=Value,group=Adjusted,colour=Adjusted)) +
                geom_smooth(method="loess",span=sp) +
                geom_smooth(method="gam",se=FALSE) +
                ylab("") +
                ggtitle(paste("BA adjusted trades - ",type_str,sep="")) +
                scale_y_continuous(labels=comma) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
                facet_grid(Quantity~TradeType,scales="free_y")

sp <- 60/length(unique(rtn_stats[rtn_stats$Trader==101&grepl("Add|New",rtn_stats$TradeType),]$TradeDate))
size_plot_dk <- ggplot(rtn_stats[rtn_stats$Trader==101&grepl("Add|New",rtn_stats$TradeType),],aes(x=TradeDate,y=Value,group=Adjusted,colour=Adjusted)) +
                geom_smooth(method="loess",span=sp) +
                geom_smooth(method="gam",se=FALSE) +
                #geom_point() +
                ylab("") +
                ggtitle(paste("DK adjusted trades - ",type_str,sep="")) +
                scale_y_continuous(labels=comma) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
                facet_grid(Quantity~TradeType,scales="free_y")

#----------------------------------------------------
#Overall 'grand average' performance impact

rtn_stats <- aggregate(history_data[c('SkewRtn','SwingRtn','SkewSize','SwingSize')],list(Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)mean(x,na.rm=TRUE))
rtn_stats <- merge(rtn_stats,
                   aggregate(history_data[c('SkewRtn','SwingRtn')],list(Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)sd(x,na.rm=TRUE)),
                   by=c('Trader','TradeType'))
rtn_stats$SkewShrp <- rtn_stats$SkewRtn.x/rtn_stats$SkewRtn.y
rtn_stats$SwingShrp <- rtn_stats$SwingRtn.x/rtn_stats$SwingRtn.y
types <- c('New Long','New Short','Add Long','Add Short')
rtn_stats <- rtn_stats[!is.na(rtn_stats$TradeType)&rtn_stats$TradeType%in%types,]
rtn_plt_data <- rbind(#cbind(data.frame(Quantity="SkewSize",Value=rtn_stats$SkewSize,rtn_stats[c('StockHit','Trader','TradeType')])),
                      #cbind(data.frame(Quantity="SwingSize",Value=rtn_stats$SwingSize,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewReturn",Value=rtn_stats$SkewRtn.x*10000,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingReturn",Value=rtn_stats$SwingRtn.x*10000,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewVol",Value=rtn_stats$SkewRtn.y*10000,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingVol",Value=rtn_stats$SwingRtn.y*10000,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewSharpe",Value=rtn_stats$SkewShrp,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingSharpe",Value=rtn_stats$SwingShrp,rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SharpeIncrease",Value=(100*(rtn_stats$SkewShrp-rtn_stats$SwingShrp)/abs(rtn_stats$SwingShrp)),rtn_stats[c('Trader','TradeType')])),
                      cbind(data.frame(Quantity="SizeIncrease",Value=(100*(rtn_stats$SkewSize-rtn_stats$SwingSize)/abs(rtn_stats$SwingSize)),rtn_stats[c('Trader','TradeType')])))
rtn_plot_all <- ggplot(rtn_plt_data,aes(x=TradeType,fill=TradeType)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("") +
  facet_grid(Quantity~Trader,scales="free_y")

rtn_plt_data[rtn_plt_data$Trader==11,]$Trader='JS'
rtn_plt_data[rtn_plt_data$Trader==70,]$Trader='BA'
rtn_plt_data[rtn_plt_data$Trader==101,]$Trader='DK'

rtn_plt_data$Quantity <- gsub('Swing','Orig',rtn_plt_data$Quantity)
rtn_plt_data$Quantity <- gsub('Skew','Adj',rtn_plt_data$Quantity)

rtn_plot <- ggplot(rtn_plt_data[rtn_plt_data$Trader=='JS'&rtn_plt_data$Quantity!='SizeIncrease',],aes(x=TradeType,fill=TradeType)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  facet_grid(Quantity~Trader,scales="free_y")

rtn_plot <- ggplot(rtn_plt_data[rtn_plt_data$Trader=='BA'&rtn_plt_data$Quantity!='SizeIncrease',],aes(x=TradeType,fill=TradeType)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  facet_grid(Quantity~Trader,scales="free_y")

rtn_plot <- ggplot(rtn_plt_data[rtn_plt_data$Trader=='DK'&rtn_plt_data$Quantity!='SizeIncrease',],aes(x=TradeType,fill=TradeType)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text = element_text(size = 16),axis.title = element_text(size = 16),title = element_text(size = 18),axis.text=element_text(size = 12)) +
  facet_grid(Quantity~Trader,scales="free_y")

