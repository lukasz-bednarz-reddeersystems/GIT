setwd("C:/Development/TradingEnhancementEngine/R/MBAMsupport")
options(modifiedOnlySource=TRUE)
library(R.utils)
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
library(ggplot2)
library(plotly)
library(GGally)

traders   <- c(101,11,70)
dates <- c("2016-05-01")
exclude <- c("DK_SHDG","DK_LHDG","BA_SHEDGE","BA_LHEDGE","JS_SHEDGE","JS_LHEDGE")

first <- TRUE
for(t in traders){
  kf <- function()dated_eighteen_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))  
  }
}
history_data <- history_data[!history_data$Strategy%in%exclude,]
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
scale_data <- c('ValueUSD','VolInto','VolOutof','SkewInto','SkewOutof','CompoundReturnInto','CompoundReturnOutof')
cols <- c('Trader','TradeDate','Strategy','Instrument',scale_data,'RSI14','TodayPL','Long','MarketValue','EarliestMarketValue','MarketRelPL','MinDate','PnLOutof')
history_data <- position_age_from_flats(history_data,cols)
#history_data=readRDS("C:/Development/TradingEnhancementEngine/R/model_data/history_data.rds")
#PnlOutof is missing on some trades for some reason... replace it in this case with TodayPL
history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$PnLOutof <- history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$TodayPL
history_data <- trade_typer(history_data)

history_data$PsnLong <- history_data$MarketValue>0
history_data <- switch_direction(history_data,'SkewInto')
history_data <- switch_direction(history_data,'SkewOutof')
history_data <- switch_direction(history_data,'CompoundReturnInto')
history_data <- switch_direction(history_data,'CompoundReturnOutof')

initial_trades <- unique(history_data[!is.na(history_data$TradeID)&(history_data$TradeType=='New Long'|history_data$TradeType=='New Short'),c(cols,'PsnIncreased','TradeType')])
increases <- unique(history_data[!is.na(history_data$TradeID)&(history_data$TradeType=='Add Long'|history_data$TradeType=='Add Short'),c(cols,'PsnIncreased','TradeType')])

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
       #geom_point(mapping = aes_string(colour="PsnIncreased")) + 
       geom_point() + 
       geom_smooth(method=method,mapping = aes_string(colour="PsnIncreased"), ...)
  return(p)
}
density_plot_kernel <- function(data, mapping, ...){
  hitrate <- mean(data$PnLOutof>0,na.rm=TRUE)*100
  losers  <- mean(data[data$PnLOutof<0,]$PnLOutof,na.rm=TRUE)
  winners <- mean(data[data$PnLOutof>0,]$PnLOutof,na.rm=TRUE)
  p <- ggplot(data = data, mapping = mapping) + 
       geom_density(mapping = aes_string(colour="PsnIncreased"), fill=NA) +
       annotate("text",x=2.5,y=0.5,label=paste(round(hitrate),"%",sep="")) +
       annotate("text",x=2.5,y=0.4,label=paste("Win:",round(winners),sep="")) +
       annotate("text",x=2.5,y=0.3,label=paste("Loss:",round(losers),sep=""))
  return(p)
}

pcols <- c('VolIntoZscore','VolOutofZscore','SkewIntoZscore','SkewOutofZscore','CompoundReturnIntoZscore','CompoundReturnOutofZscore','ValueUSDZscore')
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
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS position increases")

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
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK position increases")

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
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA position increases")

#Examine specific trade group performance
initial_trades <- rbind(initial_trades,increases)
initial_trades$Month <- format(initial_trades$TradeDate,"%Y-%m")
initial_trades$Trader <- substr(initial_trades$Strategy,1,2)
initial_trades$TraderTradeType <- paste(initial_trades$Trader,initial_trades$TradeType,sep=" ") 

trade_stats <- data_fractile(initial_trades,'ValueUSD',10,'Decile',c('TraderTradeType','Month'))
trade_stats$StockHit <- trade_stats$CompoundReturnOutof > 0
trade_stats$PsnHit <- trade_stats$CompoundReturnOutof > 0
ext <- trade_stats$PnLOutof/abs((trade_stats$CompoundReturnOutof/10000)*trade_stats$ValueUSD)
trade_stats$Extraction <- sign(ext)*log(abs(ext))
trade_stats$PsnWin <- NA
trade_stats$PsnWin[trade_stats$PnLOutof>0] <- trade_stats$PnLOutof[trade_stats$PnLOutof>0]
trade_stats$PsnLoss <- NA
trade_stats$PsnLoss[trade_stats$PnLOutof<0] <- trade_stats$PnLOutof[trade_stats$PnLOutof<0]

#1. Overall

trade_stat_package <- c('VolInto','SkewInto','VolOutof','SkewOutof','ValueUSD','CompoundReturnOutof','PnLOutof','RSI14','StockHit','PsnHit','Extraction','ValueUSDDecile_N')
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

trade_stat_package <- c('VolInto','SkewInto','VolOutof','ValueUSD','CompoundReturnOutof','PsnWin','PsnLoss','RSI14','PsnHit','Extraction','ValueUSDDecile_N','PnLOutof')
agg_stats <- aggregate(trade_stats[trade_stat_package],list(TradeDate=trade_stats$TradeDate,TraderTradeType=trade_stats$TraderTradeType,Decile=ifelse(trade_stats$TraderTradeTypeMonthGroupDecile_N==1,'Bottom',ifelse(trade_stats$TraderTradeTypeMonthGroupDecile_N==10,'Top','2-9'))),function(x)mean(x,na.rm=TRUE))
agg_stats$PayOff <- agg_stats$PsnWin/agg_stats$PsnLoss
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
maxday <- plot_data[c('TradeDate')]
maxday$Month <- paste(format(maxday$TradeDate,"%Y-Q"),quarter(maxday$TradeDate),sep="")
maxday <- aggregate(maxday['TradeDate'],list(Month=maxday$Month),function(x)max(x,na.rm=TRUE))

monthly_agg <- aggregate(plot_data['Value'],list(Month=paste(format(plot_data$TradeDate,"%Y-Q"),quarter(plot_data$TradeDate),sep=""),Metric=plot_data$Metric,TraderTradeType=plot_data$TraderTradeType),function(x)mean(x,na.rm=TRUE))
colnames(monthly_agg)[colnames(monthly_agg)=='Value'] <- 'AggValue'
monthly_agg <- merge(monthly_agg,maxday,by='Month')
monthly_agg <- merge(monthly_agg[c('Month','TraderTradeType','Metric','AggValue','TradeDate')],plot_data,by=c('TradeDate','TraderTradeType','Metric'))
monthly_agg <- aggregate(monthly_agg[c('Value','AggValue')],list(Month=monthly_agg$Month,TraderTradeType=monthly_agg$TraderTradeType,Metric=monthly_agg$Metric,TradeDate=monthly_agg$TradeDate),function(x)mean(x,na.rm=TRUE))
monthly_agg$Value <- 1.5*monthly_agg$AggValue

trader='BA'
vars <- c('VolInto','VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','PnLOutof')
qtile_smmry <- ggplot(plot_data[substr(plot_data$TraderTradeType,1,2)==trader&substr(plot_data$TraderTradeType,8,11)=='Long'&plot_data$Metric%in%vars,c('TradeDate','Decile','Value','TraderTradeType','Metric')],aes(x=TradeDate,y=Value,group=Decile,colour=Decile)) +
  geom_smooth() +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~TraderTradeType,scales="free_y")

trader='JS'
vars <- c('VolInto','VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','SkewInto','PnLOutof')
qtile_smmry <- ggplot(plot_data[substr(plot_data$TraderTradeType,1,2)==trader&substr(plot_data$TraderTradeType,8,11)=='Long'&plot_data$Metric%in%vars,c('TradeDate','Decile','Value','TraderTradeType','Metric')],aes(x=TradeDate,y=Value,group=Decile,colour=Decile)) +
  geom_smooth() +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~TraderTradeType,scales="free_y")

trader='DK'
vars <- c('VolInto','VolOutof','ValueUSD','PsnWin','PsnLoss','PsnHit','SkewInto','PnLOutof')
qtile_smmry <- ggplot(plot_data[substr(plot_data$TraderTradeType,1,2)==trader&substr(plot_data$TraderTradeType,8,11)=='Long'&plot_data$Metric%in%vars,c('TradeDate','Decile','Value','TraderTradeType','Metric')],aes(x=TradeDate,y=Value,group=Decile,colour=Decile)) +
  geom_smooth() +
  #geom_text(data=monthly_agg, aes(x=TradeDate, y=Value, label=paste(Month,sprintf("%.1f",monthly_agg$AggValue))), 
  #          colour="black", inherit.aes=FALSE, parse=FALSE, angle=30)  +
  facet_grid(Metric~TraderTradeType,scales="free_y")

#3. Reduced PM/recent time market/stock colour view
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

history_data$SegmentPL <- NA
for(strategy in unique(history_data$Strategy)){
  for(instrument in unique(history_data[history_data$Strategy==strategy,]$Instrument)){
    for(visit in unique(history_data[history_data$Strategy==strategy&history_data$Instrument==instrument,]$Visit)){
      df <- history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit,]
      min_date <- min(df[grepl('New',df$TradeType),]$TradeDate,na.rm=TRUE)
      add_date <- min(df[grepl('Add',df$TradeType),]$TradeDate,na.rm=TRUE)
      if(!is.na(as.character(min_date))){
        if(!is.na(as.character(add_date))){
          pl <- sum(df[df$TradeDate<add_date,]$TodayPL,na.rm=TRUE)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentPL <- pl
          pl <- sum(df[df$TradeDate>=add_date,]$TodayPL,na.rm=TRUE)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==add_date,]$SegmentPL <- pl
        } else {
          pl <- sum(df[df$TradeDate>=min_date,]$TodayPL,na.rm=TRUE)
          history_data[history_data$Strategy==strategy&history_data$Instrument==instrument&history_data$Visit==visit&history_data$TradeDate==min_date,]$SegmentPL <- pl
        } 
      }
    }
  }
}
saveRDS(history_data,"C:/Development/TradingEnhancementEngine/R/model_data/history_data_segmentPL.rds")
segment_pl_plot <- aggregate(history_data['SegmentPL'],list(Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)sum(x,na.rm=TRUE))
df <- history_data[year(history_data$TradeDate)=='2016',]
segment_2016_pl_plot <- aggregate(df['SegmentPL'],list(Trader=df$Trader,TradeType=df$TradeType),function(x)sum(x,na.rm=TRUE))
#plot for whole data set and just for 2016
types <- c('New Long','New Short','Add Long','Add Short')
segment_pl_plot <- segment_pl_plot[segment_pl_plot$TradeType%in%types,]
segment_2016_pl_plot <- segment_2016_pl_plot[segment_2016_pl_plot$TradeType%in%types,]
segment_pl_plot <- rbind(cbind(Start=paste('Since',min(history_data$TradeDate)),segment_pl_plot),
                         cbind(Start='Since 2016-01-01',segment_2016_pl_plot))
seg_plot <- ggplot(segment_pl_plot,aes(x=TradeType,fill=TradeType)) +
            geom_bar(aes(weight=SegmentPL),position="dodge") +
            ylab("PL $") +
            facet_grid(Start~Trader,scales="free_y")

#4. How trading could be adjusted? (trade level bucket risk/return)
history_data <- readRDS("C:/Development/TradingEnhancementEngine/R/model_data/history_data_segmentPL.rds")
history_data$CompoundReturnOutof <- history_data$CompoundReturnOutof*(-1)^(1+grepl('Long',history_data$TradeType))
history_data$SkewInto <- history_data$SkewInto*(-1)^(1+grepl('Long',history_data$TradeType))
history_data$StockHit <- history_data$CompoundReturnOutof > 0
#swing_level <- 20000
history_data$skew_coeff <- 0
history_data$rebal <- 1
history_data[history_data$Trader==11,]$rebal <- 0.9
history_data[history_data$Trader==11,]$skew_coeff <- -5
history_data[history_data$Trader==70,]$rebal <- 0.75
history_data[history_data$Trader==70,]$skew_coeff <- 5
history_data$SwingSize <- history_data$ValueUSD#swing_level/(history_data$VolInto/10000)
#history_data$SkewSize <- swing_level*(1+sign(history_data$SkewInto)*(abs(history_data$SkewInto)^0.3)*(history_data$VolInto/10000))/(history_data$VolInto/10000)
history_data$Rebal <- history_data$ValueUSD 
history_data[grepl('New',history_data$TradeType),]$Rebal <- history_data[grepl('New',history_data$TradeType),]$ValueUSD*history_data[grepl('New',history_data$TradeType),]$rebal
history_data[grepl('Add',history_data$TradeType),]$Rebal <- history_data[grepl('Add',history_data$TradeType),]$ValueUSD*(1/history_data[grepl('Add',history_data$TradeType),]$rebal)
rb <- sign(history_data$SkewInto)*(abs(history_data$SkewInto)^0.3)*(history_data$VolInto/10000)
rb[is.na(rb)]<-0
history_data$SkewSize <- history_data$Rebal*(1+history_data$skew_coeff*rb)
if(length(is.infinite(history_data$SkewSize))>0)history_data$SkewSize[is.infinite(history_data$SkewSize)] <- NA
if(length(is.infinite(history_data$SwingSize))>0)history_data$SwingSize[is.infinite(history_data$SwingSize)] <- NA
history_data$SwingSizePL <- history_data$SwingSize*(history_data$CompoundReturnOutof/10000)
history_data$SkewSizePL <- history_data$SkewSize*(history_data$CompoundReturnOutof/10000)
total_trd_capital <- aggregate(history_data[c('SkewSize','SwingSize')],list(TradeDate=history_data$TradeDate,Trader=history_data$Trader),function(x)sum(x,na.rm=TRUE))
colnames(total_trd_capital) <- c('TradeDate','Trader','TotalSkewSize','TotalSwingSize')
history_data <- merge(history_data,total_trd_capital,by=c('TradeDate','Trader'))
history_data$SwingRtn <- history_data$SwingSizePL/history_data$TotalSwingSize
history_data$SkewRtn <- history_data$SkewSizePL/history_data$TotalSkewSize
rtn_stats <- aggregate(history_data[c('SkewRtn','SwingRtn','SkewSize','SwingSize')],list(StockHit=history_data$StockHit,Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)mean(x,na.rm=TRUE))
rtn_stats <- merge(rtn_stats,
                   aggregate(history_data[c('SkewRtn','SwingRtn')],list(StockHit=history_data$StockHit,Trader=history_data$Trader,TradeType=history_data$TradeType),function(x)sd(x,na.rm=TRUE)),
                   by=c('StockHit','Trader','TradeType'))
rtn_stats$SkewShrp <- rtn_stats$SkewRtn.x/rtn_stats$SkewRtn.y
rtn_stats$SwingShrp <- rtn_stats$SwingRtn.x/rtn_stats$SwingRtn.y
types <- c('New Long','New Short','Add Long','Add Short')
rtn_stats <- rtn_stats[!is.na(rtn_stats$TradeType)&rtn_stats$TradeType%in%types,]
rtn_plt_data <- rbind(#cbind(data.frame(Quantity="SkewSize",Value=rtn_stats$SkewSize,rtn_stats[c('StockHit','Trader','TradeType')])),
                      #cbind(data.frame(Quantity="SwingSize",Value=rtn_stats$SwingSize,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewReturn",Value=rtn_stats$SkewRtn.x*10000,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingReturn",Value=rtn_stats$SwingRtn.x*10000,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewVol",Value=rtn_stats$SkewRtn.y*10000,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingVol",Value=rtn_stats$SwingRtn.y*10000,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SkewSharpe",Value=rtn_stats$SkewShrp,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SwingSharpe",Value=rtn_stats$SwingShrp,rtn_stats[c('StockHit','Trader','TradeType')])),
                      cbind(data.frame(Quantity="SharpeIncrease",Value=(100*(rtn_stats$SkewShrp/rtn_stats$SwingShrp)-100),rtn_stats[c('StockHit','Trader','TradeType')])))
rtn_plot <- ggplot(rtn_plt_data,aes(x=StockHit,fill=TradeType)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("") +
  facet_grid(Quantity~Trader,scales="free_y")






