setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../reporting/raid_data_import.r")
source("../common/dataplex.r")
source("prototype_portfolio_core_functions.r")
source("coaching_review_functions.r")
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)
library(RODBC)

trader   <- 101
strats   <- c('DK_LPAT','DK_SPAT','DK_LCORE','DK_SCORE')
dates <- c("2016-05-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
history_data <- market_day_age(history_data)
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]

avg_dwn_cols <- c('Strategy','Instrument','TradeDate','TodayPL','PnLOutof','VolInto','SkewInto','PnLInto','DeltaPL','DeltaSwing','DeltaSkew','MarketValue','Av.MarketValue','Long','PsnLong','CumulativePL','IntegratedPL','MarketRelPL','CumulativeMarketRelPL','ValueUSD','ActiveTodayPL','PsnAge')
hd <- history_data
average_down_trades <- unique(hd[(hd$CumulativePL<0)&!is.na(hd$TradeID)&hd$Age>0,avg_dwn_cols])
average_down_trades <- subset(average_down_trades,(average_down_trades$Long&average_down_trades$PsnLong)|(!average_down_trades$Long&!average_down_trades$PsnLong))
average_down_positions <- unique(average_down_trades$Instrument)
other_trades <- unique(hd[(hd$CumulativePL>0)&!is.na(hd$TradeID),avg_dwn_cols])
pl_frame <- merge(hd,data.frame(Instrument=average_down_positions),by=c('Instrument'))
pl_frame <- unique(pl_frame[c(avg_dwn_cols)])
average_down_trades <- average_down_trades[order(average_down_trades$TradeDate),]
pl_frame <- pl_frame[order(pl_frame$TradeDate),]

average_down_trades$TradeCount <- NA
pl_frame$AveragedDown <- 0
for(ins in average_down_positions){
  ad <- average_down_trades[average_down_trades$Instrument==ins,]
  average_down_trades[average_down_trades$Instrument==ins,]$TradeCount <- 1:nrow(ad)
  for(trade in 1:nrow(ad)){
    pl_frame[pl_frame$Instrument==ins&pl_frame$TradeDate>=ad[trade,]$TradeDate,]$AveragedDown <- min(trade,3)     
    pl <- pl_frame[pl_frame$Instrument==ins&pl_frame$TradeDate>=ad[trade,]$TradeDate,]
    pl_frame[pl_frame$Instrument==ins&pl_frame$TradeDate>=ad[trade,]$TradeDate,]$RebasedActive <- pl$ActiveTodayPL - pl$ActiveTodayPL  
  }
}

avg_dwn <- aggregate(pl_frame[c('TodayPL','MarketRelPL','ActiveTodayPL')],list(Strategy=pl_frame$Strategy,Instrument=pl_frame$Instrument,AveragedDown=pl_frame$AveragedDown),function(x)sum(x,na.rm=TRUE))
ttls    <- aggregate(pl_frame[c('TodayPL','MarketRelPL','ActiveTodayPL')],list(Strategy=pl_frame$Strategy,AveragedDown=pl_frame$AveragedDown),function(x)sum(x,na.rm=TRUE))
ttls_plot <- rbind(cbind(data.frame(Type='PL',Value=ttls$TodayPL),ttls[c('Strategy','AveragedDown')]),
                   cbind(data.frame(Type='Relative',Value=ttls$MarketRelPL),ttls[c('Strategy','AveragedDown')]),
                   cbind(data.frame(Type='Active',Value=ttls$ActiveTodayPL),ttls[c('Strategy','AveragedDown')]))
avg_dwn$Accretive <- avg_dwn$ActiveTodayPL>0
counts <- aggregate(avg_dwn$Instrument,list(Strategy=avg_dwn$Strategy,AveragedDown=avg_dwn$AveragedDown,Accretive=avg_dwn$Accretive),length)
counts[!counts$Accretive,]$x <- -1*counts[!counts$Accretive,]$x

adown_smmry <- ggplot(data=counts[counts$AveragedDown>0&counts$Strategy%in%strats,],aes(x=AveragedDown,fill=Accretive)) +
                geom_bar(aes(weight=x),position="dodge") +
                ylab("Number accretive trades") + 
                xlab("Number times position averaged down") + 
                scale_x_discrete(limits=c("1","2","3+")) +
                ggtitle("Number accretive average down trades (2014-10-01 - 2016-05-01)") +
                theme(text = element_text(size=15)) +
                facet_grid(Strategy~.)

pl_smmry <- ggplot(data=ttls_plot[ttls_plot$AveragedDown>0&ttls_plot$Strategy%in%strats,],aes(x=AveragedDown,fill=Type)) +
  geom_bar(aes(weight=Value),position="dodge") +
  ylab("PL after position averaged down") + 
  xlab("Number times position averaged down") + 
  scale_x_discrete(limits=c("1","2","3+")) +
  ggtitle("PL after down trades (2014-10-01 - 2016-05-01)") +
  theme(text = element_text(size=15)) +
  facet_grid(Strategy~.)
