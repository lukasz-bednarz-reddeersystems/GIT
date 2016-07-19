setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(quantmod)

trader   <- 101
dates <- c("2016-04-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)

trades <- unique(history_data[!is.na(history_data$TradeID),c('Instrument','Strategy','Long','TradeDate','MidOnEntry','PriceMavg','VolInto','DailyN','MavgPrice50','RelativeRSI14','RSI14','TodayPL','MarketValue','Gm.PsnReturn')])
trades$NFromMAVG20 <- scale((trades$MidOnEntry-trades$PriceMavg)/(trades$MidOnEntry*trades$VolInto/10000))
trades$NFromMAVG50 <- scale((trades$MidOnEntry-trades$MavgPrice50)/(trades$MidOnEntry*trades$DailyN/100))
trades$RSI14 <- scale(trades$RSI14)
trades$RelativeRSI14 <- scale(trades$RelativeRSI14)
trades$Return <- trades$TodayPL/abs(trades$MarketValue)

plot_trades <- rbind(cbind(Measure='NFromMAVG20',trades[c('Strategy','Long')],Value=trades$NFromMAVG20),
                     cbind(Measure='NFromMAVG50',trades[c('Strategy','Long')],Value=trades$NFromMAVG50),
                     cbind(Measure='RSI14',trades[c('Strategy','Long')],Value=trades$RSI14),
                     cbind(Measure='RelativeRSI14',trades[c('Strategy','Long')],Value=trades$RelativeRSI14))

extension <- ggplot(data=plot_trades, aes(x=Value, colour=Measure)) +
  geom_freqpoly() +
  facet_grid( Strategy~Long, scales='free_y') +
  ylab("") + xlab("") + ggtitle('Stock extension') 

#Number of trades per month that exceed 2 sigma for distance from MAVG and RelativeRSI

distance_measure <- 'NFromMAVG20'
rsi_measure <- 'RSI14'
thr <- 1

trd <- trades[!is.na(trades[distance_measure])&!is.na(trades[rsi_measure]),]
extended_trades <- trd[trd$Long==1&(trd[distance_measure]>thr|trd[rsi_measure]>thr),]
extended_trades <- rbind(extended_trades,trd[trd$Long==0&(trd[distance_measure]<(-thr)|trd[rsi_measure]<(-thr)),])

getSymbols("^SX5E")
index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])

#1. Number of extended buys and sells in month compared to number market down days
buy_sells <- extended_trades
buy_sells$Month <- format(buy_sells$TradeDate,'%Y-%m')
buy_sells <- aggregate(buy_sells['Instrument'],list(Month=buy_sells$Month,Strategy=buy_sells$Strategy,Long=buy_sells$Long),function(x)length(x))
up_days <- merge(index,extended_trades,by='TradeDate')
up_days$Up_Buy <- up_days$SX5E.Close>0&(up_days$Long==1)
up_days$Up_Sell<- up_days$SX5E.Close>0&(up_days$Long==0)
up_days$Month <-format(up_days$TradeDate,'%Y-%m')
up_days <- aggregate(up_days[c('Up_Buy','Up_Sell')],list(Month=up_days$Month,Strategy=up_days$Strategy),sum)
colnames(buy_sells) <- c('Month','Strategy','Long','Count')
buy_sells$Side <- 'Sell'
buy_sells[buy_sells$Long==1,]$Side <- 'Buy'
buy_sells <- rbind(buy_sells[c('Month','Strategy','Count','Side')],
                   cbind(Side='Up_Buy',up_days[c('Month','Strategy')],Count=up_days$Up_Buy),
                   cbind(Side='Up_Sell',up_days[c('Month','Strategy')],Count=up_days$Up_Sell))

ttls_long <- aggregate(buy_sells[grep("BA_L",buy_sells$Strategy),]$Count,list(Month=buy_sells[grep("BA_L",buy_sells$Strategy),]$Month,Side=buy_sells[grep("BA_L",buy_sells$Strategy),]$Side),sum)
ttls_long$Strategy <- 'Overall Long'
colnames(ttls_long) <- c('Month','Side','Count','Strategy')
ttls_short <- aggregate(buy_sells[grep("BA_S",buy_sells$Strategy),]$Count,list(Month=buy_sells[grep("BA_S",buy_sells$Strategy),]$Month,Side=buy_sells[grep("BA_S",buy_sells$Strategy),]$Side),sum)
ttls_short$Strategy <- 'Overall Short'
colnames(ttls_short) <- c('Month','Side','Count','Strategy')

buy_sells <- rbind(buy_sells,ttls_long)
buy_sells <- rbind(buy_sells,ttls_short)

n_buys_sells <- ggplot(data=buy_sells, aes(x=Month, fill=Side)) +
  geom_bar(aes(weight=Count),position="dodge") +
  facet_grid(Strategy~.) +
  ylab("") + xlab("") + ggtitle('Stock extension') 

#2. Market return, return on day of trade and return out of trade
extended_return <- extended_trades
extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/10000
n_trades <- aggregate(extended_return$Gm.PsnReturn,list(TradeDate=extended_return$TradeDate,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(!is.na(x)))
extended_return <- merge(n_trades,extended_return,by=c('TradeDate','Strategy','Long'))
extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/extended_return$x
extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return')],list(TradeDate=extended_return$TradeDate,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(x,na.rm=TRUE))
extended_return <- merge(extended_return,index,by='TradeDate')
extended_return$Return[is.infinite(extended_return$Return)] <- NA
extended_return_cor <- cor(extended_return$Return,extended_return$SX5E.Close,use="na.or.complete")
extended_return$Month <-format(extended_return$TradeDate,'%Y-%m')
extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return','SX5E.Close')],list(Month=extended_return$Month,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)mean(log(1+x),na.rm=TRUE))
extended_return <- rbind(cbind(Return='Position',extended_return[c('Month','Strategy','Long')],Value=extended_return$Gm.PsnReturn),
                         cbind(Return='Trade',extended_return[c('Month','Strategy','Long')],Value=extended_return$Return),
                         cbind(Return='Index',extended_return[c('Month','Strategy','Long')],Value=extended_return$SX5E.Close))

ttl_rtn_long <- aggregate(extended_return[grep("BA_L",extended_return$Strategy),]$Value,list(Month=extended_return[grep("BA_L",extended_return$Strategy),]$Month,Side=extended_return[grep("BA_L",extended_return$Strategy),]$Long,Return=extended_return[grep("BA_L",extended_return$Strategy),]$Return),mean)
ttl_rtn_long$Strategy <- 'Overall Long'
colnames(ttl_rtn_long) <- c('Month','Long','Return','Value','Strategy')
ttl_rtn_short <- aggregate(extended_return[grep("BA_S",extended_return$Strategy),]$Value,list(Month=extended_return[grep("BA_S",extended_return$Strategy),]$Month,Side=extended_return[grep("BA_S",extended_return$Strategy),]$Long,Return=extended_return[grep("BA_S",extended_return$Strategy),]$Return),mean)
ttl_rtn_short$Strategy <- 'Overall Short'
colnames(ttl_rtn_short) <- c('Month','Long','Return','Value','Strategy')

extended_return <- rbind(extended_return,ttl_rtn_long)
extended_return <- rbind(extended_return,ttl_rtn_short)
extended_return$Value[is.infinite(extended_return$Value)] <- 0
extended_return$Value[is.nan(extended_return$Value)] <- 0

extension_rtns <- ggplot(data=extended_return, aes(x=Month, fill=Return)) +
  geom_bar(aes(weight=Value),position="dodge") +
  facet_grid(Strategy~Long, scales="free_y") +
  ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily log return due to extended trades') 

# focus_return <- extended_return[(extended_return$Long==1&(extended_return$Strategy=='Overall Long'|extended_return$Strategy=='BA_LHYBRID'))|(extended_return$Long==0&(extended_return$Strategy=='Overall Short'|extended_return$Strategy=='BA_SHYBRID')),]
# focus_rtns <- ggplot(data=focus_return, aes(x=Month, fill=Return)) +
#   geom_bar(aes(weight=Value),position="dodge") +
#   facet_grid(Strategy~., scales="free_y") +
#   ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily log return due to extended trades') 

all_trd <- unique(trd[c('Gm.PsnReturn','Return','TradeDate','Strategy','Long')])
all_trd$Gm.PsnReturn <- all_trd$Gm.PsnReturn/10000
n_trades <- aggregate(all_trd$Gm.PsnReturn,list(TradeDate=all_trd$TradeDate,Strategy=all_trd$Strategy,Long=all_trd$Long),function(x)sum(!is.na(x)))
all_trd <- merge(n_trades,all_trd,by=c('TradeDate','Strategy','Long'))
all_trd$Gm.PsnReturn <- all_trd$Gm.PsnReturn/all_trd$x
all_return <- aggregate(all_trd[c('Gm.PsnReturn','Return')],list(TradeDate=all_trd$TradeDate,Strategy=all_trd$Strategy,Long=all_trd$Long),function(x)sum(x,na.rm=TRUE))
all_return$Return[is.infinite(all_return$Return)] <- NA
all_return <- merge(all_return,index,by='TradeDate')
all_return_cor <- cor(all_return$Return,all_return$SX5E.Close,use="na.or.complete")
all_return$Month <-format(all_return$TradeDate,'%Y-%m')
all_return <- aggregate(all_return[c('Gm.PsnReturn','Return')],list(Month=all_return$Month,Strategy=all_return$Strategy,Long=all_return$Long),function(x)mean(log(1+x),na.rm=TRUE))
all_return <- rbind(cbind(Return='All Positions',all_return[c('Month','Strategy','Long')],Value=all_return$Gm.PsnReturn),
                    cbind(Return='All Trades',all_return[c('Month','Strategy','Long')],Value=all_return$Return))
extended_return <- rbind(all_return,extended_return)

focus_return_rel <- extended_return
vframe <- merge(focus_return_rel[focus_return_rel$Return=='Position',],focus_return_rel[focus_return_rel$Return=='All Positions',],by=c('Month','Long','Strategy'))
vframe$Value <- vframe$Value.x - vframe$Value.y
rel_return <- cbind(Return='Position',vframe[c('Month','Long','Strategy','Value')])
vframe <- merge(focus_return_rel[focus_return_rel$Return=='Trade',],focus_return_rel[focus_return_rel$Return=='All Trades',],by=c('Month','Long','Strategy'))
vframe$Value <- vframe$Value.x - vframe$Value.y
dex <- merge(focus_return_rel[focus_return_rel$Return=='Index',],focus_return_rel[focus_return_rel$Return=='All Positions',],by=c('Month','Long','Strategy'))
dex <- cbind(dex[c('Month','Long','Strategy')],Return='Index',Value=dex$Value.x)
rel_return <- rbind(rel_return,cbind(Return='Trade',vframe[c('Month','Long','Strategy','Value')]),dex)

relative_rtns <- ggplot(data=rel_return, aes(x=Month, fill=Return)) +
  geom_bar(aes(weight=Value),position="dodge") +
  facet_grid(Strategy~Long, scales="free_y") +
  ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily relative log return due to extended trades') 

#3 Compute summary quantities
extended_instruments <- extended_trades$Instrument
extended_positions <- merge(data.frame(Instrument=extended_instruments),history_data,by=c('Instrument'))
smmry_cols <- c('SkewInto','SkewOutof','DeltaSwing','DeltaSkew','DeltaPL')
mn_data <- unique(extended_positions[c('Strategy','Instrument','TradeDate',smmry_cols)])
mn_smmry<- aggregate(extended_positions[smmry_cols],list(Strategy=extended_positions$Strategy),function(x)mean(x,na.rm=TRUE))
pl_data <- unique(extended_positions[c('Strategy','Instrument','TradeDate','TodayPL')])
pl_data$Month <- format(pl_data$TradeDate,'%Y-%m')
pl_mnth_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy,Month=pl_data$Month),function(x)sum(x,na.rm=TRUE))
colnames(pl_mnth_smmry) <- c('Strategy','Month','PL')
pl_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy),function(x)sum(x,na.rm=TRUE))
all_data<- unique(history_data[c('Strategy','Instrument','TradeDate','TodayPL')])
all_data$Month <- format(all_data$TradeDate,'%Y-%m')
all_mnth_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy,Month=all_data$Month),function(x)sum(x,na.rm=TRUE))
colnames(all_mnth_pl) <- c('Strategy','Month','PL')
all_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy),function(x)sum(x,na.rm=TRUE))

mnthly_pl <- rbind(cbind(Key='Total',all_mnth_pl),
                   cbind(Key='Extended',pl_mnth_smmry))
mpl <- ggplot(data=mnthly_pl, aes(x=Month, fill=Key)) +
  geom_bar(aes(weight=PL),position="dodge") +
  facet_grid(Strategy~.) +
  ylab("PL") + xlab("Month") + ggtitle('PL in positions featuring extended trades') 
