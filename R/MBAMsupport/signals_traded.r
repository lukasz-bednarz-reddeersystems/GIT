setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules_legacy/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../scripts/coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(plotly)

traders   <- c(101,11,70)
dates <- c("2016-07-01")
start_date <- c("2016-04-01")
search <- FALSE
search_for <- 'Gap Up'
signal_path <- 'C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data'

first <- TRUE
for(t in traders){
  kf <- function()dated_four_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_four_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    if(!search){
      trader_signals <- cbind(Trader=t,read.csv(paste(signal_path,"/",t,"_signals.csv",sep=""),header=FALSE)) 
      colnames(trader_signals) <- c('Trader','Name')
    }
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_four_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))  
    if(!search){
      ss <- read.csv(paste(signal_path,"/",t,"_signals.csv",sep=""),header=FALSE)
      colnames(ss) <- c('Name')
      trader_signals <- rbind(trader_signals,cbind(Trader=t,ss))
    }
  }
}

history_data <- market_rel_pl(history_data,trade_rel=FALSE)
#saveRDS(history_data,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/Q2_2016_history.rds")
history_data <- market_day_age(history_data)
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]

all_signals <- get_single_url(middleware_urls@all_razor_fns)
if(search){
  signal_subset <- all_signals[grep(search_for,all_signals$Name),c('Name','ID')]  
} else {
  signal_subset <- merge(trader_signals,all_signals[c('Name','ID')],by='Name')
}
traded_dates <- unique(all_trades$TradeDate[all_trades$TradeDate>=start_date])

first <- TRUE
for(d in traded_dates){
  for(s in unique(signal_subset$ID)){
    #Switched to single URL because its faster and no need to cache... 
    wlist <- tryCatch({get_single_url(paste('http://raidapp2:8083/watchlists/razorlist?id=11&fn=',s,'&date=',as.Date(d),sep=""))},error=function(cond){})
    if(length(wlist)>0){
      if(nrow(wlist)>0){
        instruments <- cbind(Date=d,wlist[c('InstrumentID')])  
      }  
    }
    #instruments <- data_request("watchlist",data.frame(TraderID=11,WatchlistID=s,Date=as.Date(d)),c('InstrumentID'))
    #instruments <- instruments@data
    #Trader ID here does not effect the result... effectively just a dummy variable
    if(nrow(instruments)>0){
      if(first){
        watchlist_content <- cbind(Signal=s,instruments)
        first <- FALSE
      }
      else{
        watchlist_content <- rbind(watchlist_content,cbind(Signal=s,instruments))
      } 
    }
  }
}
#saveRDS(watchlist_content,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/Q2_2016_watchlists.rds")

all_psns <- history_data
all_trades <- unique(all_psns[!is.na(all_psns$TradeID),c('Strategy','Instrument','TradeDate','TodayPL')])
all_psns <- unique(all_psns[is.na(all_psns$TradeID),c('Strategy','Instrument','TradeDate','TodayPL')])

watchlist_content <- merge(watchlist_content,signal_subset,by.x='Signal',by.y='ID')
all_trades <- merge(all_trades,watchlist_content,by.x=c('TradeDate','Instrument'),by.y=c('Date','InstrumentID'))
n_trades_by_signal <- aggregate(all_trades$Instrument,list(Signal=all_trades$Name,Strategy=all_trades$Strategy),length)
all_psns <- merge(all_psns,watchlist_content,by.x=c('TradeDate','Instrument'),by.y=c('Date','InstrumentID'),all.x=TRUE)
n_psns_by_signal <- aggregate(all_psns$Instrument,list(Signal=all_psns$Name,Strategy=all_psns$Strategy),length)
pl_by_signal <- aggregate(all_trades$TodayPL,list(Signal=all_trades$Name,Strategy=all_trades$Strategy),sum)
psn_pl_by_signal <- aggregate(all_psns$TodayPL,list(Signal=all_psns$Name,Strategy=all_psns$Strategy),sum)
all_signals <- aggregate(watchlist_content$InstrumentID,list(Signal=watchlist_content$Name,Trader=watchlist_content$Trader),length)
colnames(all_signals) <- c('Signal','Trader','Count')

trader <- 'JS'
trader_signals <- all_signals[all_signals$Trader==11,c('Signal','Count')]
exclude <- c('JS_LHEDGE','JS_SHEDGE')
#1. Most traded
colnames(n_trades_by_signal) <- c('Signal','Strategy','Trades')
trader_by_signal <- n_trades_by_signal[grep(trader,n_trades_by_signal$Strategy),]
most_traded <- merge(trader_by_signal,trader_signals,by=c('Signal'))
most_traded$PctHit<- 100*most_traded$Trades/most_traded$Count

#2. Most held
colnames(n_psns_by_signal) <- c('Signal','Strategy','Trades')
psn_by_signal <- n_psns_by_signal[grep(trader,n_psns_by_signal$Strategy),]
most_held <- merge(psn_by_signal,trader_signals,by=c('Signal'))
most_held$PctHit <- 100*most_held$Trades/most_held$Count

first <- TRUE
for(strat in unique(most_held$Strategy)){
  mh <- most_held[psn_by_signal$Strategy==strat,]
  if(first){
    top_held <- head(mh[order(mh$PctHit),],n=10)    
    first <- FALSE
  } else {
    top_held <- rbind(top_held,head(mh[order(mh$PctHit),],n=10))
  }
}
first <- TRUE
for(strat in unique(most_traded$Strategy)){
  mt <- most_traded[most_traded$Strategy==strat,]
  if(first){
    top_traded <- head(mt[order(mt$PctHit),],n=10)    
    first <- FALSE
  } else {
    top_traded <- rbind(top_traded,head(mt[order(mt$PctHit),],n=10))
  }
}
top_held_traded <- rbind(cbind(Type='Held',top_held),
                         cbind(Type='Traded',top_traded))

ggplot(top_held_traded[!top_held_traded$Strategy%in%exclude,],aes(x=Signal,fill=Type)) +
  geom_bar(aes(weight=PctHit),position="dodge") +
  ylab("") + xlab("") +
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = 1)) +
  facet_grid(Strategy~.,scales="free_y")

#3. Most least/profitable traded, total and per trade
all_trades <- merge(all_trades,watchlist_content,by.x=c('TradeDate','Instrument'),by.y=c('Date','InstrumentID'))
n_trades_by_signal <- aggregate(all_trades$Instrument,list(Signal=all_trades$Name,Strategy=all_trades$Strategy),length)

pl <- rbind(cbind(Quantity='Position count',Type='Fired',n_psns_by_signal),
            cbind(Quantity='Position count',Type='Traded',n_trades_by_signal),
            cbind(Quantity='Day0 PL',Type='Fired',psn_pl_by_signal),
            cbind(Quantity='Day0 PL',Type='Traded',pl_by_signal))
ggplot(pl,aes(x=reorder(Signal,x),fill=reorder(Strategy,x))) +
  geom_bar(aes(weight=x)) +
  ylab("") + xlab("") + labs(fill="Strategy") +
  theme(axis.text.x = element_text(size=10,angle = 90, hjust = 1)) +
  facet_grid(Quantity~Type,scales="free_y")




