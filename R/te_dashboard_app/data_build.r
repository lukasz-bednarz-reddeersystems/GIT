setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("coaching_review_functions.r")

traders <- c(70,101,11)
dates   <- c("2016-05-01")
cols    <- c("TodayPL","MarketRelPL","ActiveTodayPL","PassiveTodayPL")
history_data <- load_and_compute_market_rel(dated_eighteen_monthly_lookback,traders,dates)
saveRDS(history_data,"../te_dashboard_app/history_data.rds")
position_data <- position_age_from_flats(history_data,cols,abslimit=0,rellimit=0)
saveRDS(position_data,"../te_dashboard_app/position_data.rds")

cols <- c('Trader','TradeDate','Strategy','Instrument','ValueUSD','VolInto','VolOutof','SkewInto','SkewOutof','CompoundReturnInto','CompoundReturnOutof','RSI14','TodayPL','Long','MarketValue','EarliestMarketValue','MarketRelPL','MinDate','PnLOutof')
history_data <- position_age_from_flats(history_data,cols)
#PnlOutof is missing on some trades for some reason... replace it in this case with TodayPL
history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$PnLOutof <- history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$TodayPL
history_data$TradeType <- 'NA'
history_data$TradeType[history_data$Long==1&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Increase'
history_data$TradeType[(history_data$Long==0)&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Decrease'
history_data$TradeType[history_data$Long==1&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Decrease'
history_data$TradeType[(history_data$Long==0)&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Increase'
history_data$TradeType[(history_data$Long==1)&history_data$PsnAge==0] <- 'OpenLong'
history_data$TradeType[(history_data$Long==0)&history_data$PsnAge==0] <- 'OpenShort'
history_data$Hit <- history_data$PnLOutof > 0
history_data$StockHit <- history_data$CompoundReturnOutof > 0
history_data$StockWin <- NA
history_data$StockLoss <- NA
history_data$StockWin[which(history_data$CompoundReturnOutof>0)] <- history_data$CompoundReturnOutof[which(history_data$CompoundReturnOutof>0)]
history_data$StockLoss[which(history_data$CompoundReturnOutof<0)] <- history_data$CompoundReturnOutof[which(history_data$CompoundReturnOutof<0)]
psn_increased <- aggregate(history_data$TradeType,list(Strategy=history_data$Strategy,Visit=history_data$Visit,Instrument=history_data$Instrument),function(x)sum(x=='Increase',na.rm=TRUE)>0)
colnames(psn_increased) <- c('Strategy','Visit','Instrument','PsnIncreased')
history_data <- merge(history_data,psn_increased,by=c('Strategy','Visit','Instrument'),all.x=TRUE)

switch_direction <- function(history_data,column){
  history_data[column] <- -1^(1+history_data$PsnLong)*history_data[column]
  return(history_data)
}

history_data$PsnLong <- history_data$MarketValue>0
history_data <- switch_direction(history_data,'SkewInto')
history_data <- switch_direction(history_data,'SkewOutof')
history_data <- switch_direction(history_data,'CompoundReturnInto')
history_data <- switch_direction(history_data,'CompoundReturnOutof')
saveRDS(history_data,"../te_dashboard_app/trade_data.rds")