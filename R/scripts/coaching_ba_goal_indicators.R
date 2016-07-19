setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader   <- 70
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)

first <- TRUE
for(date in dates){
  key_func <- function(){dated_three_monthly_lookback(trader,date)}
  analysis <- analysis_module_request(key_func,history_analysis_module_builder)
  if(first){
    history_data <- analysis@ppmdl@modeldata@data  
  }
  else{
    history_data <- unique(rbind(history_data,analysis@ppmdl@modeldata@data))
  }
}

market_rel_pl <- function(history_data){
  psn_dates <- history_data[!is.na(history_data$TradeID),c('Instrument','TradeDate')]
  instruments <- unique(psn_dates$Instrument)
  first <- TRUE
  for(ins in instruments){
    if(first){
      min_dates <- data.frame(Instrument = ins,TradeDate = min(psn_dates[psn_dates$Instrument==ins,'TradeDate'],na.rm=TRUE))
      first <- FALSE
    }  
    else{
      min_dates <- rbind(min_dates,data.frame(Instrument = ins,TradeDate = min(psn_dates[psn_dates$Instrument==ins,'TradeDate'],na.rm=TRUE)))
    }
  }
  colnames(min_dates) <- c('Instrument','MinDate')
  history_data <- merge(history_data,min_dates,by='Instrument')
  colnames(min_dates) <- c('Instrument','TradeDate')
  initial_holdings <- unique(merge(history_data,min_dates,by=c('Instrument','TradeDate')))
  initial_holdings <- unique(initial_holdings[c('MarketValue','Instrument','ClosePrice')])
  colnames(initial_holdings) <- c('EarliestMarketValue','Instrument','EarliestPrice')
  history_data <- merge(history_data,initial_holdings,by=c('Instrument'),all.x=TRUE)
  history_data$EarliestHolding <- history_data$EarliestMarketValue/history_data$EarliestPrice
  history_data$CurrentPassiveValue <- history_data$EarliestHolding*history_data$ClosePrice
  history_data$CurrentPassivePL <- history_data$CurrentPassiveValue - history_data$EarliestMarketValue
  l <- length(history_data$CurrentPassiveValue)
  history_data$PassiveTodayPL <- c(NA,history_data$CurrentPassiveValue[2:l]*(history_data$ClosePrice[2:l]/history_data$PriorClosePrice[2:l])-history_data$CurrentPassiveValue[2:l])
  history_data$ActiveTodayPL <- history_data$TodayPL - history_data$PassiveTodayPL
  return(history_data)
}
history_data <- market_rel_pl(history_data)

instruments <- unique(history_data$Instrument)
instruments_overall_offside <- c()
instruments_not_offside <- c()
off_first <- TRUE
on_first <- TRUE
for(ins in instruments){
  off <- history_data[history_data$Instrument==ins,'IntegratedPL'][length(history_data[history_data$Instrument==ins,'IntegratedPL'])]
  if(length(off)==0)off<-0
  if(off<0){
    instruments_overall_offside <- c(instruments_overall_offside,ins)
    if(off_first){
      offside_data <- history_data[history_data$Instrument==ins,] 
      off_first <-  FALSE
    }
    else{
      offside_data <- rbind(offside_data,history_data[history_data$Instrument==ins,] )
    }
  }
  else{
    instruments_not_offside <- c(instruments_not_offside,ins)
    if(on_first){
      onside_data <- history_data[history_data$Instrument==ins,] 
      on_first <-  FALSE
    }
    else{
      onside_data <- rbind(onside_data,history_data[history_data$Instrument==ins,] )
    }
  }
}

#Determine which trades have negtive overall integrated PL and which dont
trade_cols <- c('TradeDate','TradeID','Name','MarketValue','SkewInto','ValueUSD','VolInto','DeltaPL','DeltaSwing','DeltaSkew','PnLOutof','TodayPL','Age')
all_trades <- history_data[!is.na(history_data$TradeID),]
trades_in_offside_psns <- offside_data[!is.na(offside_data$TradeID)&offside_data$IntegratedPL<0,]
trades_in_offside_psns <- unique(trades_in_offside_psns[trade_cols])
trades_in_onside_psns  <- onside_data[!is.na(onside_data$TradeID)&onside_data$IntegratedPL>=0,]
trades_in_onside_psns  <- unique(trades_in_onside_psns[trade_cols])

p <- plot_ly(x=c('Offside','Total'),y=c(length(instruments_overall_offside),length(instruments)),type="bar")
p <- layout(title="Offside (absolute)",yaxis=list(title="Number positions"),xaxis=list(title=""))
p

trades_on <- nrow(unique(trades_in_onside_psns[!is.na(trades_in_onside_psns$TradeID),c('TradeID','TradeDate','MarketValue')]))
trades_off<- nrow(unique(trades_in_offside_psns[!is.na(trades_in_offside_psns$TradeID),c('TradeID','TradeDate','MarketValue')]))

t <- plot_ly(x=c('Offside','Onside'),y=c(trades_off,trades_on),type="bar")
t <- layout(title="Trades in positions",yaxis=list(title="Number trades"),xaxis=list(title=""))
t

q <- plot_ly(y=trades_in_offside_psns$SkewInto,type="box")
add_trace(q,y=trades_in_onside_psns$SkewInto,type='box')
q <- layout(title="Skew into trade",yaxis=list(title="Skew"),xaxis=list(title=""))
q

v <- plot_ly(y=trades_in_offside_psns$VolInto,type="box")
add_trace(v,y=trades_in_onside_psns$VolInto,type='box')
v <- layout(title="Vol into trade",yaxis=list(title="Vol"),xaxis=list(title=""))
v

d_pl <- plot_ly(y=trades_in_offside_psns$DeltaPL,type="box")
add_trace(d_pl,y=trades_in_onside_psns$DeltaPL,type='box')
d_pl <- layout(title="Delta PL",yaxis=list(title="PL"),xaxis=list(title=""))
d_pl

d_sw <- plot_ly(y=trades_in_offside_psns$DeltaSwing,type="box")
add_trace(d_sw,y=trades_in_onside_psns$DeltaSwing,type='box')
d_sw <- layout(title="Delta Swing",yaxis=list(title="Swing"),xaxis=list(title=""))
d_sw

d_sk <- plot_ly(y=trades_in_offside_psns$DeltaSkew,type="box")
add_trace(d_sk,y=trades_in_onside_psns$DeltaSkew,type='box')
d_sk <- layout(title="Delta Skew",yaxis=list(title="Skew"),xaxis=list(title=""))
d_sk

po <- plot_ly(y=trades_in_offside_psns$PnLOutof[trades_in_offside_psns$PnLOutof>0],type="box")
add_trace(po,y=trades_in_onside_psns$PnLOutof[trades_in_onside_psns$PnLOutof>0],type='box')
po <- layout(title="PL out",yaxis=list(title="PL"),xaxis=list(title=""))
po

pi <- plot_ly(y=trades_in_offside_psns$PnLOutof[trades_in_offside_psns$PnLOutof<0],type="box")
add_trace(pi,y=trades_in_onside_psns$PnLOutof[trades_in_onside_psns$PnLOutof<0],type='box')
pi <- layout(title="PL out",yaxis=list(title="PL"),xaxis=list(title=""))
pi


pl <- plot_ly(x=c('Offside','Total'),y=c(sum(offside_data$TodayPL,na.rm=TRUE),sum(history_data$TodayPL,na.rm=TRUE)),type="bar")
pl <- layout(yaxis=list(title="Total PL"),xaxis=list(title=""))
pl

sum(trades_in_onside_psns$PassiveTodayPL,na.rm=TRUE)
sum(trades_in_offside_psns$PassiveTodayPL,na.rm=TRUE)

sum(trades_in_onside_psns$ActiveTodayPL,na.rm=TRUE)
sum(trades_in_offside_psns$ActiveTodayPL,na.rm=TRUE)

psns_off <- unique(trades_in_offside_psns[c('Age','Instrument')])
psns_on  <- unique(trades_in_onside_psns[c('Age','Instrument')])

mean(psns_off$Age,na.rm=TRUE)
mean(psns_on$Age,na.rm=TRUE)

ag <- plot_ly(y=psns_off$Age,type="box")
add_trace(ag,y=psns_on$Age,type='box')
ag <- layout(title="Age",yaxis=list(title="Age"),xaxis=list(title=""))
ag

mv <- plot_ly(y=trades_in_offside_psns$MarketValue[trades_in_offside_psns$MarketValue<0],type="box")
add_trace(mv,y=trades_in_onside_psns$MarketValue[trades_in_onside_psns$MarketValue<0],type='box')
mv <- layout(title="Market Value (short)",yaxis=list(title="$"),xaxis=list(title=""))
mv

mvl <- plot_ly(y=trades_in_offside_psns$MarketValue[trades_in_offside_psns$MarketValue>0],type="box")
add_trace(mvl,y=trades_in_onside_psns$MarketValue[trades_in_onside_psns$MarketValue>0],type='box')
mvl <- layout(title="Market Value (long)",yaxis=list(title="$"),xaxis=list(title=""))
mvl


