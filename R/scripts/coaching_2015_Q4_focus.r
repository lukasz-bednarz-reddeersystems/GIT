setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_snapshot_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
earnings_prc <- analysis_module_request(kf,resultsday_snapshot_analysis_module_builder) 
earnings_exp <- analysis_module_request(kf,resultsday_exposure_snapshot_analysis_module_builder) 
plot_in_viewer <- TRUE
instruments <- unique(history_data$Instrument)

#Position age -------------------------------------------------------------------
psn_hstry <- categorise_psn_ages(history_data)
position_history <- psn_hstry[[1]]
min_dates <- psn_hstry[[2]]
bstats <- bucket_stats(position_history,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
bucket_totals <- bstats[[1]]
pnl_by_age <- plot_ly(x=bucket_totals$PsnAgeCategory,y=bucket_totals$TodayPL,type="bar")
pnl_by_age <- layout(pnl_by_age,xaxis=list(title="Age Category"),yaxis=list(title="Total PL"))

size_stats <- bucket_stats(position_history[position_history$PsnLong==TRUE,],'MarketValue',function(x)mean(x,na.rm=TRUE),function(x)mean(x,na.rm=TRUE))
size_totals <- size_stats[[1]]
size_stats_shrt <- bucket_stats(position_history[position_history$PsnLong==FALSE,],'MarketValue',function(x)mean(x,na.rm=TRUE),function(x)mean(x,na.rm=TRUE))
size_totals_shrt <- size_stats_shrt[[1]]
size_by_age <- plot_ly(x=size_totals$PsnAgeCategory,y=size_totals$MarketValue,type="bar")
size_by_age <- add_trace(size_by_age,y=size_totals_shrt$MarketValue,type="bar")
size_by_age <- layout(size_by_age,xaxis=list(title="Age Category"),yaxis=list(title="Average value"))

position_history <- market_rel_pl(position_history)
relative_bstats <- bucket_stats(position_history,'ActiveTodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
relative_bucket_totals <- relative_bstats[[1]]

pnl_active <- plot_ly(x=relative_bucket_totals$PsnAgeCategory,y=relative_bucket_totals$ActiveTodayPL,type="bar")
pnl_active <- layout(pnl_active,xaxis=list(title="Age Category"),yaxis=list(title="Active PL"))

psn_age <- subplot(pnl_by_age,size_by_age,pnl_active,nrows=3)
if(plot_in_viewer){
  psn_age
}

strategies <- unique(history_data$Strategy)
pnl_cum <- list()
for(strat in strategies){
  pnl_cum[[strat]] <- tryCatch({
    pl_timescales(history_data[history_data$Strategy==strat,])    
  },error=function(cond){
    message(paste(strat,"failed."))
  })
}

pnl_cum[['All']] <- pl_timescales(history_data)    
pnl_cum[['All_Long']] <- pl_timescales(history_data[history_data$PsnLong==TRUE,])
pnl_cum[['All_Short']] <- pl_timescales(history_data[history_data$PsnLong==FALSE,])

if(plot_in_viewer){
  for(p in pnl_cum)p
}

# Pet names --------------------------------------------------------------------
flat_data <- count_flats(history_data)
bubble_data <- build_bubble_data(flat_data[[2]],history_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sd(x,na.rm=TRUE))
bubble_hist <- plot_ly(bubble_data,x=times_flat,y=n_instruments,mode="markers",color=value,size=alt_value,opacity=0.35)
bubble_hist <- add_trace(bubble_hist,bubble_data,x=times_flat,y=n_instruments,type="bar",opacity=1)
if(plot_in_viewer){
  bubble_hist
}

revisit_data <- create_revisit_data(flat_data[[1]],history_data)
revisit_bucket_data <- build_revisit_plot_data(revisit_data,flat_data[[2]],function(x)sum(x,na.rm=TRUE))
rvp <- plot_ly(data.frame(Visits=1:3,PL=c(revisit_bucket_data[(revisit_bucket_data$TotalVisits==1)&(revisit_bucket_data$Visit==1),'TodayPL'],0,0)),x=Visits,y=PL,type="bar")
rvp <- add_trace(rvp,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==1),'TodayPL'],0))
rvp <- add_trace(rvp,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==2),'TodayPL'],0))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==1),'TodayPL']))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==2),'TodayPL']))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==3),'TodayPL']))
rvp <- layout(rvp,xaxis=list(title="Visits"),yaxis=list(title="Total PL"))

rvt <- plot_ly(data.frame(Visits=1:3,PL=c(revisit_bucket_data[(revisit_bucket_data$TotalVisits==1)&(revisit_bucket_data$Visit==1),'IsTrade'],0,0)),x=Visits,y=PL,type="bar")
rvt <- add_trace(rvt,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==1),'IsTrade'],0))
rvt <- add_trace(rvt,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==2),'IsTrade'],0))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==1),'IsTrade']))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==2),'IsTrade']))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==3),'IsTrade']))
rvt <- layout(rvt,xaxis=list(title="Visits"),yaxis=list(title="N Trades"))

rv <- subplot(rvp,rvt,nrows=2)
if(plot_in_viewer){
  rv
}

#Offside positions -------------------------------------------------------------
offside_list <- integrated_offside(history_data)
offside_data <- offside_list[[1]]
onside_data <- offside_list[[2]]
instruments_overall_offside <- offside_list[[3]]
instruments_not_offside <- offside_list[[4]]
trade_cols <- c('TradeDate','Instrument','TradeID','Name','MarketValue','SkewInto','ValueUSD','VolInto','DeltaPL','DeltaSwing','DeltaSkew','PnLOutof','TodayPL','Age')
all_trades <- history_data[!is.na(history_data$TradeID),]
trades_in_offside_psns <- offside_data[!is.na(offside_data$TradeID)&offside_data$IntegratedPL<0,]
trades_in_offside_psns <- unique(trades_in_offside_psns[trade_cols])
trades_in_onside_psns  <- onside_data[!is.na(onside_data$TradeID)&onside_data$IntegratedPL>=0,]
trades_in_onside_psns  <- unique(trades_in_onside_psns[trade_cols])

n_offside <- plot_ly(x=c('Offside','Total'),y=c(length(instruments_overall_offside),length(instruments)),type="bar")
n_offside <- layout(yaxis=list(title="Number positions"),xaxis=list(title=""))

trades_on <- nrow(unique(trades_in_onside_psns[!is.na(trades_in_onside_psns$TradeID),c('TradeID','TradeDate','MarketValue')]))
trades_off<- nrow(unique(trades_in_offside_psns[!is.na(trades_in_offside_psns$TradeID),c('TradeID','TradeDate','MarketValue')]))

off_on_trades <- plot_ly(x=c('Offside','Onside'),y=c(trades_off,trades_on),type="bar")
off_on_trades <- layout(yaxis=list(title="Number trades"),xaxis=list(title=""))

counts <- subplot(n_offside,off_on_trades,nrows=2)
if(plot_in_viewer){
  counts
}

stats_in <- trade_stats_in(trades_in_offside_psns,trades_in_onside_psns)
if(plot_in_viewer){
  stats_in
}

delta<- trade_delta_stats(trades_in_offside_psns,trades_in_onside_psns)
if(plot_in_viewer){
  delta  
}

history_data$ValueUSD <- ((-1)^(history_data$Long+1))*((-1)^(history_data$PsnLong+1))*history_data$ValueUSD

pl_by_trades_integrated_offside <- group_by_trade_counts_cumulative_pl(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols)
pl_trades_integrated_offside <- group_by_trade_counts_stats(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaPL',function(x)mean(x,na.rm=TRUE),TRUE)
swing_trades_integrated_offside <- group_by_trade_counts_stats(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaSwing',function(x)mean(x,na.rm=TRUE),TRUE)
skew_trades_integrated_offside <- group_by_trade_counts_stats(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaSkew',function(x)mean(x,na.rm=TRUE),TRUE)
value_trades_integrated_offside <- group_by_trade_counts_stats(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'ValueUSD',function(x)mean(x[x!=0],na.rm=TRUE),TRUE)
count_trades_integrated_offside <- group_by_trade_counts_stats(history_data,trades_in_offside_psns,instruments_overall_offside,trade_cols,'TradeDate',function(x)length(x[!is.na(x)]),FALSE,'ValueUSD',function(x)mean(x[x!=0],na.rm=TRUE),TRUE)
if(plot_in_viewer){
  pl_by_trades_integrated_offside
  pl_trades_integrated_offside
  swing_trades_integrated_offside
  skew_trades_integrated_offside
  value_trades_integrated_offside
  count_trades_integrated_offside
}

trades_offside <- history_data[!is.na(offside_data$TradeID)&offside_data$CumulativePL<0,]
instruments_offside <- unique(trades_offside$Instrument)

pl_by_trades_cumulative_offside <- group_by_trade_counts_cumulative_pl(history_data,trades_offside,instruments_offside,trade_cols)
pl_trades_offside <- group_by_trade_counts_stats(history_data,trades_offside,instruments_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaPL',function(x)mean(x,na.rm=TRUE),TRUE)
swing_trades_offside <- group_by_trade_counts_stats(history_data,trades_offside,instruments_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaSwing',function(x)mean(x,na.rm=TRUE),TRUE)
skew_trades_offside <- group_by_trade_counts_stats(history_data,trades_offside,instruments_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'DeltaSkew',function(x)mean(x,na.rm=TRUE),TRUE)
value_trades_offside <- group_by_trade_counts_stats(history_data,trades_offside,instruments_offside,trade_cols,'TodayPL',function(x)mean(x[x!=0],na.rm=TRUE),TRUE,'ValueUSD',function(x)mean(x[x!=0],na.rm=TRUE),TRUE)
count_trades_offside <- group_by_trade_counts_stats(history_data,trades_offside,instruments_offside,trade_cols,'TradeDate',function(x)length(x[!is.na(x)]),FALSE,'ValueUSD',function(x)mean(x[x!=0],na.rm=TRUE),TRUE)
if(plot_in_viewer){
  pl_by_trades_cumulative_offside
  pl_trades_offside
  swing_trades_offside
  skew_trades_offside
  value_trades_offside
  count_trades_offside
}

# Average down -------------------------------------------------------------------
avg_dwn_cols <- c('Instrument','TradeDate','TodayPL','PnLOutof','VolInto','SkewInto','PnLInto','DeltaPL','DeltaSwing','DeltaSkew','MarketValue','Av.MarketValue','Long','PsnLong','CumulativePL','IntegratedPL')
average_down_trades <- unique(history_data[(history_data$CumulativePL<0)&!is.na(history_data$TradeID),avg_dwn_cols])
average_down_trades <- subset(average_down_trades,(average_down_trades$Long&average_down_trades$PsnLong)|(!average_down_trades$Long&!average_down_trades$PsnLong))
average_down_positions <- unique(average_down_trades$Instrument)
other_trades <- unique(history_data[(history_data$CumulativePL>0)&!is.na(history_data$TradeID),avg_dwn_cols])

n_avg_dwn <- plot_ly(x=c('Avg. Down','Total'),y=c(length(average_down_positions),length(instruments)),type="bar")
n_avg_dwn <- layout(yaxis=list(title="Number positions"),xaxis=list(title=""))

n_avg_dwn_trades <- plot_ly(x=c('Avg. Down','Other'),y=c(nrow(average_down_trades),nrow(other_trades)),type="bar")
n_avg_dwn_trades <- layout(yaxis=list(title="Number trades"),xaxis=list(title=""))

n_avg_dwn_bar <- subplot(n_avg_dwn,n_avg_dwn_trades,nrows=2)
if(plot_in_viewer){
  n_avg_dwn_bar  
}

adown <- rank_average_down(average_down_trades)
if(plot_in_viewer){
  adown
}

av_down_cum_pl <- group_by_trade_counts_cumulative_pl(history_data,average_down_trades,average_down_positions,avg_dwn_cols)
if(plot_in_viewer){
  av_down_cum_pl
}
