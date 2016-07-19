setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
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

pl_frame <- position_history[position_history$PsnAgeCategory>0,]
pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','PsnAgeCategory','ActiveTodayPL','PsnAge')])

cum_pl <- aggregate(pl_frame['TodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
cumdata <- data.frame(Age=cum_pl$Age,TotalPL=cumsum(cum_pl$TodayPL))
cum_pnl_age <- plot_ly(cumdata, x = Age, y = TotalPL)

cum_act_pl <- aggregate(pl_frame['ActiveTodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
act_cumdata <- data.frame(Age=cum_act_pl$Age,TotalPL=cumsum(cum_act_pl$ActiveTodayPL))
act_cum_pnl_age <- plot_ly(act_cumdata, x = Age, y = TotalPL)

pnl_cum <- subplot(cum_pnl_age,act_cum_pnl_age,nrows=2)
if(plot_in_viewer){
  pnl_cum
}