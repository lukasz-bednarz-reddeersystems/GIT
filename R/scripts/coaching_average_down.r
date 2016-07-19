setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 101
dates <- c("2016-01-01")

kf <- function()dated_twelve_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
plot_in_viewer <- TRUE
instruments <- unique(history_data$Instrument)

# Average down -------------------------------------------------------------------
avg_dwn_cols <- c('Instrument','TradeDate','TodayPL','PnLOutof','VolInto','SkewInto','PnLInto','DeltaPL','DeltaSwing','DeltaSkew','MarketValue','Av.MarketValue','Long','PsnLong','CumulativePL','IntegratedPL')
average_down_trades <- unique(history_data[(history_data$CumulativePL<0)&!is.na(history_data$TradeID)&history_data$Age>0,avg_dwn_cols])
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

adown <- rank_offside(average_down_trades)
if(plot_in_viewer){
  adown
}

av_down_cum_pl <- group_by_trade_counts_cumulative_pl(history_data,average_down_trades,average_down_positions,avg_dwn_cols)
if(plot_in_viewer){
  av_down_cum_pl
}
