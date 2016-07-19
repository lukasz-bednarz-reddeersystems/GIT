setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
plot_in_viewer <- FALSE

#Effect of RSI on initial positions
long_rsi_data = unique(history_data[history_data$Age==0&!is.na(history_data$TradeID)&history_data$Long&history_data$PsnLong,c('PriorRSI14','RSI14','PnLOutof','PsnReturnOutFull','PsnReturnOut','PriceMavg','MidOnEntry','VolInto','PriorClosePrice','ClosePrice')])
long_rsi_data$DeltaRSI <- long_rsi_data$RSI14-long_rsi_data$PriorRSI14
long_rsi_data$DeltaRtn <- long_rsi_data$PsnReturnOutFull-long_rsi_data$PsnReturnOut
long_rsi_data$MidOnEntry[is.na(long_rsi_data$MidOnEntry)] <- (long_rsi_data$PriorClosePrice[is.na(long_rsi_data$MidOnEntry)]+long_rsi_data$ClosePrice[is.na(long_rsi_data$MidOnEntry)])/2
long_rsi_data$Entry <- ((long_rsi_data$MidOnEntry/long_rsi_data$PriceMavg) - 1)/(long_rsi_data$VolInto/10000)
long_rsi_data$Entry <- sign(long_rsi_data$Entry)*sqrt(abs(long_rsi_data$Entry))

rsi_short <- plot_ly(long_rsi_data,x=RSI14, y=PsnReturnOut, mode="markers", color=Entry)
rsi_short <- layout(rsi_short,yaxis = list(title="5d return (bps)"), xaxis = list(title=""), title="14d RSI")
rsi_long  <- plot_ly(long_rsi_data,x=RSI14, y=PsnReturnOutFull, mode="markers", color=Entry)
rsi_long  <- layout(rsi_long,yaxis = list(title="20d return (bps)"), xaxis = list(title=""))
rsi_delta <- plot_ly(long_rsi_data,x=DeltaRSI, y=DeltaRtn, mode="markers", color=Entry)
rsi_delta <- layout(rsi_delta,yaxis = list(title="Diff. return"), xaxis = list(title="Diff. RSI"))
rsi_plot  <- subplot(rsi_short,rsi_long,rsi_delta,nrows=3) %>% layout(showlegend = FALSE)

if(plot_in_viewer){
  rsi_plot
}
