setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_snapshot_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_eventdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 70
dates <- c("2016-01-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
earnings_prc <- analysis_module_request(kf,resultsday_snapshot_analysis_module_builder) 
earnings_exp <- analysis_module_request(kf,resultsday_exposure_snapshot_analysis_module_builder) 
plot_in_viewer <- TRUE

#Trades over numbers ----------------------------------------------------------
earnings_cols <-  c("TradeID","PnLInto","PnLOutof","VolInto","VolOutof","Av.MarketValue","SkewInto","SkewOutof","TodayPL","PsnLong","Long","NewPosition","Hit1D")
earnings_data <- earnings_prc@ppmdl@modeldata@data
earnings_data <- earnings_data[earnings_data$DaysSinceLastResults==0,earnings_cols]
if(plot_in_viewer){
  #New longs
  plot_price_snapshot(earnings_prc,earnings_exp,earnings_data,2,TRUE,TRUE,TRUE)
}

#Postions held over numbers ----------------------------------------------------
earnings_psns <- analysis_module_request(kf,results_daypsn_analysis_module_builder)
psn_data  <- earnings_psns@ppmdl@modeldata@data
hit_rates <- aggregate(psn_data$Hit1D,list(Long=psn_data$Long,PsnTraded=psn_data$PsnTraded),function(x)mean(x,na.rm=TRUE))
outcomes  <- aggregate(psn_data[c('TodayPL','PnLOutof','MarketValue','PsnReturn')],list(Long=psn_data$Long,PsnTraded=psn_data$PsnTraded,Hit1D=psn_data$Hit1D),function(x)mean(x,na.rm=TRUE))

