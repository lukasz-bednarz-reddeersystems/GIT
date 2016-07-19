setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
history_data <- market_rel_pl(history_data)

#Position size evolution vs PL ------------------------------
psn_hstry <- categorise_psn_ages(history_data)
position_history <- new_psns(psn_hstry[[1]])

pl_frame <- position_history[position_history$PsnAgeCategory>0,]
pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','PsnAgeCategory','ActiveTodayPL','PsnAge','MarketValue')])

cum_pl <- aggregate(pl_frame['TodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
cumdata <- data.frame(Age=cum_pl$Age,TotalPL=cumsum(cum_pl$TodayPL))
cum_pnl_age <- plot_ly(cumdata, x = Age, y = TotalPL)

cum_act_pl <- aggregate(pl_frame['ActiveTodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
act_cumdata <- data.frame(Age=cum_act_pl$Age,TotalPL=cumsum(cum_act_pl$ActiveTodayPL))
act_cum_pnl_age <- plot_ly(act_cumdata, x = Age, y = TotalPL)

cum_sze <- aggregate(pl_frame['MarketValue'],list(Age=pl_frame$PsnAge),function(x)mean(abs(x),na.rm=TRUE))
cum_sze <- plot_ly(cum_sze, x = Age, y = MarketValue)

pnl_cum <- subplot(cum_pnl_age,act_cum_pnl_age,nrows=2)
