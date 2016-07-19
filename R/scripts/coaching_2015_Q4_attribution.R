setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_snapshot_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
plot_in_viewer <- TRUE
strategies <- unique(history_data$Strategy)

pnl_cum <- list()
for(strat in strategies){
  pnl_cum[[strat]] <- tryCatch({
    pl_timescales(history_data[history_data$Strategy==strat,],data=TRUE)    
  },error=function(cond){
    message(paste(strat,"failed."))
  })
}

pnl_cum[['All']] <- pl_timescales(history_data,data=TRUE)    
pnl_cum[['All_Long']] <- pl_timescales(history_data[history_data$PsnLong==TRUE,],data=TRUE)
pnl_cum[['All_Short']] <- pl_timescales(history_data[history_data$PsnLong==FALSE,],data=TRUE)

data <- pnl_cum[['All']][[8]]
categorised <- attribution_categories(data)

psn_cat_on <- position_category_breakdown(categorised[categorised$CumulativePL>0,])
totals_on <- psn_cat_on[[1]]
psn_cat_data_on <- psn_cat_on[[2]]

cormat_on <- compute_stock_rank_cor(psn_cat_data_on)

psn_cat_off <- position_category_breakdown(categorised[categorised$CumulativePL<0,])
totals_off <- psn_cat_off[[1]]
psn_cat_data_off <- psn_cat_off[[2]]

totals <- subplot(totals_on,totals_off,nrows=2)

cormat_off <- compute_stock_rank_cor(psn_cat_data_off)


