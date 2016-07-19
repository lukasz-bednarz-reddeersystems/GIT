setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 70
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
history_data <- market_rel_pl(history_data)
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]
all_cuml_offside_data <- integrated_offside(history_data,type="CumulativePL")

trade_cols <- c('TradeDate','TradeID','Name','Strategy','MarketValue','SkewInto','ValueUSD','VolInto','DeltaPL','DeltaSwing','DeltaSkew','PnLOutof','TodayPL','Age','Long')
cuml_offside_data <- new_psns(all_cuml_offside_data[[1]])
cuml_onside_data <- new_psns(all_cuml_offside_data[[2]])
cuml_trades_in_offside_psns <- cuml_offside_data[!is.na(cuml_offside_data$TradeID)&cuml_offside_data$CumulativePL<0,]
cuml_trades_in_offside_psns <- unique(cuml_trades_in_offside_psns[trade_cols])
cuml_trades_in_onside_psns  <- cuml_onside_data[!is.na(cuml_onside_data$TradeID)&cuml_onside_data$CumulativePL>=0,]
cuml_trades_in_onside_psns  <- unique(cuml_trades_in_onside_psns[trade_cols])

occrt_c <- aggregate(cuml_offside_data['CumulativePL'],list(Date=cuml_offside_data$TradeDate),function(x)sum(unique(x)<0))
all <- aggregate(history_data['Instrument'],list(Date=history_data$TradeDate),function(x)length(unique(x)))
colnames(all) <- c('Date','Total')
colnames(occrt_c) <- c('Date','Total')

positions_data <- rbind(cbind(Name='All positions',all),cbind(Name='Offside',occrt_c))
positions_plot <- ggplot(data=positions_data, aes(x=Date, y=Total, group = Name, colour = Name)) +
  geom_line(size = 0.8) + 
  ylab("Number of positions") + xlab('Date') + ggtitle('Number of absolute offside positions') + scale_linetype_discrete(name = "") 

port <- portfolio_decomposition(history_data)
port[is.na(port)] <- 0
core <- port[port$Core==TRUE,]

return_age <- aggregate(port[c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(Age=port$PsnAge),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
return_mn <- aggregate(port[c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(Age=port$PsnAge),function(x)10000*mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
exposure <- aggregate(port[c('NTrades','Exposure','CoreExposure','TradedExposure','PassiveExposure','ActiveExposure','CorePassiveExp','CoreActiveExp','TradedPassiveExp','TradedActiveExp')],list(Age=port$PsnAge),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))

total_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TotalReturn+1))),Av.Rtn=return_mn$TotalReturn)
rtn_cum <- plot_ly(total_rtn, x = Age, y = Rtn, name="Total")

core_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$CoreReturn+1))),Av.Rtn=return_mn$CoreReturn)
rtn_cum <- add_trace(core_rtn, x = Age, y = Rtn, name="Core")

traded_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TradedReturn+1))),Av.Rtn=return_mn$TradedReturn)
rtn_cum <- add_trace(traded_rtn, x = Age, y = Rtn, name="Traded")

passive_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$PassiveReturn+1))),Av.Rtn=return_mn$PassiveReturn)
rtn_cum <- add_trace(passive_rtn, x = Age, y = Rtn, name="Passive")

active_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$ActiveReturn+1))),Av.Rtn=return_mn$ActiveReturn)
rtn_cum <- add_trace(active_rtn, x = Age, y = Rtn, name="Active")

corepassive_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$CorePassive+1))),Av.Rtn=return_mn$CorePassive)
rtn_cum <- add_trace(corepassive_rtn, x = Age, y = Rtn, name="CorePassive")

coreactive_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$CoreActive+1))),Av.Rtn=return_mn$CoreActive)
rtn_cum <- add_trace(coreactive_rtn, x = Age, y = Rtn, name="CoreActive")

tradedpassive_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TradedPassive+1))),Av.Rtn=return_mn$TradedPassive)
rtn_cum <- add_trace(tradedpassive_rtn, x = Age, y = Rtn, name="TradedPassive")

tradedactive_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TradedActive+1))),Av.Rtn=return_mn$TradedActive)
rtn_cum <- add_trace(tradedactive_rtn, x = Age, y = Rtn, name="TradedActive")

expr <- plot_ly(exposure,x=Age,y=Exposure,name="Total")
expr <- add_trace(exposure,x=Age,y=CoreExposure,name="Core")
expr <- add_trace(exposure,x=Age,y=TradedExposure,name="Traded")
expr <- add_trace(exposure,x=Age,y=PassiveExposure,name="Passive")
expr <- add_trace(exposure,x=Age,y=ActiveExposure,name="Active")
expr <- add_trace(exposure,x=Age,y=CorePassiveExp,name="CorePassive")
expr <- add_trace(exposure,x=Age,y=CoreActiveExp,name="CoreActive")
expr <- add_trace(exposure,x=Age,y=TradedPassiveExp,name="TradedPassive")
expr <- add_trace(exposure,x=Age,y=TradedActiveExp,name="TradedActive")

ntrades <- plot_ly(exposure,x=Age,y=NTrades,type="bar",name="NTrades")

all_cum <- subplot(rtn_cum,ntrades,expr,nrows=3)

rtn_tscale <- plot_ly(total_rtn, x = Age, y = Av.Rtn, name="Total")
rtn_tscale <- add_trace(core_rtn, x = Age, y = Av.Rtn, name="Core")
rtn_tscale <- add_trace(active_rtn, x = Age, y = Av.Rtn, name="Active")

#calendar return and PL
getSymbols("^SX5E")
index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])

calendar_rtn <- aggregate(port[c('NTrades','TodayPL','CorePL','TradedPL','PassivePL','ActivePL','CorePassivePL','CoreActivePL','TradedPassivePL','TradedActivePL','TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(Date=port$TradeDate),function(x)sum(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
ix <- index
colnames(ix) <- c('Date','Index')
calendar_rtn <- merge(calendar_rtn,ix,by='Date')
cdar_exposure <- aggregate(port[c('Exposure','CoreExposure','TradedExposure','PassiveExposure','ActiveExposure','CorePassiveExp','CoreActiveExp','TradedPassiveExp','TradedActiveExp')],list(Date=port$TradeDate),function(x)sum(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))

index_cdar_rtn <- data.frame(Date=calendar_rtn$Date,Rtn=exp(cumsum(log(calendar_rtn$Index+1))))
cdar_rtns <- plot_ly(index_cdar_rtn,x=Date,y=Rtn,name="SX5E")

total_cdar_rtn <- data.frame(Date=calendar_rtn$Date,Rtn=exp(cumsum(log(calendar_rtn$TotalReturn+1))))
cdar_rtns <- add_trace(total_cdar_rtn,x=Date,y=Rtn,name="Total")

core_cdar_rtn <- data.frame(Date=calendar_rtn$Date,Rtn=exp(cumsum(log(calendar_rtn$CoreReturn+1))))
cdar_rtns <- add_trace(core_cdar_rtn,x=Date,y=Rtn,name="Core")

traded_cdar_rtn <- data.frame(Date=calendar_rtn$Date,Rtn=exp(cumsum(log(calendar_rtn$TradedReturn+1))))
cdar_rtns <- add_trace(traded_cdar_rtn,x=Date,y=Rtn,name="Traded")

total_cdar_pl <- data.frame(Date=calendar_rtn$Date,PL=cumsum(calendar_rtn$TodayPL))
cdar_pl <- plot_ly(total_cdar_pl,x=Date,y=PL,name="Total")

calendar_rtn$CorePL[is.na(calendar_rtn$CorePL)] <- 0
core_cdar_pl <- data.frame(Date=calendar_rtn$Date,PL=cumsum(calendar_rtn$CorePL))
cdar_pl <- add_trace(core_cdar_pl,x=Date,y=PL,name="Core")

calendar_rtn$TradedPL[is.na(calendar_rtn$TradedPL)] <- 0
traded_cdar_pl <- data.frame(Date=calendar_rtn$Date,PL=cumsum(calendar_rtn$TradedPL))
cdar_pl <- add_trace(traded_cdar_pl,x=Date,y=PL,name="Traded")

cdar_ntrades <- plot_ly(calendar_rtn,x=Date,y=NTrades,type="bar")

cdar_expr <- plot_ly(cdar_exposure,x=Date,y=Exposure,name="Total")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=CoreExposure,name="Core")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=TradedExposure,name="Traded")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=PassiveExposure,name="Passive")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=ActiveExposure,name="Active")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=CorePassiveExp,name="CorePassive")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=CoreActiveExp,name="CoreActive")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=TradedPassiveExp,name="TradedPassive")
cdar_expr <- add_trace(cdar_exposure,x=Date,y=TradedActiveExp,name="TradedActive")

full_cdar <- subplot(cdar_rtns,cdar_ntrades,cdar_expr,nrows=3)





