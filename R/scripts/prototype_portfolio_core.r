setwd("C:/Development/AllRaid/trunk/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)
library(quantmod)

trader   <- 11
dates <- c("2016-04-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
history_data <- history_data[history_data$TradeDate<'2016-01-01'&history_data$TradeDate>'2015-09-01',]

port <- portfolio_decomposition(history_data)
wport[is.na(port)] <- 0
core <- port[port$Core==TRUE,]
core_pl <- aggregate(core['TodayPL'],list(TradeDate=core$TradeDate),function(x)sum(x,na.rm=TRUE))
core_count <- aggregate(core['TodayPL'],list(TradeDate=core$TradeDate),length)
port_count <- aggregate(port['TodayPL'],list(TradeDate=port$TradeDate),length)
colnames(core_count) <- c('TradeDate','Count')
colnames(port_count) <- c('TradeDate','Count')

ccnt <- plot_ly(core_count,x=TradeDate,y=Count)
ccnt <- add_trace(port_count,x=TradeDate,y=Count)

#monthly core, total, active overlap, pfo rank return corr

month_year <- unique(format(port$TradeDate,"%Y-%m"))
first <- TRUE
for(my in month_year){
  rows <- port[format(port$TradeDate,"%Y-%m")==my,]
  if(first){
    olaps <- cbind(Date=as.Date(paste(my,"-01",sep="")),compute_overlaps(rows))
    rrank <- cbind(Date=as.Date(paste(my,"-01",sep="")),compute_return_rank(rows))
    first <- FALSE
  }
  else{
    olaps <- rbind(olaps,cbind(Date=as.Date(paste(my,"-01",sep="")),compute_overlaps(rows)))
    rrank <- rbind(rrank,cbind(Date=as.Date(paste(my,"-01",sep="")),compute_return_rank(rows)))
  }
}

ocnt <- plot_ly(olaps,x=Date,y=Total,name="Total")
ocnt <- add_trace(olaps,x=Date,y=Core,name="Core")
ocnt <- add_trace(olaps,x=Date,y=Active,name="Active")
ocnt <- add_trace(olaps,x=Date,y=ActiveCore,name="ActiveCore")

cor <- plot_ly(rrank,x=Date,y=Total,name="Total")
cor <- add_trace(rrank,x=Date,y=Core,name="Core")
cor <- add_trace(rrank,x=Date,y=Active,name="ActiveCore")

#core-active-total regression

getSymbols("^SX5E")
index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])
first <- TRUE
for(my in month_year){
  rows <- port[format(port$TradeDate,"%Y-%m")==my,]
  rows <- clean_rtn(rows,c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive'))
  rows <- aggregate(rows[c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(TradeDate=rows$TradeDate),function(x)sum(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
  ind  <- index[format(index$TradeDate,"%Y-%m")==my,]
  rows  <- merge(rows,ind,by='TradeDate')
  data_list <- list(rows[c('TradeDate','TotalReturn')],rows[c('TradeDate','CoreReturn')],rows[c('TradeDate','TradedReturn')],rows[c('TradeDate','PassiveReturn')],rows[c('TradeDate','ActiveReturn')],rows[c('TradeDate','CorePassive')],rows[c('TradeDate','CoreActive')],rows[c('TradeDate','TradedActive')],rows[c('TradeDate','TradedPassive')],rows[c('TradeDate','SX5E.Close')])
  names(data_list) <- c('Total','Core','Traded','Passive','Active','CorePassive','CoreActive','TradedPassive','TradedActive','SX5E')
  rgn <- pfo_daily_regression(data_list)
  if(first){
    rgn_data <- list(rgn)
    first <- FALSE
  }
  else{
    rgn_data[[(length(rgn_data)+1)]] <- rgn
  }
}
names(rgn_data) <- month_year

alpha_bps <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Core.x','Total.y']],rgn_data)),type="bar",name="Core")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Traded.x','Total.y']],rgn_data)),type="bar",name="Traded")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Passive.x','Total.y']],rgn_data)),type="bar",name="Passive")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Active.x','Total.y']],rgn_data)),type="bar",name="Active")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['CorePassive.x','Total.y']],rgn_data)),type="bar",name="CorePassive")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['CoreActive.x','Total.y']],rgn_data)),type="bar",name="CoreActive")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['TradedPassive.x','Total.y']],rgn_data)),type="bar",name="TradedPassive")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['TradedActive.x','Total.y']],rgn_data)),type="bar",name="TradedActive")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['SX5E.x','Total.y']],rgn_data)),type="bar",name="Index")
alpha_bps <- layout(yaxis=list(title="Alpha total vs. core/active/index"),xaxis=list(title=""))

beta <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Core.x','Total.y']],rgn_data)),type="bar",name="Core")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Traded.x','Total.y']],rgn_data)),type="bar",name="Traded")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Passive.x','Total.y']],rgn_data)),type="bar",name="Passive")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Active.x','Total.y']],rgn_data)),type="bar",name="Active")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['CorePassive.x','Total.y']],rgn_data)),type="bar",name="CorePassive")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['CoreActive.x','Total.y']],rgn_data)),type="bar",name="CoreActive")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['TradedPassive.x','Total.y']],rgn_data)),type="bar",name="TradedPassive")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['TradedActive.x','Total.y']],rgn_data)),type="bar",name="TradedActive")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['SX5E.x','Total.y']],rgn_data)),type="bar",name="Index")
beta <- layout(yaxis=list(title="Beta total vs. core/active/index"),xaxis=list(title=""))

crrl <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['Core.x','Total.y']],rgn_data)),type="bar",name="Core")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['Traded.x','Total.y']],rgn_data)),type="bar",name="Traded")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['Passive.x','Total.y']],rgn_data)),type="bar",name="Passive")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['Active.x','Total.y']],rgn_data)),type="bar",name="Active")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['CorePassive.x','Total.y']],rgn_data)),type="bar",name="CorePassive")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['CoreActive.x','Total.y']],rgn_data)),type="bar",name="CoreActive")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['TradedPassive.x','Total.y']],rgn_data)),type="bar",name="TradedPassive")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['TradedActive.x','Total.y']],rgn_data)),type="bar",name="TradedActive")
crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Correlation']][['SX5E.x','Total.y']],rgn_data)),type="bar",name="Index")
crrl <- layout(yaxis=list(title="Correlation total vs. core/active/index"),xaxis=list(title=""))

port_props <- subplot(alpha_bps,beta,crrl,nrows=3)

#timescale of active and core return

return_age <- aggregate(port[port$PsnAge<51,c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(Age=port[port$PsnAge<51,]$PsnAge),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
return_mn <- aggregate(port[port$PsnAge<51,c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')],list(Age=port[port$PsnAge<51,]$PsnAge),function(x)10000*mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
exposure <- aggregate(port[port$PsnAge<51,c('NTrades','Exposure','CoreExposure','TradedExposure','PassiveExposure','ActiveExposure','CorePassiveExp','CoreActiveExp','TradedPassiveExp','TradedActiveExp')],list(Age=port[port$PsnAge<51,]$PsnAge),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))

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

rtn_tscale <- plot_ly(total_rtn[total_rtn$Age<51,], x = Age, y = Av.Rtn, name="Total")
rtn_tscale <- add_trace(core_rtn[core_rtn$Age<51,], x = Age, y = Av.Rtn, name="Core")
rtn_tscale <- add_trace(active_rtn[active_rtn$Age<51,], x = Age, y = Av.Rtn, name="Active")

#calendar return and PL

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