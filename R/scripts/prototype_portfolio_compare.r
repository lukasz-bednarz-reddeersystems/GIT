setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)
library(quantmod)

traders   <- c(101,11,70)
dates <- c("2016-01-01")

first <- TRUE
for(t in traders){
  kf <- function()dated_twelve_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    all_port <- cbind(Trader=t,portfolio_decomposition(trader_data))
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
    all_port <- rbind(all_port,cbind(Trader=t,portfolio_decomposition(trader_data)))
  }
}
all_port <- rbind(all_port,cbind(Trader=0,portfolio_decomposition(history_data)))

pivot_traders <- function(pfo_data,traders=c(70,101,11,0)){
  first <- TRUE
  for(t in traders){
    trows <- pfo_data[pfo_data$Trader==t,]
    if(first){
      pivoted <- trows[setdiff(colnames(pfo_data),'Trader')]
      tcols <- (colnames(pivoted)!='TradeDate')&(colnames(pivoted)!='Instrument')
      colnames(pivoted)[tcols] <- paste(colnames(pivoted)[tcols],"_",as.character(t),sep="")
      first <- FALSE
    }
    else{
      fd <- trows[setdiff(colnames(pfo_data),'Trader')]
      tcols <- (colnames(fd)!='TradeDate')&(colnames(fd)!='Instrument')
      colnames(fd)[tcols] <- paste(colnames(fd)[tcols],"_",as.character(t),sep="")
      pivoted <- merge(pivoted,fd,by=c('TradeDate','Instrument'),all=TRUE)
      pivoted <- pivoted[!is.na(pivoted$TradeDate)&!is.na(pivoted$Instrument),]
    }
  }
  return(pivoted)
}

port <- pivot_traders(all_port)
port[is.na(port)] <- 0
getSymbols("^SX5E")
index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])
first <- TRUE
month_year <- unique(format(port$TradeDate,"%Y-%m"))
for(my in month_year){
  rows <- port[format(port$TradeDate,"%Y-%m")==my,]
  rows <- aggregate(rows[c('TotalReturn_0','CoreReturn_0','TradedReturn_0','TotalReturn_70','CoreReturn_70','TradedReturn_70','TotalReturn_11','CoreReturn_11','TradedReturn_11','TotalReturn_101','CoreReturn_101','TradedReturn_101')],list(TradeDate=rows$TradeDate),function(x)sum(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
  ind  <- index[format(index$TradeDate,"%Y-%m")==my,]
  rows  <- merge(rows,ind,by='TradeDate')
  data_list <- list(rows[c('TradeDate','TotalReturn_0')],rows[c('TradeDate','CoreReturn_0')],rows[c('TradeDate','TradedReturn_0')],rows[c('TradeDate','TotalReturn_70')],rows[c('TradeDate','CoreReturn_70')],rows[c('TradeDate','TradedReturn_70')],rows[c('TradeDate','SX5E.Close')],rows[c('TradeDate','TotalReturn_11')],rows[c('TradeDate','CoreReturn_11')],rows[c('TradeDate','TradedReturn_11')],rows[c('TradeDate','TotalReturn_101')],rows[c('TradeDate','CoreReturn_101')],rows[c('TradeDate','TradedReturn_101')])
  names(data_list) <- c('Total_MBAM','Core_MBAM','Traded_MBAM','Total_BA','Core_BA','Traded_BA','SX5E','Total_JS','Core_JS','Traded_JS','Total_DK','Core_DK','Traded_DK')
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
#rgn_data <- rgn_data[c('2015-09','2015-10','2015-11','2015-12')]

alpha_bps <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Total_JS.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Traded_JS.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS (Traded)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Core_JS.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS (Core)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Total_DK.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Traded_DK.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK (Traded)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Core_DK.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK (Core)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Total_BA.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Traded_BA.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA (Traded)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['Core_BA.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA (Core)")
alpha_bps <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)10000*x[['Alpha']][['SX5E.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-Index")
alpha_bps <- layout(yaxis=list(title="Alpha total vs. core/active/index"),xaxis=list(title=""))

beta <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Total_JS.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Traded_JS.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS (Traded)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Core_JS.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-JS (Core)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Total_DK.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Traded_DK.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK (Traded)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Core_DK.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-DK (Core)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Total_BA.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Traded_BA.x','Traded_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA (Traded)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['Core_BA.x','Core_MBAM.y']],rgn_data)),type="bar",name="MBAM-BA (Core)")
beta <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)x[['Beta']][['SX5E.x','Total_MBAM.y']],rgn_data)),type="bar",name="MBAM-Index") 
beta <- layout(yaxis=list(title="Beta total vs. core/active/index"),xaxis=list(title=""))

#crrl <- plot_ly(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)100*x[['Correlation']][['Core.x','Total.y']],rgn_data)),type="bar",name="Core")
#crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)100*x[['Correlation']][['Active.x','Total.y']],rgn_data)),type="bar",name="Active")
#crrl <- add_trace(x=as.Date(paste(names(rgn_data),"-01",sep="")),y=unlist(Map(function(x)100*x[['Correlation']][['SX5E.x','Total.y']],rgn_data)),type="bar",name="Index")
#crrl <- layout(yaxis=list(title="Correlation total vs. core/active/index"),xaxis=list(title=""))

#timescale of active and core return

return_age <- aggregate(port[c('TotalReturn_0','CoreReturn_0','TradedReturn_0')],list(Age=port$PsnAge_0),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
return_mn <- aggregate(port[c('TotalReturn_0','CoreReturn_0','TradedReturn_0')],list(Age=port$PsnAge_0),function(x)10000*mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))

total_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TotalReturn_0+1))),Av.Rtn=return_mn$TotalReturn_0)
rtn_cum <- plot_ly(total_rtn[total_rtn$Age<51,], x = Age, y = Rtn, name="Total")

core_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$CoreReturn_0+1))),Av.Rtn=return_mn$CoreReturn_0)
rtn_cum <- add_trace(core_rtn[total_rtn$Age<51,], x = Age, y = Rtn, name="Core")

active_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TradedReturn_0+1))),Av.Rtn=return_mn$TradedReturn_0)
rtn_cum <- add_trace(active_rtn[total_rtn$Age<51,], x = Age, y = Rtn, name="Active")

return_age <- aggregate(port[c('TotalReturn_11','CoreReturn_11','TradedReturn_11')],list(Age=port$PsnAge_11),function(x)mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))
return_mn <- aggregate(port[c('TotalReturn_11','CoreReturn_11','TradedReturn_11')],list(Age=port$PsnAge_11),function(x)10000*mean(x[!is.infinite(x)&!is.nan(x)],na.rm=TRUE))

js_total_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TotalReturn_11+1))),Av.Rtn=return_mn$TotalReturn_11)
js_rtn_cum <- plot_ly(js_total_rtn[js_total_rtn$Age<51,], x = Age, y = Rtn, name="Total")

js_core_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$CoreReturn_11+1))),Av.Rtn=return_mn$CoreReturn_11)
js_rtn_cum <- add_trace(js_core_rtn[js_total_rtn$Age<51,], x = Age, y = Rtn, name="Core")

js_active_rtn <- data.frame(Age=return_age$Age,Rtn=exp(cumsum(log(return_age$TradedReturn_11+1))),Av.Rtn=return_mn$TradedReturn_11)
js_rtn_cum <- add_trace(js_active_rtn[js_total_rtn$Age<51,], x = Age, y = Rtn, name="Active")

rtn_tscale <- plot_ly(total_rtn[total_rtn$Age<51,], x = Age, y = Av.Rtn, name="Total")
rtn_tscale <- add_trace(core_rtn[core_rtn$Age<51,], x = Age, y = Av.Rtn, name="Core")
rtn_tscale <- add_trace(active_rtn[active_rtn$Age<51,], x = Age, y = Av.Rtn, name="Active")

#calendar return

rtn_cols <- c('TradeDate','TotalReturn_0','CoreReturn_0','TradedReturn_0','TotalReturn_70','CoreReturn_70','TradedReturn_70','TotalReturn_101','CoreReturn_101','TradedReturn_101','TotalReturn_11','CoreReturn_11','TradedReturn_11')
rtn_data <- port[rtn_cols]
for(c in setdiff(rtn_cols,'TradeDate')){
  if(length(is.na(rtn_data[[c]]))>0){
    rtn_data[is.na(rtn_data[[c]]),c] <- 0  
  }
  if(length(is.nan(rtn_data[[c]]))>0){
    rtn_data[is.nan(rtn_data[[c]]),c] <- 0  
  }
  if(length(is.infinite(rtn_data[[c]]))>0){
    rtn_data[is.infinite(rtn_data[[c]]),c] <- 0  
  }
}
getSymbols("^SX5E")
rtn_data <- aggregate(rtn_data[setdiff(rtn_cols,'TradeDate')],list(Date=rtn_data$TradeDate),sum)
index <- data.frame(Date=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])
ix <- index
colnames(ix) <- c('Date','Index')
rtn_data <- merge(rtn_data,ix,by='Date')
calendar_rtn <- apply(rtn_data[setdiff(colnames(rtn_data),'Date')],2,function(x)exp(cumsum(log(x+1))))
calendar_rtn <- cbind(calendar_rtn,rtn_data['Date'])

cdar_rtn <- plot_ly(calendar_rtn, x = Date, y = TotalReturn_0, name="MBAM")

