setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)
library(quantmod)
library(lubridate)

trader   <- 70
dates <- c("2016-01-01")

kf <- function()dated_twelve_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
history_data <- history_data[history_data$TradeDate<'2016-01-01'&history_data$TradeDate>'2015-01-01',]

trade_data <- history_data[!is.na(history_data$TradeID),]
trade_key <- unique(trade_data[c('Instrument','TradeID','TradeDate','Strategy','PsnLong','Long','ValueUSD')])
value_on_day <- aggregate(((-1)^(1+trade_data$Long))*((-1)^(1+trade_data$PsnLong))*trade_data$ValueUSD,list(trade_data$TradeID,trade_data$TradeDate),function(x)sum(x,na.rm=TRUE))
colnames(value_on_day) <- c('TradeID','TradeDate','ValueUSD')

port <- portfolio_decomposition(history_data)
port[is.na(port)] <- 0

month_year <- unique(format(port$TradeDate,"%Y-%m"))
rtn_cols <- c('TotalReturn','CoreReturn','TradedReturn','PassiveReturn','ActiveReturn','CorePassive','CoreActive','TradedPassive','TradedActive')

first <- TRUE
for(r in 1:nrow(value_on_day)){
  vdate <- value_on_day[r,'TradeDate']
  vid <- value_on_day[r,'TradeID']
  vins<- trade_key[trade_key$TradeID==vid,'Instrument'][1]
  plong <-trade_key[trade_key$TradeID==vid,'PsnLong'][1]
  removed_hist <- history_data 
  removed_hist$MarketValue[removed_hist$Instrument==vins&removed_hist$TradeDate>=vdate&abs(removed_hist$MarketValue)>abs(value_on_day[r,'ValueUSD'])] <- removed_hist$MarketValue[removed_hist$Instrument==vins&removed_hist$TradeDate>=vdate&abs(removed_hist$MarketValue)>abs(value_on_day[r,'ValueUSD'])] - value_on_day[r,'ValueUSD']
  if(nrow(removed_hist[plong&removed_hist$Instrument==vins&removed_hist$MarketValue<0,])>0){
    removed_hist[plong&removed_hist$Instrument==vins&removed_hist$MarketValue<0,]$MarketValue <- 0  
  }
  if(nrow(removed_hist[!plong&removed_hist$Instrument==vins&removed_hist$MarketValue>0,])>0){
    removed_hist[!plong&removed_hist$Instrument==vins&removed_hist$MarketValue>0,]$MarketValue <- 0  
  }
  port_removed <- portfolio_decomposition(removed_hist)
  port_removed[is.na(port_removed)] <- 0
 
  for(my in month_year){
    rows <- port[format(port$TradeDate,"%Y-%m")==my,]
    rtn <- apply(rows[rtn_cols],2,function(x)prod(x[!is.infinite(x)]+1,na.rm=TRUE))
    rm_rows <- port_removed[format(port_removed$TradeDate,"%Y-%m")==my,]
    rm_rtn <- apply(rm_rows[rtn_cols],2,function(x)prod(x[!is.infinite(x)]+1,na.rm=TRUE))
    df <- rm_rtn[rtn_cols]/rtn[rtn_cols]
    if(first){
      trade_diff <- cbind(Month=as.Date(paste(my,"-01",sep="")),TradeDate=value_on_day[r,'TradeDate'],TradeID=value_on_day[r,'TradeID'],as.data.frame(t(df)))
      first <- FALSE
    }
    else{
      trade_diff <- rbind(trade_diff,cbind(Month=as.Date(paste(my,"-01",sep="")),TradeDate=value_on_day[r,'TradeDate'],TradeID=value_on_day[r,'TradeID'],as.data.frame(t(df))))
    }
  }
  message(r)
}

trade_data <- merge(trade_data,trade_diff,by=c('TradeID','TradeDate'))
date_cuttoffs <- as.Date(ymd(trade_data$Month) %m+% months(1))
trade_data <- trade_data[trade_data$TradeDate <= date_cuttoffs,]

buckets <- aggregate(trade_data[rtn_cols],list(Name=trade_data$Name,Month=trade_data$Month),function(x)mean(x-1,na.rm=TRUE))
bk <- buckets[buckets$Month=='2015-10-01',]
plot_ly(x=bk$Name,y=bk$TotalReturn*10000,type="bar",name="Total")
add_trace(x=bk$Name,y=bk$CoreReturn*10000,type="bar",name="Core")
add_trace(x=bk$Name,y=bk$TradedReturn*10000,type="bar",name="Traded")

plot_ly(x=bk$Name,y=bk$TotalReturn*10000,type="bar",name="Total")
add_trace(x=bk$Name,y=bk$CorePassive*10000,type="bar",name="CorePassive")
add_trace(x=bk$Name,y=bk$CoreActive*10000,type="bar",name="CoreActive")
add_trace(x=bk$Name,y=bk$TradedPassive*10000,type="bar",name="TradedPassive")
add_trace(x=bk$Name,y=bk$TradedActive*10000,type="bar",name="TradedActive")

psn_mngmt_names <- list(Month=trade_data$Month,
                        Adding.Long=trade_data$Adding.Long,
                        Cut.Loss.Short=trade_data$Cut.Loss.Short,
                        Take.Profit.Long=trade_data$Take.Profit.Long,
                        Adding.Short=trade_data$Adding.Short,
                        Take.Profit.Short=trade_data$Take.Profit.Short,
                        Cut.Loss.Long=trade_data$Cut.Loss.Long,
                        New.Long=trade_data$New.Long,
                        New.Short=trade_data$New.Short)
buckets <- aggregate(trade_data[rtn_cols],psn_mngmt_names,function(x)mean(x,na.rm=TRUE))

bk <- aggregate(buckets[rtn_cols],list(Adding.Long=buckets$Adding.Long,Month=buckets$Month),function(x)mean(x-1,na.rm=TRUE))
bk <- bk[bk$Month=='2015-10-01',]
plot_ly(x=as.character(bk$Adding.Long),y=bk$TotalReturn*10000,type="bar",name="Adding long Total")
add_trace(x=as.character(bk$Adding.Long),y=bk$CoreReturn*10000,type="bar",name="Core")
add_trace(x=as.character(bk$Adding.Long),y=bk$TradedReturn*10000,type="bar",name="Traded")
add_trace(x=as.character(bk$Adding.Long),y=bk$CorePassive*10000,type="bar",name="CorePassive")
add_trace(x=as.character(bk$Adding.Long),y=bk$CoreActive*10000,type="bar",name="CoreActive")
add_trace(x=as.character(bk$Adding.Long),y=bk$TradedPassive*10000,type="bar",name="TradedPassive")
add_trace(x=as.character(bk$Adding.Long),y=bk$TradedActive*10000,type="bar",name="TradedActive")

bk <- aggregate(buckets[rtn_cols],list(New.Long=buckets$New.Long,Month=buckets$Month),function(x)mean(x-1,na.rm=TRUE))
bk <- bk[bk$Month=='2015-09-01',]
plot_ly(x=as.character(bk$New.Long),y=bk$TotalReturn*10000,type="bar",name="New long Total")
add_trace(x=as.character(bk$New.Long),y=bk$CoreReturn*10000,type="bar",name="Core")
add_trace(x=as.character(bk$New.Long),y=bk$TradedReturn*10000,type="bar",name="Traded")

td <- trade_data[trade_data$Age>0,]
td$Type <- NA
td$Type[td$PsnLong&td$Long] <- 'increase long'
td$Type[td$PsnLong&!td$Long] <- 'decrease long'
td$Type[!td$PsnLong&!td$Long] <- 'increase short'
td$Type[!td$PsnLong&td$Long] <- 'decrease short'
buckets <- aggregate(td[rtn_cols],list(Offside=td$IntegratedPL<0,Month=td$Month,Type=td$Type),function(x)prod(x,na.rm=TRUE))
buckets[rtn_cols] <- buckets[rtn_cols] - 1

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='increase long',]
oct_offside <- plot_ly(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Oct. + lng)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Oct. + lng)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Oct. + lng)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='decrease long',]
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Oct. - lng)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Oct. - lng)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Oct. - lng)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='increase short',]
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Oct. + sht)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Oct. + sht)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Oct. + sht)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='decrease short',]
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Oct. - sht)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Oct. - sht)")
oct_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Oct. - sht)")
oct_offside <- layout(xaxis=list(title=""))

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='increase long',]
dec_offside <- plot_ly(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Dec. + lng)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Dec. + lng)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Dec. + lng)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='decrease long',]
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Dec. - lng)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Dec. - lng)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Dec. - lng)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='increase short',]
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Dec. + sht)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Dec. + sht)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Dec. + sht)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='decrease short',]
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Dec. - sht)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Dec. - sht)")
dec_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Dec. - sht)")
dec_offside <- layout(xaxis=list(title=""))

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='increase long',]
nov_offside <- plot_ly(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Nov. + lng)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Nov. + lng)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Nov. + lng)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='decrease long',]
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Nov. - lng)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Nov. - lng)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Nov. - lng)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='increase short',]
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Nov. + sht)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Nov. + sht)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Nov. + sht)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='decrease short',]
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TotalReturn,type="bar",name="Ttl (Nov. - sht)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=CoreReturn,type="bar",name="Cre (Nov. - sht)")
nov_offside <- add_trace(bk,x=c('Onside','Offside'),y=TradedReturn,type="bar",name="Trd (Nov. - sht)")
nov_offside <- layout(xaxis=list(title=""))

buckets <- aggregate(trade_data[rtn_cols],list(Results=trade_data$Results,Month=trade_data$Month),function(x)mean(x-1,na.rm=TRUE))
bk <- buckets[buckets$Month=='2015-09-01',]
plot_ly(x=bk$Name,y=bk$TotalReturn*10000,type="bar",name="Total")
add_trace(x=bk$Name,y=bk$CoreReturn*10000,type="bar",name="Core")
add_trace(x=bk$Name,y=bk$TradedReturn*10000,type="bar",name="Traded")

buckets <- aggregate(trade_data[trade_data$Results,rtn_cols],list(Strategy=trade_data[trade_data$Results,]$Strategy,Month=trade_data[trade_data$Results,]$Month),function(x)mean(x-1,na.rm=TRUE))
bk <- buckets[buckets$Month=='2015-09-01',]
plot_ly(x=bk$Strategy,y=bk$TotalReturn*10000,type="bar",name="Total")
add_trace(x=bk$Strategy,y=bk$CoreReturn*10000,type="bar",name="Core")
add_trace(x=bk$Strategy,y=bk$TradedReturn*10000,type="bar",name="Traded")
add_trace(x=bk$Strategy,y=bk$CorePassive*10000,type="bar",name="CorePassive")
add_trace(x=bk$Strategy,y=bk$CoreActive*10000,type="bar",name="CoreActive")

rgr <- trade_data[trade_data$Month=='2015-09-01'&trade_data$Age<100,]
buckets <- aggregate(rgr[rtn_cols],list(Age=rgr$Age),function(x)mean(x-1,na.rm=TRUE))
plot_ly(x=buckets$Age,y=buckets$CoreReturn,mode="markers")

rgr <- trade_data[trade_data$Month=='2015-09-01',]
plot_ly(x=rgr$RSI14,y=rgr$CoreReturn,mode="markers")

td <- trade_data[trade_data$Rights.Issue|trade_data$Special.Dividend|trade_data$Rights.Issue|trade_data$Stock.Split|trade_data$Spin.Off,]
td$Type <- NA
td$Type[td$PsnLong&td$Long] <- 'increase long'
td$Type[td$PsnLong&!td$Long] <- 'decrease long'
td$Type[!td$PsnLong&!td$Long] <- 'increase short'
td$Type[!td$PsnLong&td$Long] <- 'decrease short'
buckets <- aggregate(td[rtn_cols],list(Offside=td$IntegratedPL<0,Month=td$Month,Type=td$Type),function(x)prod(x,na.rm=TRUE))
buckets[rtn_cols] <- buckets[rtn_cols] - 1

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='increase long',]
oct_special <- plot_ly(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Oct. + lng)")
oct_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Oct. + lng)")
oct_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Oct. + lng)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='decrease long',]
oct_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Oct. - lng)")
oct_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Oct. - lng)")
oct_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Oct. - lng)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='increase short',]
oct_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Oct. + sht)")
oct_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Oct. + sht)")
oct_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Oct. + sht)")

bk <- buckets[buckets$Month=='2015-10-01'&buckets$Type=='decrease short',]
oct_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Oct. - sht)")
oct_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Oct. - sht)")
oct_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Oct. - sht)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='increase long',]
nov_special <- plot_ly(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Nov. + lng)")
nov_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Nov. + lng)")
nov_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Nov. + lng)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='decrease long',]
nov_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Nov. - lng)")
nov_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Nov. - lng)")
nov_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Nov. - lng)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='increase short',]
nov_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Nov. + sht)")
nov_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Nov. + sht)")
nov_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Nov. + sht)")

bk <- buckets[buckets$Month=='2015-11-01'&buckets$Type=='decrease short',]
nov_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Nov. - sht)")
nov_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Nov. - sht)")
nov_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Nov. - sht)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='increase long',]
dec_special <- plot_ly(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Dec. + lng)")
dec_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Dec. + lng)")
dec_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Dec. + lng)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='decrease long',]
dec_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Dec. - lng)")
dec_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Dec. - lng)")
dec_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Dec. - lng)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='increase short',]
dec_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Dec. + sht)")
dec_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Dec. + sht)")
dec_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Dec. + sht)")

bk <- buckets[buckets$Month=='2015-12-01'&buckets$Type=='decrease short',]
dec_special <- add_trace(bk,x=c('Special'),y=TotalReturn,type="bar",name="Ttl (Dec. - sht)")
dec_special <- add_trace(bk,x=c('Special'),y=CoreReturn,type="bar",name="Cre (Dec. - sht)")
dec_special <- add_trace(bk,x=c('Special'),y=TradedReturn,type="bar",name="Trd (Dec. - sht)")


