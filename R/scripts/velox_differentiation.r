setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(quantmod)
library(lubridate)
library(scales)

get_dow <- function(x,dw) {
  date <- ymd(x)
  first <- floor_date(date,"month")
  dow <- sapply(seq(0,6),function(x) wday(first+days(x)))
  first_dow <- first + days(which(dow==dw)-1)
  return(first_dow)
}

get_many_dow <- function(y,dw) {
  hlp <- function(x)get_dow(x,dw)
  tmp <- lapply(y, hlp)
  tmp <- lapply(tmp, as.character)
  return(unlist(tmp))
}

get_positions <- function(data){
  #TODO: Ideally include untraded positions here
  pcols<- c('Instrument','Name','TradeDate','Strategy','Av.Age','Av.PL','Av.MarketValue','Av.Quantity','Av.PsnReturn','PsnRtnVol','Gm.PsnReturn','PsnLong','Age')
  psns <- unique(data[!is.na(data$TradeID),pcols])
  return(psns)
}

enrich_psns_from_ihistory <- function(psns,enrich_with){
  keys <- unique(psns[c('TradeDate','Instrument')])
  colnames(keys) <- c('DateTime','Instrument')
  data <- data_request("instrument_history",keys,c("DataElement","Value"))
  data <- data@data
  enrich_data <- data[Reduce(function(x,y)x&y,Map(function(x)data$DataElement==x,enrich_with)),]
  psns <- merge(psns,enrich_data,by=c('TradeDate','Instrument'))
  return(psns)
}

mkt_cap_on_date <- function(stocks,date){
  key <- data.frame(Instrument=stocks,DateTime=as.Date(date))
  data<- data_request("instrument_history",key,c("DataElement","Value"))
  data <- data@data
  data$Value <- as.numeric(data$Value)
  os <- data[data$DataElement=='lOutstandingShares',c('Instrument','DateTime','Value')]
  colnames(os) <- c('Instrument','DateTime','OutstandingShares')
  pc <- data[data$DataElement=='dblClosePrice',c('Instrument','DateTime','Value')]
  colnames(pc) <- c('Instrument','DateTime','ClosePrice')
  mktcap <- merge(os,pc,by=c('Instrument','DateTime'))
  mktcap$MarketCap <- mktcap$OutstandingShares*mktcap$ClosePrice
  return(mktcap)
}

dates <- c("2016-01-01")
trader <- 11
kf <- function()dated_twelve_monthly_lookback(trader,dates[1])
js_history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
js_psns <- get_positions(js_history_data)
trader <- 70
kf <- function()dated_twelve_monthly_lookback(trader,dates[1])
ba_history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
ba_psns <- get_positions(ba_history_data)
trader <- 101
kf <- function()dated_twelve_monthly_lookback(trader,dates[1])
dk_history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
dk_psns <- get_positions(dk_history_data)
all_history <- rbind.fill(js_history_data,ba_history_data,dk_history_data)
all_psns <- rbind(cbind(Trader=11,js_psns),cbind(Trader=70,ba_psns),cbind(Trader=101,dk_psns))

# MARKET CAP
target_dates <- c("2015-12-01","2015-11-01","2015-10-01","2015-09-01","2015-08-01","2015-07-01","2015-06-01","2015-05-01","2015-04-01","2015-03-01","2015-02-01","2015-01-01")
mktcap_dates <- get_many_dow(target_dates,3)
first <- TRUE
for(md in mktcap_dates){
  instruments <- unique(all_psns[all_psns$TradeDate<md&all_psns$TradeDate>(as.Date(md)-30),]$Instrument)
  md <- mkt_cap_on_date(instruments,md)
  if(first){
    mdata <- md
    first <- FALSE
    
  }
  else{
    mdata <- rbind(mdata,md)
  }
}
first <- TRUE
for(md in mktcap_dates){
  for(t in c(11,70,101)){
    trader_ins <- unique(all_psns[all_psns$TradeDate<md&all_psns$TradeDate>(as.Date(md)-30)&all_psns$Trader==t,c('Trader','Instrument','Strategy')])
    trader_mcap <-merge(mdata[mdata$DateTime==md,],trader_ins,by='Instrument')
    if(first){
      mcap_data <- trader_mcap
      first <- FALSE
    }
    else{
      mcap_data <- rbind(mcap_data,trader_mcap)
    }
  }
}

initial_trader <- function(data){
  data$TraderI[data$Trader==11] <- "JS"
  data$TraderI[data$Trader==70] <- "BA"
  data$TraderI[data$Trader==101] <- "DK"
  return(data)
}

mean_mcap <- aggregate(mcap_data$MarketCap,list(Trader=mcap_data$Trader),function(x)median(x,na.rm=TRUE))
mean_mcap <- initial_trader(mean_mcap)
plt_mcap <- ggplot(data=mean_mcap, aes(x=reorder(TraderI,x), fill=TraderI)) +
  geom_bar(aes(weight=x)) +
  ylab("Median market cap ($)") + xlab("Trader") + ggtitle('Median position market cap 2015') +
  labs(fill="PM") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.y  = element_text(angle=45, vjust=1),text = element_text(size=15)) 

#HOLDING PERIOD
holding_category <- function(ages){
  rval <- ages
  rval[ages<=10] <- 1
  rval[ages>10&ages<=30] <- 2
  rval[ages>30] <- 3
  return(rval)
}
flats <- count_flats(all_history)
flats <- flats[[1]]
colnames(flats) <- c('Instrument','TradeDate')
psn_flat <- merge(all_history,flats,by=c('Instrument','TradeDate'))
psn_flat <- unique(psn_flat[c('Instrument','TradeDate','Strategy','Age')])
psn_flat$Trader <- substr(psn_flat$Strategy,1,2)
psn_flat$HoldingPeriod <- holding_category(psn_flat$Age)
age_mean <- aggregate(psn_flat$Age,list(Trader=psn_flat$Trader,TradeDate=psn_flat$TradeDate),function(x)mean(x,na.rm=TRUE))
age_2015 <- aggregate(psn_flat$Age,list(Trader=psn_flat$Trader),function(x)median(x,na.rm=TRUE))
category_count <- aggregate(psn_flat$Age,list(Trader=psn_flat$Trader,Age=psn_flat$HoldingPeriod),function(x)length(x))
category_count[category_count$Trader=="JS",'x'] <- category_count[category_count$Trader=="JS",'x']/sum(category_count[category_count$Trader=="JS",'x'],na.rm=TRUE)
category_count[category_count$Trader=="BA",'x'] <- category_count[category_count$Trader=="BA",'x']/sum(category_count[category_count$Trader=="BA",'x'],na.rm=TRUE)
category_count[category_count$Trader=="DK",'x'] <- category_count[category_count$Trader=="DK",'x']/sum(category_count[category_count$Trader=="DK",'x'],na.rm=TRUE)
category_count$Range <- NA
category_count$Range[category_count$Age==1] <- '< 10d'
category_count$Range[category_count$Age==2] <- '10d-30d'
category_count$Range[category_count$Age==3] <- '> 30d'
plt_htime <- ggplot(data=category_count, aes(x=reorder(Range,Age), fill=Trader)) +
  geom_bar(aes(weight=x),position="dodge") +
  ylab("Fraction of positions") + xlab("Trader") + ggtitle('Median postion age during 2015') +
  theme(text = element_text(size=15)) +
  labs(fill="PM") 

#SIGNAL BREAKDOWN
sig_hist <-all_history[!is.na(all_history$TradeID),]
sig_hist$Trader <- substr(sig_hist$Strategy,1,2)
sig_hist <- unique(sig_hist[c("Instrument","Trader","TradeID","TradeDate","Strategy","Long","ValueUSD","TodayPL","Special.Dividend","Results","Close.Period","Dividend","Trading.Statement","AGM","Director.Sell.Non.Core","Conference.Call","Road.Show","Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" , "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue", "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected","Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed","Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing", "New.Issue")])
acols <- c("Special.Dividend","Results","Close.Period","Dividend","Trading.Statement","AGM","Director.Sell.Non.Core","Conference.Call","Road.Show","Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" , "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue", "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected","Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed","Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing", "New.Issue")
first <-  TRUE
for(signal in acols){
  sh <- sig_hist[!is.na(sig_hist[[signal]]),]
  sh <- sh[sh[[signal]]==TRUE,]
  if(nrow(sh)>0){
    sig_to <- aggregate(sh[c('ValueUSD','TodayPL')],list(Trader=sh$Trader),function(x)sum(x,na.rm=TRUE))
    sig_to <- cbind(Signal=signal,sig_to)
    if(first){
      all_sig <- sig_to
      first <- FALSE
    }
    else{
      all_sig <- rbind(all_sig,sig_to)
    } 
  }
}
all_sig$Signal <- gsub("\\."," ",all_sig$Signal)
all_sig$ValueUSD[all_sig$Trader=='BA'] <- all_sig$ValueUSD[all_sig$Trader=='BA']/110000000
all_sig$ValueUSD[all_sig$Trader=='DK'] <- all_sig$ValueUSD[all_sig$Trader=='DK']/150000000
all_sig$ValueUSD[all_sig$Trader=='JS'] <- all_sig$ValueUSD[all_sig$Trader=='JS']/80000000
plt_sig_value <- ggplot(data=all_sig, aes(x=Signal, fill=Trader)) +
  geom_bar(aes(weight=ValueUSD),position="dodge") +
  ylab("Total $ traded/Allocation") + ggtitle('Events traded 2015') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=15)) 

#CORE NAMES
cre_plot <- function(all_port,trader_initials,plot=NULL){
  trader <- switch(trader_initials,
                   JS=11,
                   BA=70,
                   DK=101)
  core <- all_port[all_port$Core&all_port$Trader==trader,]
  trdd <- all_port[!all_port$Core&all_port$Trader==trader,]
  core_count <- aggregate(core['TodayPL'],list(TradeDate=core$TradeDate),length)
  port_count <- aggregate(trdd['TodayPL'],list(TradeDate=trdd$TradeDate),length)
  colnames(core_count) <- c('TradeDate','Count')
  colnames(port_count) <- c('TradeDate','Count')
  if(length(plot)==0){
    ccnt <- plot_ly(core_count,x=TradeDate,y=Count,name=paste(trader_initials,"Core"))
    ccnt <- add_trace(port_count,x=TradeDate,y=Count,name=paste(trader_initials,"Traded"))  
  }
  else{
    ccnt <- add_trace(core_count,x=TradeDate,y=Count,name=paste(trader_initials,"Core"))
    ccnt <- add_trace(port_count,x=TradeDate,y=Count,name=paste(trader_initials,"Traded"))
  }
  return(ccnt)
}
all_port <- portfolio_decomposition(js_history_data)
all_port$Trader <- 11
all_port <- rbind(all_port,cbind(Trader=70,portfolio_decomposition(ba_history_data)))
all_port <- rbind(all_port,cbind(Trader=101,portfolio_decomposition(dk_history_data)))

port_plt <- aggregate(all_port['Instrument'],list(Core=all_port$Core,Trader=all_port$Trader),function(x)length(x))
port_plt <- rbind(data.frame(Trader='BA',Activity=port_plt[port_plt$Trader==70&!port_plt$Core,]$Instrument/sum(port_plt[port_plt$Trader==70,]$Instrument)),
                  data.frame(Trader='DK',Activity=port_plt[port_plt$Trader==101&!port_plt$Core,]$Instrument/sum(port_plt[port_plt$Trader==101,]$Instrument)),
                  data.frame(Trader='JS',Activity=port_plt[port_plt$Trader==11&!port_plt$Core,]$Instrument/sum(port_plt[port_plt$Trader==11,]$Instrument)))
activity <-  ggplot(data=port_plt, aes(x=Trader, fill=Trader)) +
             geom_bar(aes(weight=Activity)) +
             ylab("Fraction positions") + xlab("PM") + ggtitle('Fraction active positions in portfolio during 2015') +
             theme(text = element_text(size=15))
             

compute_pm_overlaps <- function(pfo,first_trader,second_trader,type){
  instruments <- unique(pfo$Instrument)
  count <- 0
  for(ins in instruments){
    count <- count + length(intersect(pfo[pfo$Instrument==ins&pfo$Trader==first_trader&pfo[type],'Instrument'],pfo[pfo$Instrument==ins&pfo$Trader==second_trader&pfo[type],'Instrument']))
  }
  count <- count/length(unique(pfo$TradeDate))
  return(count)
}
compute_daily_pm_overlap <- function(pfo,type,trail_by=14){
  dates <- unique(pfo$TradeDate)
  dates <- sort(dates[trail_by:length(dates)])
  ba_js <- c()
  ba_dk <- c()
  dk_js <- c()
  for(d in dates){
    ba_js <- c(ba_js,compute_pm_overlaps(pfo[pfo$TradeDate>(d-trail_by)&pfo$TradeDate<=d,],70,11,type))
    ba_dk <- c(ba_dk,compute_pm_overlaps(pfo[pfo$TradeDate>(d-trail_by)&pfo$TradeDate<=d,],70,101,type))
    dk_js <- c(dk_js,compute_pm_overlaps(pfo[pfo$TradeDate>(d-trail_by)&pfo$TradeDate<=d,],101,11,type))
  }
  rl <- data.frame(Date=dates,BA_JS=ba_js,BA_DK=ba_dk,DK_JS=dk_js)
  return(rl)
}
all_port$Traded <- !all_port$Core
pm_overlap_core <- compute_daily_pm_overlap(all_port,'Core')
pm_overlap_traded <- compute_daily_pm_overlap(all_port,'Traded')

pm_ov <- plot_ly(pm_overlap_core,x=Date,y=BA_JS,name="BA-JS:Core")
pm_ov <- add_trace(pm_overlap_traded,x=Date,y=BA_JS,name="BA-JS:Traded")
pm_ov <- add_trace(pm_overlap_core,x=Date,y=BA_DK,name="BA-DK:Core")
pm_ov <- add_trace(pm_overlap_traded,x=Date,y=BA_DK,name="BA-DK:Traded")
pm_ov <- add_trace(pm_overlap_core,x=Date,y=DK_JS,name="DK-JS:Core")
pm_ov <- add_trace(pm_overlap_traded,x=Date,y=DK_JS,name="DK-JS:Traded")
pm_ov <- layout(title="Average stock overlap (14 day rolling)",xaxis=list(title=""),yaxis=list(title="Overlap"))

ap <- all_port[!is.infinite(all_port$ActiveWeight)&!is.na(all_port$ActiveWeight),]
topnames <- aggregate(ap[c('ActiveWeight','CoreWeight')],list(Trader=ap$Trader,Name=ap$Name,Date=strftime(ap$TradeDate,format="%Y-%m")),function(x)max(abs(x),na.rm=TRUE))
topnames$Date <- as.Date(paste(topnames$Date,'-01',sep=""))
first <- TRUE
for(t in c(70,11,101)){
  for(d in topnames$Date){
    if(first){
      tns <- topnames[topnames$Trader==t&topnames$Date==d,]
      all_tns <- rbind(cbind(Core=TRUE,tns[order(-tns$CoreWeight),][1,]),cbind(Core=FALSE,tns[order(-tns$ActiveWeight),][1,]))
      first <- FALSE
    }
    else{
      tns <- topnames[topnames$Trader==t&topnames$Date==d,]
      all_tns <- rbind(all_tns,rbind(cbind(Core=TRUE,tns[order(-tns$CoreWeight),][1,]),cbind(Core=FALSE,tns[order(-tns$ActiveWeight),][1,])))
    }
  }
}
all_tns <- unique(all_tns)
tn_plot <- plot_ly(all_tns[all_tns$Trader==11,], x = Date, y = ActiveWeight, text = all_tns[all_tns$Trader==11,'Name'], mode = "markers+text", textfont = list(textangle=90))

#TURNOVER
history_data <- all_history
history_data$Trader <- NA
history_data$Trader[grep('JS',history_data$Strategy)] <- 11
history_data$Trader[grep('BA',history_data$Strategy)] <- 70
history_data$Trader[grep('DK',history_data$Strategy)] <- 101
to <- rbind(data.frame(Bucket='JS_Long',Value=100*sum(history_data[history_data$Trader==11&history_data$Long&history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/80000000),
            data.frame(Bucket='DK_Long',Value=100*sum(history_data[history_data$Trader==101&history_data$Long&history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/150000000),
            data.frame(Bucket='BA_Long',Value=100*sum(history_data[history_data$Trader==70&history_data$Long&history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/110000000),
            data.frame(Bucket='JS_Short',Value=100*sum(history_data[history_data$Trader==11&!history_data$Long&!history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/80000000),
            data.frame(Bucket='DK_Short',Value=100*sum(history_data[history_data$Trader==101&!history_data$Long&!history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/150000000),
            data.frame(Bucket='BA_Short',Value=100*sum(history_data[history_data$Trader==70&!history_data$Long&!history_data$PsnLong,'ValueUSD'],na.rm=TRUE)/110000000))

plt_to <- ggplot(data=to, aes(x=Bucket, fill=substr(Bucket,1,2))) +
  geom_bar(aes(weight=Value)) +
  labs(fill="PM") +
  ylab("% of allocation") + ggtitle('Value traded 2015')  +
  theme(text = element_text(size=15))

grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plt_mcap, vp = vplayout(1, 1))
print(plt_htime, vp = vplayout(1, 2))
print(activity, vp = vplayout(2, 1))
print(plt_to, vp = vplayout(2, 2))

#COUNTRY/SECTOR
instruments <- unique(all_history$Instrument)
first <- TRUE
for(ins in instruments){
  country <- data_request("instrument_country",data.frame(lInstrumentID=ins),c("sCountryName"))
  sector <- data_request("instrument_sector",data.frame(InstrumentID=ins),c("Name"))
  if(first){
    country_sector <- data.frame(Instrument=ins,Country=cnt,Sector=sct)
    first <- FALSE
  }
  else{
    country_sector <- rbind(country_sector,data.frame(Instrument=ins,Country=cnt,Sector=sct))
  }
}
exposure_summary <- merge(unique(all_psns[c('Trader','Instrument','Name','Av.Age','Av.PL','Av.MarketValue','Av.Quantity','Av.PsnReturn','PsnRtnVol','Gm.PsnReturn')]),country_sector,by='Instrument')
country_exposure <- aggregate(exposure_summary['Av.MarketValue'],list(Instrument=exposure_summary$Instrument,Trader=exposure_summary$Trader,Country=exposure_summary$Country),function(x)mean(abs(x[!is.nan(x)]),na.rm=TRUE))
country_exposure <- aggregate(exposure_summary['Av.MarketValue'],list(Trader=exposure_summary$Trader,Country=exposure_summary$Country),function(x)sum(abs(x[!is.nan(x)]),na.rm=TRUE)/10)
country_exposure <- initial_trader(country_exposure)

sector_exposure <- aggregate(exposure_summary['Av.MarketValue'],list(Instrument=exposure_summary$Instrument,Trader=exposure_summary$Trader,Sector=exposure_summary$Sector),function(x)mean(abs(x[!is.nan(x)]),na.rm=TRUE))
sector_exposure <- aggregate(exposure_summary['Av.MarketValue'],list(Trader=exposure_summary$Trader,Sector=exposure_summary$Sector),function(x)sum(abs(x[!is.nan(x)]),na.rm=TRUE)/10)
sector_exposure <- initial_trader(sector_exposure)

cnt_sct_data <- rbind(data.frame(Trader=country_exposure$TraderI,Name=country_exposure$Country,Type='Country',Value=country_exposure$Av.MarketValue),
                      data.frame(Trader=sector_exposure$TraderI,Name=sector_exposure$Sector,Type='Sector',Value=sector_exposure$Av.MarketValue))
cnt_sct_data$Value[cnt_sct_data$Trader=='JS'] <- cnt_sct_data$Value[cnt_sct_data$Trader=='JS']/80000000
cnt_sct_data$Value[cnt_sct_data$Trader=='BA'] <- cnt_sct_data$Value[cnt_sct_data$Trader=='BA']/110000000
cnt_sct_data$Value[cnt_sct_data$Trader=='DK'] <- cnt_sct_data$Value[cnt_sct_data$Trader=='DK']/150000000

plt_cnt <- ggplot(data=cnt_sct_data[cnt_sct_data$Type=='Country',], aes(x=Name, fill=Trader)) +
  geom_bar(aes(weight=Value),position="dodge") +
  labs(fill="PM") +
  ylab("Total $ traded/Allocation") + ggtitle('Value traded by country 2015') +
  facet_grid(Type~.,scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=15))
plt_sct <- ggplot(data=cnt_sct_data[cnt_sct_data$Type=='Sector',], aes(x=Name, fill=Trader)) +
  geom_bar(aes(weight=Value),position="dodge") +
  labs(fill="PM") +
  ylab("Total $ traded/Allocation") + ggtitle('Value traded by sector 2015') +
  facet_grid(Type~.,scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=15))

grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 1)))
print(plt_cnt, vp = vplayout(1, 1))
print(plt_sct, vp = vplayout(2, 1))

library(maps)
world <- map_data("world")
cf <- cnt_sct_data[cnt_sct_data$Type=='Country',]
colnames(cf) <- c('Trader','region','Type','Value')
cf$region <- as.character(cf$region)
cf$region[cf$region=='United Kingdom'] <- 'UK'
choro <- merge(world, cf, sort = FALSE, by = "region",all.x=TRUE)
chba <- choro[is.na(choro$Value),]
chba$Trader <- 'BA'
chjs <- choro[is.na(choro$Value),]
chjs$Trader <- 'JS'
chdk <- choro[is.na(choro$Value),]
chdk$Trader <- 'DK'
choro <- rbind(choro,chba,chjs,chdk)
choro <- choro[!is.na(choro$Trader),]
choro <- choro[order(choro$order), ]
choro$Trader <- as.character(choro$Trader)
to_map <- ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Value)) +
  coord_map("mercator") +
  scale_fill_distiller(palette = 'Spectral') +
  facet_grid(Trader~.)

#FACTOR EXPOSURES
instruments <- unique(all_history$Instrument)
first <- TRUE
for(ins in instruments){
  factor_exp <- data_request("risk_instrument_exposure",data.frame(lInstrumentID=ins,dtDateTime=all_history$TradeDate),c("sFactorName","dblZScore","dblValue"))
  if(first){
    factor_data <- factor_exp@data
    first <- FALSE
  }
  else{
    factor_data <- rbind(factor_data,factor_exp@data)
  }
}
colnames(factor_data) <- c('Instrument','TradeDate','FactorName','ZScore')
factor_summary <- merge(all_port,factor_data,by=c('Instrument','TradeDate'))
only_factors <- c('rValue','rStrength','rGrowth','rSize','rSectorTrendExtension','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')
factor_summary <- factor_summary[factor_summary$FactorName%in%only_factors,]
factor_summary$TotalExposure <- factor_summary$ZScore*factor_summary$Weight
factor_summary$TradedExposure <- factor_summary$ZScore*factor_summary$ActiveWeight
factor_summary$CoreExposure <- factor_summary$ZScore*factor_summary$CoreWeight
tilt_averages <- aggregate(factor_summary[c('ZScore','TotalExposure','TradedExposure','CoreExposure','Return')],list(Trader=factor_summary$Trader,Date=factor_summary$TradeDate,Factor=factor_summary$FactorName),function(x)mean(x,na.rm=TRUE))
tilt_totals <- aggregate(factor_summary[c('ZScore','TotalExposure','TradedExposure','CoreExposure','Return')],list(Trader=factor_summary$Trader,Date=factor_summary$TradeDate,Factor=factor_summary$FactorName),function(x)sum(x,na.rm=TRUE))

factor_tilt_plot <- function(tilt_averages,only_factors,ycol='TotalExposure'){
  first <- TRUE
  for(trader in c(11,70,101)){
    cnt <- 0
    for(factor in only_factors){
      fct <- tilt_averages[tilt_averages$Factor==factor&tilt_averages$Trader==trader,]
      fct <- fct[order(fct$Date),]
      if(first){
        fct_plt <- plot_ly(fct,x=Date,y=get(ycol),name=paste(unique(Factor),'_',unique(Trader)))
        first <- FALSE
      }
      else{
        fct_plt <- add_trace(fct,x=Date,y=get(ycol),name=paste(unique(Factor),'_',unique(Trader)))
      }
      cnt <- cnt+1
    }
  }
  fct_plt <- layout(yaxis=list(title="Exposure"))
  return(fct_plt)
}
fct_tilt <- factor_tilt_plot(tilt_averages,only_factors)

factor_tilt_compare <- function(tilt_averages,only_factors,traders,long,ycol='TotalExposure'){
  first <- TRUE
  cnt <- 0
  for(factor in only_factors){
    fct <- merge(tilt_averages[tilt_averages$Factor==factor&tilt_averages$Trader==traders[1]&tilt_averages$PsnLong==long,],tilt_averages[tilt_averages$Factor==factor&tilt_averages$Trader==traders[2]&tilt_averages$PsnLong==long,],by=c('Date'))
    fct <- fct[order(fct$Date),]
    if(first){
      fct_plt <- plot_ly(fct,x=get(paste(ycol,'.x',sep="")),y=get(paste(ycol,'.y',sep="")),name=unique(Factor.x),mode="markers")
      first <- FALSE
    }
    else{
      fct_plt <- add_trace(fct,x=get(paste(ycol,'.x',sep="")),y=get(paste(ycol,'.y',sep="")),name=unique(Factor.x),mode="markers")
    }
    cnt <- cnt+1
  }
  return(fct_plt)
}
ern_rtn <- plot_ly(tilt_averages[tilt_averages$Trader==101&tilt_averages$Factor=='rEarnings'&!is.na(tilt_averages$Return)&!is.infinite(tilt_averages$Return),],x=TotalExposure,y=Return,mode='markers',name="DK")
ern_rtn <- add_trace(tilt_averages[tilt_averages$Trader==11&tilt_averages$Factor=='rEarnings'&!is.na(tilt_averages$Return)&!is.infinite(tilt_averages$Return),],x=TotalExposure,y=Return,mode='markers',name="JS")
ern_rtn <- add_trace(tilt_averages[tilt_averages$Trader==70&tilt_averages$Factor=='rEarnings'&!is.na(tilt_averages$Return)&!is.infinite(tilt_averages$Return),],x=TotalExposure,y=Return,mode='markers',name="BA")
ern_rtn <- layout(title="Average Daily Portfolio Return vs Earnings Exposure")

trim <- function(x){
  return(unlist(Map(function(z)substr(z,2,nchar(z)),x)))
}

tilt_totals$Factor <- trim(tilt_totals$Factor)
tilt_totals$PM <- NA
tilt_totals[tilt_totals$Trader==11,]$PM <- 'JS'
tilt_totals[tilt_totals$Trader==70,]$PM <- 'BA'
tilt_totals[tilt_totals$Trader==101,]$PM <- 'DK'

plt_fct <- ggplot(data=tilt_totals,aes(x=Date,y=TotalExposure,group=PM,colour=PM)) +
  geom_line(size=1) +
  facet_grid(Factor~.) +
  ylab("Portfolio tilt") + ggtitle('Portfolio factor exposure') +
  theme(text = element_text(size=15))

#RETURNS
start <- as.Date('2014-11-01')
end   <- as.Date('2015-12-31')
s <- start
e <- start+1

first <- TRUE
for(d in 1:(end-start)){
  s <- as.Date(s %m+% days(1))
  if(wday(s) != 1 & wday(s) != 7){
    p <- rbind(get_trader_performance_simple(11,as.Date(s),as.Date(s)),
               get_trader_performance_simple(70,as.Date(s),as.Date(s)),
               get_trader_performance_simple(101,as.Date(s),as.Date(s)))
    t <- rbind(get_trader_performance_simple(11,as.Date(s),as.Date(s),mbam=TRUE),
               get_trader_performance_simple(70,as.Date(s),as.Date(s),mbam=TRUE),
               get_trader_performance_simple(101,as.Date(s),as.Date(s),mbam=TRUE))
    t <- cbind(TraderID=0,aggregate(t[c('ReturnOnAllocated')],list(DateTime=t$DateTime),sum))
    p <- rbind(p[c('TraderID','DateTime','ReturnOnAllocated')],t[c('TraderID','DateTime','ReturnOnAllocated')])
    if(first){
      raw_perf <- p
      first <- FALSE
    }
    else{
      raw_perf <- rbind(raw_perf,p)
    }  
  }
}
#Trailing 30 day vol
end  <-  max(raw_perf$DateTime)
start<-  as.Date('2015-01-01')
s <- start
first <- TRUE
for(d in 1:(end-start)){
  df <- raw_perf[raw_perf$DateTime<=(s-1+d)&raw_perf$DateTime>(s-31+d),]
  cdf<- raw_perf[raw_perf$DateTime<=(s-1+d),]
  if(nrow(df)>0){
    df <- df[order(df$DateTime),]
    vl <- cbind(Date=(s-1+d),aggregate(df[c('ReturnOnAllocated')],list(Trader=df$TraderID),function(x)sd(x,na.rm=TRUE)))
    vl <- merge(vl,aggregate(cdf[c('ReturnOnAllocated')],list(Trader=cdf$TraderID),function(x)sd(x,na.rm=TRUE)),by='Trader')
    sf <- merge(vl,aggregate(df[c('ReturnOnAllocated')],list(Trader=df$TraderID),function(x)prod(x+1)),by='Trader')
    sf$SharpeOnAllocated <- (sf$ReturnOnAllocated-1)/sf$ReturnOnAllocated.x
    vl <- merge(vl,sf[c('Trader','SharpeOnAllocated')],by='Trader')
    if(first){
      vol <- vl
      first <- FALSE
    }
    else{
      vol <- rbind(vol,vl)  
    }
  }
  s <- as.Date(s %m+% days(1)) 
}
colnames(vol) <- c('TraderID','DateTime','VolReturnOnInvested','CumVolReturnOnAllocated','SharpeOnAllocated')
all_perf <- merge(vol,raw_perf,by=c('DateTime','TraderID'))

all_perf$Trader <- NA
all_perf$Trader[all_perf$TraderID==0] <- 'Velox'
all_perf$Trader[all_perf$TraderID==11] <- 'JS'
all_perf$Trader[all_perf$TraderID==70] <- 'BA'
all_perf$Trader[all_perf$TraderID==101] <- 'DK'
first <- TRUE
for(t in unique(all_perf$Trader)){
  df <- all_perf[all_perf$Trader==t,] 
  df <- df[order(df$DateTime),]
  df$CumReturn <- exp(cumsum(log(1+df$ReturnOnAllocated)))
  df$CumVol <- df$CumVolReturnOnAllocated*sqrt(1:nrow(df)+61)
  df$CumSharpe <- (df$CumReturn-1)/df$CumVol
  if(first){
    cf <- df[c('Trader','DateTime','CumReturn','CumSharpe')]
    first <- FALSE
  }
  else{
    cf <- rbind(cf,df[c('Trader','DateTime','CumReturn','CumSharpe')])
  }
}
all_perf <- merge(all_perf,cf,by=c('Trader','DateTime'))

plt_data <- rbind(data.frame(Quantity='Cumulative return',Trader='BA',DateTime=all_perf[all_perf$Trader=='BA',]$DateTime,Value=all_perf[all_perf$Trader=='BA',]$CumReturn),
                  data.frame(Quantity='Cumulative Sharpe',Trader='BA',DateTime=all_perf[all_perf$Trader=='BA',]$DateTime,Value=all_perf[all_perf$Trader=='BA',]$CumSharpe),
                  data.frame(Quantity='Cumulative return',Trader='DK',DateTime=all_perf[all_perf$Trader=='DK',]$DateTime,Value=all_perf[all_perf$Trader=='DK',]$CumReturn),
                  data.frame(Quantity='Cumulative Sharpe',Trader='DK',DateTime=all_perf[all_perf$Trader=='DK',]$DateTime,Value=all_perf[all_perf$Trader=='DK',]$CumSharpe),
                  data.frame(Quantity='Cumulative return',Trader='JS',DateTime=all_perf[all_perf$Trader=='JS',]$DateTime,Value=all_perf[all_perf$Trader=='JS',]$CumReturn),
                  data.frame(Quantity='Cumulative Sharpe',Trader='JS',DateTime=all_perf[all_perf$Trader=='JS',]$DateTime,Value=all_perf[all_perf$Trader=='JS',]$CumSharpe),
                  data.frame(Quantity='Cumulative Sharpe',Trader='Velox',DateTime=all_perf[all_perf$Trader=='Velox',]$DateTime,Value=all_perf[all_perf$Trader=='Velox',]$CumSharpe),
                  data.frame(Quantity='Cumulative return',Trader='Velox',DateTime=all_perf[all_perf$Trader=='Velox',]$DateTime,Value=all_perf[all_perf$Trader=='Velox',]$CumReturn))

plt_rtn <- ggplot(data=plt_data,aes(x=DateTime,y=Value,group=Trader,colour=Trader)) +
           geom_line(size=1) +
           facet_grid(Quantity~.,scales="free_y") +
           ylab("Performance") + ggtitle('Cumulative return and Sharpe 2015') +
           theme(text = element_text(size=15))

#TRADING RANGE
trading_range_data <- unique(all_history[c('Instrument','TradeDate','ClosePrice','MarketValue','Strategy')])
#Better to import daily N and 50d mavg...
compute_pfo_range <- function(data,window=30){
  dates <- unique(data$TradeDate)
  ds <- as.numeric(max(dates) - min(dates))
  start <- min(dates) + window
  first <- TRUE
  for(d in window:ds){
    df <- data[data$TradeDate>(start-window+1)&data$TradeDate<(start+d),]
    N <- aggregate(df['ClosePrice'],list(Instrument=df$Instrument),function(x)sd(x,na.rm=TRUE))
    mavg <- aggregate(df['ClosePrice'],list(Instrument=df$Instrument),function(x)mean(x,na.rm=TRUE))
    range <- merge(N,mavg,by=c('Instrument'))
    range <- merge(range,df[df$TradeDate==(start+d-1),],by=c('Instrument'))
    range$Range <- (range$ClosePrice-range$ClosePrice.y)/range$ClosePrice.x
    range <- range[c('Range',intersect(colnames(data),colnames(range)))]
    if(first){
      rval <- range
      first<- FALSE
    }
    else{
      rval <- rbind(rval,range)
    }
  }
  return(rval)
}
js_pfo_range <- compute_pfo_range(trading_range_data[grep('JS',trading_range_data$Strategy),c('Instrument','TradeDate','ClosePrice','MarketValue')])
ba_pfo_range <- compute_pfo_range(trading_range_data[grep('BA',trading_range_data$Strategy),c('Instrument','TradeDate','ClosePrice','MarketValue')])
dk_pfo_range <- compute_pfo_range(trading_range_data[grep('DK',trading_range_data$Strategy),c('Instrument','TradeDate','ClosePrice','MarketValue')])

value_in_range <- function(range_data){
  range_data <- range_data[!is.na(range_data$MarketValue)&!is.na(range_data$Range),]
  range_data$Long <- range_data$MarketValue>0
  totals <- aggregate(range_data['MarketValue'],list(TradeDate=range_data$TradeDate,Long=range_data$Long),function(x)sum(abs(x),na.rm=TRUE))
  range_data$PriceLevel <- NA
  range_data$PriceLevel[(range_data$Range < -0.33)] <- 'Low'
  range_data$PriceLevel[(range_data$Range < 0.33)&(range_data$Range >= -0.33)] <- 'Mid'
  range_data$PriceLevel[(range_data$Range >= 0.33)] <- 'High'
  range_data <- merge(totals,aggregate(range_data['MarketValue'],list(TradeDate=range_data$TradeDate,Long=range_data$Long,PriceLevel=range_data$PriceLevel),function(x)sum(abs(x),na.rm=TRUE)),by=c('TradeDate','Long'))
  range_data$Weight <- range_data$MarketValue.y/range_data$MarketValue.x
  return(range_data[c('TradeDate','Long','PriceLevel','Weight')])  
}
js_pfo_range_weight <- value_in_range(js_pfo_range)
ba_pfo_range_weight <- value_in_range(ba_pfo_range)
dk_pfo_range_weight <- value_in_range(dk_pfo_range)
plt_range_data <- rbind(cbind(Trader='JS',js_pfo_range_weight),
                   cbind(Trader='BA',ba_pfo_range_weight),
                   cbind(Trader='DK',dk_pfo_range_weight))
plt_range_data$Side <- NA
plt_range_data$Side[plt_range_data$Long] <- 'Long'
plt_range_data$Side[!plt_range_data$Long] <- 'Short'
plt_range <- ggplot(data=plt_range_data,aes(x=TradeDate,y=Weight,group=PriceLevel,colour=PriceLevel)) +
  geom_line(size=1) +
  facet_grid(Trader~Side,scales="free_y") +
  ylab("Weight in trading range") + ggtitle('Portfolio price level') +
  labs(colour="Range") +
  theme(text = element_text(size=15))

