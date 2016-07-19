setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
library(ggplot2)
library(plotly)
library(GGally)

traders   <- c(101,11,70)
dates <- c("2016-05-01")
#exclude <- c("DK_SHDG","DK_LHDG","BA_SHEDGE","BA_LHEDGE","JS_SHEDGE","JS_LHEDGE")

first <- TRUE
for(t in traders){
  kf <- function()dated_eighteen_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))  
  }
}

history_data <- history_data[!history_data$Strategy%in%exclude,]
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
scale_data <- c('ValueUSD','VolInto','VolOutof','SkewInto','SkewOutof','CompoundReturnInto','CompoundReturnOutof')
cols <- c('Trader','TradeDate','Strategy','Instrument',scale_data,'RSI14','TodayPL','Long','MarketValue','EarliestMarketValue','MarketRelPL','MinDate','PnLOutof')
history_data <- position_age_from_flats(history_data,cols)
#saveRDS(history_data,"C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/shiny_TE_demo/history_data.rds")
#history_data=readRDS("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/history_data.rds")
#PnlOutof is missing on some trades for some reason... replace it in this case with TodayPL
history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$PnLOutof <- history_data[!is.na(history_data$TradeID)&is.na(history_data$PnLOutof),]$TodayPL
history_data$TradeType <- 'NA'
history_data$TradeType[history_data$Long==1&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Increase'
history_data$TradeType[(history_data$Long==0)&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Decrease'
history_data$TradeType[history_data$Long==1&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Decrease'
history_data$TradeType[(history_data$Long==0)&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Increase'
psn_increased <- aggregate(history_data$TradeType,list(Strategy=history_data$Strategy,Visit=history_data$Visit,Instrument=history_data$Instrument),function(x)sum(x=='Increase',na.rm=TRUE)>0)
colnames(psn_increased) <- c('Strategy','Visit','Instrument','PsnIncreased')
history_data <- merge(history_data,psn_increased,by=c('Strategy','Visit','Instrument'),all.x=TRUE)

switch_direction <- function(history_data,column){
  history_data[column] <- -1^(1+history_data$PsnLong)*history_data[column]
  return(history_data)
}

history_data$PsnLong <- history_data$MarketValue>0
history_data <- switch_direction(history_data,'SkewInto')
history_data <- switch_direction(history_data,'SkewOutof')
history_data <- switch_direction(history_data,'CompoundReturnInto')
history_data <- switch_direction(history_data,'CompoundReturnOutof')

initial_trades <- unique(history_data[!is.na(history_data$TradeID)&history_data$PsnAge==0,c(cols,'PsnIncreased')])
increases <- unique(history_data[!is.na(history_data$TradeID)&history_data$TradeType=='Increase',c(cols,'PsnIncreased')])
scale_and_clip <- function(data,bound=5){
  na_idx <- is.na(data) 
  data[na_idx] <- mean(data,na.rm=TRUE)
  data <- as.numeric(scale(data))
  data[abs(data)>bound] <- NA
  data[na_idx] <- NA
  return(data)
}
for(scale_col in scale_data){
  initial_trades[[scale_col]] <- scale_and_clip(initial_trades[[scale_col]])
  increases[[scale_col]] <- scale_and_clip(increases[[scale_col]])
}
initial_plot_data <- rbind(cbind(Quantity='Skewness',Timeframe="Pre-trade",data.frame(Value=initial_trades$SkewInto),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Skewness',Timeframe="Post-trade",data.frame(Value=initial_trades$SkewOutof),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Pre-trade",data.frame(Value=initial_trades$VolInto),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Post-trade",data.frame(Value=initial_trades$VolOutof),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Pre-trade",data.frame(Value=initial_trades$CompoundReturnInto),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Post-trade",data.frame(Value=initial_trades$CompoundReturnOutof),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Pre-trade",data.frame(Value=scale_and_clip(initial_trades$CompoundReturnInto/initial_trades$VolInto)),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Post-trade",data.frame(Value=scale_and_clip(initial_trades$CompoundReturnOutof/initial_trades$VolOutof)),initial_trades[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]))
increases_plot_data<- rbind(cbind(Quantity='Skewness',Timeframe="Pre-trade",data.frame(Value=increases$SkewInto),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Skewness',Timeframe="Post-trade",data.frame(Value=increases$SkewOutof),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Pre-trade",data.frame(Value=increases$VolInto),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Volatility',Timeframe="Post-trade",data.frame(Value=increases$VolOutof),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Pre-trade",data.frame(Value=increases$CompoundReturnInto),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return',Timeframe="Post-trade",data.frame(Value=increases$CompoundReturnOutof),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Pre-trade",data.frame(Value=scale_and_clip(increases$CompoundReturnInto/increases$VolInto)),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]),
                           cbind(Quantity='Return/Volatility',Timeframe="Post-trade",data.frame(Value=scale_and_clip(increases$CompoundReturnOutof/increases$VolOutof)),increases[c('Trader','TradeDate','Strategy','Instrument','ValueUSD','RSI14')]))

scatter_plot_kernel <- function(data, mapping, method="lm", ...){
  p <- ggplot(data = data, mapping = mapping) + 
       #geom_point(mapping = aes_string(colour="PsnIncreased")) + 
       geom_point() + 
       geom_smooth(method=method,mapping = aes_string(colour="PsnIncreased"), ...)
  return(p)
}
density_plot_kernel <- function(data, mapping, ...){
  hitrate <- mean(data$PnLOutof>0,na.rm=TRUE)*100
  losers  <- mean(data[data$PnLOutof<0,]$PnLOutof,na.rm=TRUE)
  winners <- mean(data[data$PnLOutof>0,]$PnLOutof,na.rm=TRUE)
  p <- ggplot(data = data, mapping = mapping) + 
       geom_density(mapping = aes_string(colour="PsnIncreased"), fill=NA) +
       annotate("text",x=2.5,y=0.5,label=paste(round(hitrate),"%",sep="")) +
       annotate("text",x=2.5,y=0.4,label=paste("Win:",round(winners),sep="")) +
       annotate("text",x=2.5,y=0.3,label=paste("Loss:",round(losers),sep=""))
  return(p)
}

pcols <- c('VolInto','VolOutof','SkewInto','SkewOutof','CompoundReturnInto','CompoundReturnOutof','ValueUSD')
trader <- 11
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("JS opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("JS position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols, 
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="JS position increases")

trader <- 101
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("DK opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("DK position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="DK position increases")

trader <- 70
ggplot(initial_plot_data[initial_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("BA opening trades") +
  facet_grid(Quantity~Timeframe,scales="free_y")
ggplot(increases_plot_data[increases_plot_data$Trader==trader,], aes(x=ValueUSD, y=Value,colour=RSI14)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  ggtitle("BA position increases") +
  facet_grid(Quantity~Timeframe,scales="free_y")

ggpairs(initial_trades[initial_trades$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA opening trades")
ggpairs(increases[increases$Trader==trader,], lower=list(continuous=scatter_plot_kernel), columns=pcols,
        diag=list(continuous=density_plot_kernel), axisLabels="show", title="BA position increases")

#Compute hit rate and asymetry for:
#Initial trades where subsequently was increased
#Initial trades where subsequently was not increased
#Initial trades overall
#Trades that increased the position

#Presumably the question is whether the positions that
#were increased should have been larger in the first place
#(The ones that are not increased, presumably were loosers)
#And/or possibly that the position could have been increased more aggressively

#Condition regression on the first trades only in the positions 
#that were increased. And/or on whether the position was profitable, 
#post trade and or on a longer timescale.
#Plot displaying this on the diagonal of the regression plot?
