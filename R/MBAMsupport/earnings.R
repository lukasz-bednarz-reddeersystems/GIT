setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)

# HOLDING A POSITION OVER FIGURES:
#   
# a)	What happens on the day if the stock was ABSOLUTELY OFF SIDE IN TO NUMBERS
# b)	What happens on the day if the stock was RELATIVELY OFF SIDE IN TO NUMBERS
# c)	A+b together.
# d)	What happens on the day if the stock was ABSOLUTELY ON SIDE IN TO NUMBERS
# e)	What happens on the day if the stock was RELATIVELY ON SIDE IN TO NUMBERS
# f)	D and e together

traders   <- c(101,11,70)
dates <- c("2016-05-01")

first <- TRUE
for(t in traders){
  kf <- function()dated_four_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_four_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_four_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
  }
}

history_data <- market_rel_pl(history_data,trade_rel=FALSE)
history_data <- market_day_age(history_data)
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]

#Determine the dates that reults occured and what type of trade occured on results date

visit <- 1
first <- TRUE
for(t in traders){
  instruments_held_on_earnings <- unique(history_data[history_data$Results&history_data$Trader==t,'Instrument'])
  instruments_held_on_earnings <- instruments_held_on_earnings[which(is.na(instruments_held_on_earnings)==FALSE)]
  for(e_ins in instruments_held_on_earnings){
    df <- unique(history_data[history_data$Trader==t&history_data$Instrument==e_ins&history_data$Results,c('Instrument','TradeDate','Age','TradeID','NewPosition','ClosePosition','MarketValue','Long','Results')])
    df$NewPosition[is.na(df$NewPosition)] <- 0
    df$ClosePosition[is.na(df$ClosePosition)] <- 0
    df$Visit <- cumsum(df$NewPosition)
    df <- df[df$Visit==visit,]
    if(nrow(df)>0){
      df$Results[is.na(df$Results)] <- FALSE
      df <- df[order(df$TradeDate),]
      df$ResultsCount <- cumsum(df$Results)
      df$Type <- NA
      df$Order <- NA
      df$Type[df$NewPosition==1] <- 'Open'
      df$Order[df$NewPosition==1] <- 1
      df$Type[df$ClosePosition==1] <- 'Close'
      df$Order[df$ClosePosition==1] <- 5
      df$Type[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue>0&df$Long] <- 'Increase'
      df$Order[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue>0&df$Long] <- 2
      df$Type[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue<0&!df$Long] <- 'Increase'
      df$Order[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue<0&!df$Long] <- 2
      df$Type[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue<0&df$Long] <- 'Decrease'
      df$Order[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue<0&df$Long] <- 4
      df$Type[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue>0&!df$Long] <- 'Decrease'
      df$Order[df$NewPosition==0&df$ClosePosition==0&!is.na(df$TradeID)&df$MarketValue>0&!df$Long] <- 4
      df$Type[df$NewPosition==0&df$ClosePosition==0&is.na(df$TradeID)] <- 'Held'
      df$Order[df$NewPosition==0&df$ClosePosition==0&is.na(df$TradeID)] <- 3
      df <- df[c('Instrument','TradeDate','ResultsCount','Type','Visit','Order')]
      if(first){
        position_results_index <- cbind(Trader=t,df)
        first <- FALSE
      }
      else{
        position_results_index <- rbind(position_results_index,cbind(Trader=t,df))
      } 
    }
  }
}
colnames(position_results_index) <- c('Trader','Instrument','ResultsDate','ResultsCount','Type','Visit','Order')

#Compute the total PL to the position after the results date 
results_data <- merge(history_data,position_results_index,by=c('Instrument','Trader'))
results_data <- unique(results_data[c('TradeID','Trader','Instrument','TradeDate','ResultsDate','ResultsCount','Type','TodayPL','CumulativePL','CumulativeMarketRelPL','MarketValue','Visit','Order')])
results_data$TradeID[results_data$Type=='Held'&results_data$TradeDate==results_data$ResultsDate] <- -1
results_visits <- unique(results_data$ResultsCount)

first <- TRUE
for(rv in results_visits){
  df <- results_data[results_data$ResultsCount==rv,]
  df <- df[df$TradeDate>=df$ResultsDate,]
  df$Side <- as.character(NA)
  df$Side[df$MarketValue>0] <- 'Long'
  df$Side[df$MarketValue<0] <- 'Short'
  df$AbsOffside <- NA
  df$RelOffside <- NA
  df$AbsOffside[df$CumulativePL<0] <- TRUE
  df$AbsOffside[df$CumulativePL>0] <- FALSE
  df$RelOffside[df$CumulativeMarketRelPL<0] <- TRUE
  df$RelOffside[df$CumulativeMarketRelPL>0] <- FALSE
  results_pl <- aggregate(df$TodayPL,list(Trader=df$Trader,Order=df$Order,Type=df$Type,Side=df$Side,RelOffside=df$RelOffside,AbsOffside=df$AbsOffside),function(x)sum(x,na.rm=TRUE))
  colnames(results_pl) <- c('Trader','Order','Type','Side','RelOffside','AbsOffside','TotalPL')
  df <- df[!is.na(df$TradeID),]
  results_hits <- aggregate(df$TodayPL,list(Trader=df$Trader,Order=df$Order,Type=df$Type,Side=df$Side,RelOffside=df$RelOffside,AbsOffside=df$AbsOffside),function(x)mean(x>0,na.rm=TRUE))
  colnames(results_hits) <- c('Trader','Order','Type','Side','RelOffside','AbsOffside','HitRate')
  results_wins <- aggregate(df$TodayPL,list(Trader=df$Trader,Order=df$Order,Type=df$Type,Side=df$Side,RelOffside=df$RelOffside,AbsOffside=df$AbsOffside,Win=df$TodayPL>0),function(x)mean(x,na.rm=TRUE))
  colnames(results_wins) <- c('Trader','Order','Type','Side','RelOffside','AbsOffside','Win','MeanPL')
  results_cnt <- aggregate(df$TradeID,list(Trader=df$Trader,Order=df$Order,Type=df$Type,Side=df$Side,RelOffside=df$RelOffside,AbsOffside=df$AbsOffside),function(x)sum(!is.na(x)))
  colnames(results_cnt) <- c('Trader','Order','Type','Side','RelOffside','AbsOffside','Count')
  results_pl <- merge(results_pl,results_hits,by=c('Trader','Order','Type','Side','RelOffside','AbsOffside'))
  results_pl <- merge(results_pl,results_cnt,by=c('Trader','Order','Type','Side','RelOffside','AbsOffside'))
  if(nrow(results_pl)>0){
    if(first){
      all_results_pl <- cbind(ResultsCount=rv,results_pl)
      all_results_wins<- cbind(ResultsCount=rv,results_wins)
      first <- FALSE
    }
    else{
      all_results_pl <- rbind(all_results_pl,cbind(ResultsCount=rv,results_pl))
      all_results_wins <- rbind(all_results_wins,cbind(ResultsCount=rv,results_wins))
    }  
  }
}
all_results_pl$Offside <- as.character(NA)
all_results_pl$Offside[all_results_pl$RelOffside&all_results_pl$AbsOffside] <- 'Offside'
all_results_pl$Offside[all_results_pl$RelOffside&!all_results_pl$AbsOffside] <- 'Rel.Offside'
all_results_pl$Offside[!all_results_pl$RelOffside&all_results_pl$AbsOffside] <- 'Abs.Offside'
all_results_pl$Offside[!all_results_pl$RelOffside&!all_results_pl$AbsOffside] <- 'Onside'
all_results_wins$Offside <- as.character(NA)
all_results_wins$Offside[all_results_wins$RelOffside&all_results_wins$AbsOffside] <- 'Offside'
all_results_wins$Offside[all_results_wins$RelOffside&!all_results_wins$AbsOffside] <- 'Rel.Offside'
all_results_wins$Offside[!all_results_wins$RelOffside&all_results_wins$AbsOffside] <- 'Abs.Offside'
all_results_wins$Offside[!all_results_wins$RelOffside&!all_results_wins$AbsOffside] <- 'Onside'

#Trade count
plot_results_cnt <- aggregate(all_results_pl['Count'],list(Trader=all_results_pl$Trader,Order=all_results_pl$Order,Type=all_results_pl$Type,Offside=all_results_pl$Offside,Side=all_results_pl$Side),function(x)sum(x))
js_rday_cnt_plot <- ggplot(data=plot_results_cnt[plot_results_cnt$Trader==11,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=Count)) + 
  labs(fill="Type of trade") + ylab("Number of trades") + xlab("What PM did on results day") +
  ggtitle("Number of results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

ba_rday_cnt_plot <- ggplot(data=plot_results_cnt[plot_results_cnt$Trader==70,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=Count)) + 
  labs(fill="Type of trade") + ylab("Number of trades") + xlab("What PM did on results day") +
  ggtitle("Number of results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

dk_rday_cnt_plot <- ggplot(data=plot_results_cnt[plot_results_cnt$Trader==101,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=Count)) + 
  labs(fill="Type of trade") + ylab("Number of trades") + xlab("What PM did on results day") +
  ggtitle("Number of results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

#TotalPL
plot_results_pl <- aggregate(all_results_pl['TotalPL'],list(Trader=all_results_pl$Trader,Order=all_results_pl$Order,Type=all_results_pl$Type,Offside=all_results_pl$Offside,Side=all_results_pl$Side),function(x)sum(x,na.rm=TRUE))
js_rday_pl_plot <- ggplot(data=plot_results_pl[plot_results_pl$Trader==11,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=TotalPL)) + 
  labs(fill="Type of trade") + ylab("Total $ PL to position after results day") + xlab("What PM did on results day") +
  ggtitle("Total PL to all positions after results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

ba_rday_pl_plot <- ggplot(data=plot_results_pl[plot_results_pl$Trader==70,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=TotalPL)) + 
  labs(fill="Type of trade") + ylab("Total $ PL to position after results day") + xlab("What PM did on results day") +
  ggtitle("Total PL to all positions after results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

dk_rday_pl_plot <- ggplot(data=plot_results_pl[plot_results_pl$Trader==101,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=TotalPL)) + 
  labs(fill="Type of trade") + ylab("Total $ PL to position after results day") + xlab("What PM did on results day") +
  ggtitle("Total PL to all positions after results day trades 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

#Hit rate
plot_results_hits <- aggregate(all_results_pl['HitRate'],list(Trader=all_results_pl$Trader,Order=all_results_pl$Order,Type=all_results_pl$Type,Offside=all_results_pl$Offside,Side=all_results_pl$Side),function(x)mean(x,na.rm=TRUE))
js_rday_hits_plot <- ggplot(data=plot_results_hits[plot_results_hits$Trader==11,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=HitRate)) + 
  labs(fill="Type of trade") + ylab("Hit Rate") + xlab("What PM did on results day") +
  ggtitle("Hit rate on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

ba_rday_hits_plot <- ggplot(data=plot_results_hits[plot_results_hits$Trader==70,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=HitRate)) + 
  labs(fill="Type of trade") + ylab("Hit Rate") + xlab("What PM did on results day") +
  ggtitle("Hit rate on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

dk_rday_hits_plot <- ggplot(data=plot_results_hits[plot_results_hits$Trader==101,],aes(x=reorder(Type,Order),fill=reorder(Type,Order))) +
  geom_bar(aes(weight=HitRate)) + 
  labs(fill="Type of trade") + ylab("Hit Rate") + xlab("What PM did on results day") +
  ggtitle("Hit rate on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

#Win loss
all_results_wins$WL <- as.character(NA)
all_results_wins$WL[all_results_wins$Win] <- 'Av.Win'
all_results_wins$WL[!all_results_wins$Win] <- 'Av.Loss'
plot_results_wins <- aggregate(all_results_wins['MeanPL'],list(Trader=all_results_wins$Trader,Order=all_results_wins$Order,Type=all_results_wins$Type,Offside=all_results_wins$Offside,Side=all_results_wins$Side,WL=all_results_wins$WL),function(x)mean(x,na.rm=TRUE))
js_rday_wins_plot <- ggplot(data=plot_results_wins[plot_results_wins$Trader==11,],aes(x=reorder(Type,Order),fill=WL)) +
  geom_bar(aes(weight=MeanPL),position="dodge") + 
  labs(fill="Type of trade") + ylab("Mean $ win/loss on day") + xlab("What PM did on results day") +
  ggtitle("Average $ win/loss on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

ba_rday_wins_plot <- ggplot(data=plot_results_wins[plot_results_wins$Trader==70,],aes(x=reorder(Type,Order),fill=WL)) +
  geom_bar(aes(weight=MeanPL),position="dodge") + 
  labs(fill="Type of trade") + ylab("Mean $ win/loss on day") + xlab("What PM did on results day") +
  ggtitle("Average $ win/loss on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

dk_rday_wins_plot <- ggplot(data=plot_results_wins[plot_results_wins$Trader==101,],aes(x=reorder(Type,Order),fill=WL)) +
  geom_bar(aes(weight=MeanPL),position="dodge") + 
  labs(fill="Type of trade") + ylab("Mean $ win/loss on day") + xlab("What PM did on results day") +
  ggtitle("Average $ win/loss on results days 2016") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Offside~Side,scales="free_y")

#summary plot
s_data <- plot_results_pl[plot_results_pl$Type!='Open',]
s_data$Initial <- as.character(NA)
s_data$Initial[s_data$Trader==11] <- 'JS'
s_data$Initial[s_data$Trader==70] <- 'BA'
s_data$Initial[s_data$Trader==101] <- 'DK'
plt_smmry_data <- aggregate(s_data['TotalPL'],list(Initial=s_data$Initial,Offside=s_data$Offside),sum)

plt_smmry_data$Offside[plt_smmry_data$Offside=='Offside'] <- 'Abs&Rel.Offside'
plt_smmry_data$Offside[plt_smmry_data$Offside=='Onside'] <- 'Abs&Rel.Onside'
plt_smmry <- ggplot(data=plt_smmry_data,aes(x=Initial,fill=Offside)) +
  geom_bar(aes(weight=TotalPL),position="dodge") +
  labs(fill="Position into results") + ylab("Total PL after results") + xlab("PM") +
  ggtitle("Total PL due to positions held over results days 2016") 
  

