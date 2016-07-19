sourceTo("../analysis_modules/analysis_module_trade_entry_level.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

#key_func <- function(){three_monthly_lookback(11)}
trader <- 101
key_func <- function(){dated_three_monthly_lookback(trader,"2016-01-01")}
trade_level_analysis <- createAnalysisModule(entry_level_analysis_module_builder,key_func)
#debug(trade_level_analysis@ppmdl@post_comp@compute)
trade_level_analysis <- updateAnalysisModel(trade_level_analysis)
trade_level_analysis <- runAnalysisModule(trade_level_analysis)

data <- unique(trade_level_analysis@ppmdl@ppdata@data[c('TradeID','EntryType','Instrument','Total.PL','PsnLong','Long')])
t  <- aggregate(data$Total.PL,by=list(data$EntryType,data$PsnLong,data$Long),function(x)mean(x,na.rm=TRUE))
pd <- t[t$Group.2==TRUE&t$Group.3==TRUE,c('Group.1','x')]

barplot(pd$x,ylab="Avg. PL",xlab="Range",names.arg=as.character(pd$Group.1),main="Positions")

data <- unique(trade_level_analysis@ppmdl@ppdata@data[c('TradeID','EntryType','Instrument','PnLOutof','PsnLong','Long')])
t  <- aggregate(data$PnLOutof,by=list(data$EntryType,data$PsnLong,data$Long),function(x)mean(x,na.rm=TRUE))
pd <- t[t$Group.2==TRUE&t$Group.3==TRUE,c('Group.1','x')]

barplot(pd$x,ylab="Avg. PL",xlab="Range",names.arg=as.character(pd$Group.1),main="Trades")

t  <- aggregate(data$Long,by=list(data$EntryType,data$PsnLong,data$Long),sum)
pd <- t[t$Group.2==TRUE&t$Group.3==TRUE,c('Group.1','x')]

barplot(pd$x,ylab="N legs",xlab="Range",names.arg=as.character(pd$Group.1),main="Trades")
  