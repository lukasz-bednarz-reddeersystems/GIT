sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

start <- as.Date('2014-11-02')
end   <- as.Date('2016-03-25')
s <- start
e <- start+1

first <- TRUE
for(d in 1:(end-start)){
  s <- as.Date(s %m+% days(1))
  if(wday(s) != 1 & wday(s) != 7){
    p <- rbind(get_trader_performance_simple(11,as.Date(s),as.Date(s),mbam=TRUE),
               get_trader_performance_simple(70,as.Date(s),as.Date(s),mbam=TRUE),
               get_trader_performance_simple(101,as.Date(s),as.Date(s),mbam=TRUE))
    p <- aggregate(p[c('PL','ReturnOnAllocated')],list(DateTime=p$DateTime),sum)
    if(first){
      all_perf <- p
      first <- FALSE
    }
    else{
      all_perf <- rbind(all_perf,p)
    }  
  }
}
factor_data <- data_request("risk_factor_returns",data.frame(dtDateTime=as.Date(unique(all_perf$DateTime))),c("sFactorName","dblChangePercent"))
factor_rtns <- factor_data@data
factor_rtns <- factor_rtns[factor_rtns$sFactorName=='rPriceMomentum12M',c('dtDateTime','dblChangePercent')]
factor_rtns$dblChangePercent <- factor_rtns$dblChangePercent/100
colnames(factor_rtns) <- c('DateTime','M12Return')
all_perf <- merge(all_perf,factor_rtns,by=c('DateTime'))

#Compute quarterly factor performance, beta and resulting attribution
qtrs <- list(list(as.Date('2015-01-01'),as.Date('2015-03-31')),
             list(as.Date('2015-04-01'),as.Date('2015-06-30')),
             list(as.Date('2015-07-01'),as.Date('2015-09-30')),
             list(as.Date('2015-10-01'),as.Date('2015-12-31')),
             list(as.Date('2016-01-01'),as.Date('2016-03-29')))

beta  <- list()
crrl  <- list()
first <- TRUE
cnt <- 1
for(d in qtrs){
  df <- all_perf[(all_perf$Date<=d[[2]])&(all_perf$Date>=d[[1]]),]
  df <- df[order(df$DateTime),]
  crrl[[cnt]] <- cor(df$ReturnOnAllocated,df$M12Return,use="na.or.complete")
  beta[[cnt]] <- coefficients(lm(ReturnOnAllocated~M12Return,df))[2]
  df$Q <- d[[1]]
  if(first){
    qtr_rtn <- aggregate(df[c('ReturnOnAllocated','M12Return')],list(Date=df$Q),function(x)prod(x+1,na.rm=TRUE))  
    first <- FALSE
  }
  else{
    qtr_rtn <- rbind(qtr_rtn,
                     aggregate(df[c('ReturnOnAllocated','M12Return')],list(Date=df$Q),function(x)prod(x+1,na.rm=TRUE)))  
  }
  cnt <- cnt + 1
}
all_qtr_rtn <- cbind(Beta=unlist(beta),qtr_rtn)
all_qtr_rtn <- cbind(Crrl=unlist(crrl),all_qtr_rtn)
all_qtr_rtn$M12Return <- 10000*(all_qtr_rtn$M12Return-1)
all_qtr_rtn$ReturnOnAllocated <- 10000*(all_qtr_rtn$ReturnOnAllocated-1)
all_qtr_rtn$Attrib <- all_qtr_rtn$Beta*all_qtr_rtn$M12Return

qtr_stats <- rbind(data.frame(Type="Regression Fit",Stat="Correlation",Date=all_qtr_rtn$Date,Value=all_qtr_rtn$Crrl),
                   data.frame(Type="Regression Fit",Stat="Beta",Date=all_qtr_rtn$Date,Value=all_qtr_rtn$Beta),
                   data.frame(Type="Return (bps)",Stat="MBAM Rtn.",Date=all_qtr_rtn$Date,Value=all_qtr_rtn$ReturnOnAllocated),
                   data.frame(Type="Return (bps)",Stat="Mom12 Rtn.",Date=all_qtr_rtn$Date,Value=all_qtr_rtn$M12Return),
                   data.frame(Type="Mom12 Attrib. (bps)",Stat="Mom12 Attrib.",Date=all_qtr_rtn$Date,Value=all_qtr_rtn$Attrib))
plt_qtr <- ggplot(data=qtr_stats, aes(x=Date, fill=Stat)) +
  geom_bar(aes(weight=Value),position="dodge") +
  facet_grid(Type~., scales="free_y") +
  ylab("") + xlab("Quarter (start month)") + ggtitle('Monthly correlation and attribution to 12M momentum') +
  theme(text = element_text(size=15)) 

cum_rtns <- data.frame(Date=all_perf$DateTime,MBAM=exp(cumsum(log(1+all_perf$ReturnOnAllocated))),Mom12=exp(cumsum(log(1+all_perf$M12Return))))

res_rtns <- rbind(data.frame(Return="MBAM",Date=cum_rtns$Date,Value=cum_rtns$MBAM),
                  data.frame(Return="Mom12M",Date=cum_rtns$Date,Value=cum_rtns$Mom12))
plt_rtns <- ggplot(data=res_rtns, aes(x=Date, y=Value, group = Return, colour = Return)) +
  geom_line(size = 1) + 
  ylab("Cumulative return") + xlab('Date') + ggtitle('Cumulative return of MBAM and 12M momentum') + scale_linetype_discrete(name = "") +
  theme(text = element_text(size=15)) 

grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 1)))
print(plt_rtns, vp = vplayout(1, 1))
print(plt_qtr, vp = vplayout(2:4, 1))


