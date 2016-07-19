sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)

start <- as.Date('2014-11-02')
end   <- as.Date('2016-03-27')
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
added_rtns <- data.frame(DateTime=c(as.Date('2015-03-28'),as.Date('2015-03-29'),as.Date('2015-03-30')), ReturnOnInvested=c(0.0001,-0.0019,-0.0043))

getSymbols('DCOILWTICO',src='FRED') 
oil <- as.data.frame(DCOILWTICO)
oil$oReturn <- c(NA,diff(oil$DCOILWTICO)/oil$DCOILWTICO[1:(length(oil$DCOILWTICO)-1)])
oil$DateTime <- as.Date(rownames(oil))

getSymbols('^NMX1770')
miners <- as.data.frame(NMX1770)
miners$mReturn <- c(NA,diff(miners$NMX1770.Close)/miners$NMX1770.Close[1:(length(miners$NMX1770.Close)-1)])
miners$DateTime <- as.Date(rownames(miners))

all_rtns <- aggregate(all_perf['ReturnOnInvested'],list(DateTime=all_perf$DateTime),mean)
all_rtns <- rbind(all_rtns,added_rtns)
all_rtns <- merge(all_rtns,oil[c('DateTime','oReturn')],by='DateTime',all.x=TRUE)
all_rtns <- merge(all_rtns,miners[c('DateTime','mReturn')],by='DateTime',all.x=TRUE)
colnames(all_rtns) <- c('Date','MBAM','WTI','NMX1770')

#Compute rolling correlation and beta
s <- start
e <- as.Date(s %m+% days(60))
oil_correlation <- list()
oil_beta <- list()
miners_correlation <- list()
miners_beta <- list()
dts <- list()
cnt <- 1
while(e <= end){
  df <- all_rtns[(all_rtns$Date<=e)&(all_rtns$Date>=s),]
  oil_correlation[[cnt]] <- cor(df$MBAM,df$WTI,use="na.or.complete")
  miners_correlation[[cnt]] <- cor(df$MBAM,df$NMX1770,use="na.or.complete")
  dts[[cnt]] <- e
  oil_beta[[cnt]] <- coefficients(lm(MBAM~WTI,df))[2]
  miners_beta[[cnt]] <- coefficients(lm(MBAM~NMX1770,df))[2]
  cnt <- cnt + 1
  s <- as.Date(s %m+% days(1))
  e <- as.Date(e %m+% days(1)) 
  message(e)
}
res_data <- rbind(data.frame(Type='Correlation',Key='WTI cash',Date=as.Date(unlist(dts)),Value=unlist(oil_correlation)),
                  data.frame(Type='Beta',Key='WTI cash',Date=as.Date(unlist(dts)),Value=unlist(oil_beta)),
                  data.frame(Type='Correlation',Key='FTSE Mining',Date=as.Date(unlist(dts)),Value=unlist(miners_correlation)),
                  data.frame(Type='Beta',Key='FTSE Mining',Date=as.Date(unlist(dts)),Value=unlist(miners_beta)))

plt <- ggplot(data=res_data, aes(x=Date, y=Value, group = Key, colour = Key)) +
  geom_line(size = 1) + 
  ylab("") + xlab('Date') + ggtitle('60d Rolling correlation and beta to oil and miners') + scale_linetype_discrete(name = "") +
  theme(text = element_text(size=15)) +
  facet_grid(Type~.,scales="free_y") 


