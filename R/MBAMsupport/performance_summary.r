setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../reporting/raid_data_import.r")
library(ggplot2)
library(quantmod)
library(lubridate)
library(formattable)
library(htmltools)
library(webshot)
library(png)
library(grid)
library(gridExtra)
library(useful)
library(RODBC)

export_formattable <- function(f, file, width = "100%", height = NULL, background = "white", delay = 0.2){
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

start <- '2016-04-01'
end   <- '2016-06-30'
start_last_q <- '2016-01-01'
end_last_q <-   '2016-03-31'

fund_perf <- rbind(get_trader_performance_simple(11,as.Date(start),as.Date(end),mbam=TRUE),
                   get_trader_performance_simple(70,as.Date(start),as.Date(end),mbam=TRUE),
                   get_trader_performance_simple(101,as.Date(start),as.Date(end),mbam=TRUE),
                   get_trader_performance_simple(11,as.Date(start_last_q),as.Date(end_last_q),mbam=TRUE),
                   get_trader_performance_simple(70,as.Date(start_last_q),as.Date(end_last_q),mbam=TRUE),
                   get_trader_performance_simple(101,as.Date(start_last_q),as.Date(end_last_q),mbam=TRUE))

top_psns   <- rbind(get_top_positions(11,as.Date(start),as.Date(end)),
                   get_top_positions(70,as.Date(start),as.Date(end)),
                   get_top_positions(101,as.Date(start),as.Date(end)))
bottom_psns<- rbind(get_bottom_positions(11,as.Date(start),as.Date(end)),
                   get_bottom_positions(70,as.Date(start),as.Date(end)),
                   get_bottom_positions(101,as.Date(start),as.Date(end)))

s <- as.Date(start)
e <- s+1

first <- TRUE
for(d in 1:(as.Date(end)-as.Date(start))){
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

first <- TRUE
for(trader in c(11,70,101)){
  df <- get_single_url(paste("http://raidapp2:8083/tradinganalysis/liquiditysummary?id=",trader,"&start=",start,"&end=",end,sep=""))
  pf <- get_single_url(paste("http://raidapp2:8083/positions/positionhistory?id=",trader,"&start=",start,"&end=",end,sep=""))
  lf <- get_single_url(paste("http://raidapp2:8083/tradinganalysis/liquiditypositionsummary?id=",trader,"&start=",start,"&end=",end,sep=""))
  df <- cbind(Trader=trader,df)
  if(first){
    liquidity_smmry <- df
    all_positions <- pf
    psn_adv <- lf
    first <- FALSE
  } else {
    liquidity_smmry <- rbind(liquidity_smmry,df)
    all_positions <- rbind(all_positions,pf)
    psn_adv <- rbind(psn_adv,lf)
  }
}
#get tikers
get_all_tickers <- function(){
  cn <- odbcConnect('RAIDLIVEDB',uid='guy.billings')
  SQL <- "SELECT sBBGTicker, lInstrumentID FROM tInstrument"
  tickers <- sqlQuery(cn,SQL)
  close(cn)
  return(tickers)
}
tckrs <- get_all_tickers()
tckrs$sBBGTicker <- gsub('Equity','',tckrs$sBBGTicker)
colnames(tckrs) <- c('Ticker','InstrumentID')
all_positions <- merge(all_positions,tckrs,by='InstrumentID')

liquidity_smmry$Description <- gsub('â???"','-',liquidity_smmry$Description)
psn_adv$ADVBand <- gsub('â???"','-',psn_adv$ADVBand)
liquidity_smmry <- aggregate(liquidity_smmry['PositionsGrossExposure'],list(Band=liquidity_smmry$Description),sum)
liquidity_smmry$Exposure <- liquidity_smmry$PositionsGrossExposure/sum(liquidity_smmry$PositionsGrossExposure)
liquidity_smmry$Exposure <- percent(liquidity_smmry$Exposure)
liquidity_smmry <- liquidity_smmry[order(-liquidity_smmry$Exposure),c('Band','Exposure')] 
liquidity_smmry$Order <- c(1,2,4,5,3)
liquidity_smmry <- liquidity_smmry[order(liquidity_smmry$Order),c('Band','Exposure')]

cscaler <- function(x)log(100*as.numeric(x)+1)/log(100)

liquidity_widget <- formattable(liquidity_smmry,list(Exposure = formatter("span", style = function(x) style(display = "block", 
                                                                                                            "border-radius" = "4px", 
                                                                                                            "padding-right" = "4px", "background" = paste("linear-gradient(to right, rgba(255, 255, 255, ",cscaler(x),"), rgba(255, 0, 0, ",cscaler(x),"))",sep="")))),
                                                     row.names=c())



getSymbols("^SX5E")
index <- data.frame(DateTime=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])

#Fund-level PL change widget
df <- aggregate(fund_perf[c('PL','ReturnOnAllocated')],list(Date=fund_perf$DateTime),sum)
df <- cbind(Change=c(NA,TRUE),df) #Change hardcoded, make dynamic
df <- df[df$Date==start,c('ReturnOnAllocated','PL','Change')]
df <- rbind(df,cbind(Change=TRUE,fund_perf[fund_perf$DateTime==start&fund_perf$TraderID==11,c('ReturnOnAllocated','PL')]))
df <- cbind(Type=c('Fund level','Best book - JS'),df) #Best book hardcode ... make dynamic
colnames(df) <- c('Performance','Return','PL','Change')

performance_widget <- formattable(df,list( Performance = formatter("span",style = x ~ style(display="block","border-radius" = "4px","background-color"=ifelse(x == "Fund level","orange","white"))),
                                           Return = formatter("span",style = x ~ ifelse(x > 0, style(color = "green", font.weight = "bold"), style(color = "red", font.weight = "bold")),x ~ sprintf("%.2f%%", x*100)),
                                           PL     = formatter("span",style = x ~ ifelse(x > 0, style(color = "green", font.weight = "bold"), style(color = "red", font.weight = "bold")),x ~ ifelse(abs(x)<1000000,sprintf("$%.2fk",x/1000),sprintf("$%.2fm",x/1000000))),
                                           Change = formatter("span", style = x ~ style(color = ifelse(x, "green", "red")),x ~ icontext(ifelse(x, "arrow-up", "arrow-down")))),
                                     row.names=c())


#Fund-level volatility widget
df <- aggregate(fund_perf[c('AnnualisedStdDeviationOfReturns')],list(Date=fund_perf$DateTime),function(x)sqrt(sum(x^2)/3))
df$Quarter <- paste("Q",quarter(df$Date),sep="")
df$AnnualisedStdDeviationOfReturns <- df$AnnualisedStdDeviationOfReturns
df <- df[c('Quarter','AnnualisedStdDeviationOfReturns')]
colnames(df) <- c("Quarter","Ann.Volatilty")

volatility_widget <- formattable(df,list( Quarter = formatter("span",style = x ~ style(display="block","border-radius" = "4px","background-color"=ifelse(x == "Q2","orange","white"))),
                                          Ann.Volatilty = formatter("span",x ~ sprintf("%.2f%%", x*100))),
                                  row.names=c())



#Beta and correlation widget
rtns <- merge(raw_perf,index,by='DateTime')
cor_data <- rtns[rtns$TraderID==0,]
cor_plot <- ggplot(data=cor_data,aes(x=SX5E.Close,y=ReturnOnAllocated)) +
            ggtitle("Return Vs.Market") +
            xlab("Market return") + ylab("Fund return") +
            geom_point() +
            geom_smooth(method="lm")
#cor_plot <- ggMarginal(cor_plot)

#Regression statistics
rtn_cor <- cor(cor_data$ReturnOnAllocated,cor_data$SX5E.Close)*100
rtn_fit <- lm(ReturnOnAllocated~SX5E.Close,cor_data)
alpha <- rtn_fit[[1]][[1]]*10000
beta <- rtn_fit[[2]][[1]]*100
regression_table <- data.frame(Parameter=c('Alpha','Beta','Correlation'),Vs.Market=c(alpha,beta,rtn_cor))
regression_widget<- formattable(regression_table,list( Parameter = formatter("span",style = x ~ style(color = ifelse(x=="Alpha", "orange", ifelse(x=="Beta", "blue", ifelse(x=="Correlation","black",NA))))),
                                                       Vs.Market = formatter("span",x ~ ifelse(x==alpha,sprintf("%.2f bps", x),sprintf("%.2f%%", x)))),
                                                       row.names=c())

png("performace.png")
grid.newpage() 
pushViewport(viewport(layout = grid.layout(10, 2)))
export_formattable(performance_widget,'./performance_widget.png',width="50%")
img <- readPNG("./performance_widget.png")
grid.raster(img,vp=vplayout(1:2,1:2))
export_formattable(volatility_widget,'./volatility_widget.png',width="25%")
img <- readPNG("./volatility_widget.png")
grid.raster(img,vp=vplayout(4:5,1))
export_formattable(liquidity_widget,'./liquidity_widget.png',width="25%")
img <- readPNG("./liquidity_widget.png")
grid.raster(img,vp=vplayout(6:10,2))
dev.off()

png("regression.png",width=480,height=200)
grid.newpage() 
pushViewport(viewport(layout = grid.layout(3, 2)))
print(cor_plot,vp=vplayout(1:3,2))
export_formattable(regression_widget,'./regression_widget.png',width="25%")
img <- readPNG("./regression_widget.png")
grid.raster(img,vp=vplayout(1:2,1))
dev.off()

#Build top positions table
top_aggregate <- aggregate(top_psns['MONEY|PL'],list(Ticker=top_psns[['STRING|Ticker']]),sum)
top_aggregate <- top_aggregate[!grepl("Index",top_aggregate$Ticker),]
colnames(top_aggregate) <- c('Ticker','PL')
top_tickers <- head(top_aggregate[order(-top_aggregate$PL),],n=10)
bottom_aggregate <- aggregate(bottom_psns['MONEY|PL'],list(Ticker=bottom_psns[['STRING|Ticker']]),sum)
bottom_aggregate <- bottom_aggregate[!grepl("Index",bottom_aggregate$Ticker),]
colnames(bottom_aggregate) <- c('Ticker','PL')
bottom_tickers <- head(bottom_aggregate[order(bottom_aggregate$PL),],n=10)

size_aggregate <- aggregate(all_positions['MarketValue'],list(Ticker=all_positions$Ticker,Date=all_positions$Date),sum)
size_aggregate <- aggregate(size_aggregate['MarketValue'],list(Ticker=size_aggregate$Ticker),mean)
size_aggregate <- size_aggregate[!grepl("Index",size_aggregate$Ticker),]
colnames(size_aggregate) <- c('Ticker','Av.Value')
big_tickers <- head(size_aggregate[order(-size_aggregate$Av.Value),],n=10)
short_tickers <- head(size_aggregate[order(size_aggregate$Av.Value),],n=10)

adv_aggregate <- aggregate(psn_adv['PercentageOfADV'],list(Ticker=psn_adv$Ticker),sum)
adv_aggregate <- adv_aggregate[!grepl("Index",adv_aggregate$Ticker),]
adv_tickers <- head(adv_aggregate[order(-adv_aggregate$PercentageOfADV),],n=10)
colnames(adv_tickers) <- c('Ticker','ADV')

position_table <- data.frame(Winners=apply(top_tickers,1,function(x)paste(x[1]," $",round(as.numeric(x[2])),sep="")),
                              Loosers=apply(bottom_tickers,1,function(x)paste(x[1]," -$",round(abs(as.numeric(x[2]))),sep="")),
                              Long = apply(big_tickers,1,function(x)paste(x[1]," $",round(as.numeric(x[2])),sep="")),
                              Short = apply(short_tickers,1,function(x)paste(x[1]," -$",round(abs(as.numeric(x[2]))),sep="")),
                              ADV = apply(adv_tickers,1,function(x)paste(x[1]," ",round(as.numeric(x[2])),sep="")))

green_tickers <- unique(c(top_tickers$Ticker[top_tickers$Ticker%in%big_tickers$Ticker],top_tickers$Ticker[top_tickers$Ticker%in%short_tickers$Ticker],top_tickers$Ticker[top_tickers$Ticker%in%adv_tickers$Ticker]))
red_tickers <- unique(c(bottom_tickers$Ticker[bottom_tickers$Ticker%in%big_tickers$Ticker],bottom_tickers$Ticker[bottom_tickers$Ticker%in%short_tickers$Ticker],bottom_tickers$Ticker[bottom_tickers$Ticker%in%adv_tickers$Ticker]))

get_ticker <- function(y){
  unlist(Map(function(x)paste(strsplit(as.character(x)," ")[[1]][1:2],collapse=" "),y))
}

position_widget<- formattable(position_table,list( Winners = formatter("span",style = x ~ style(color=ifelse(get_ticker(x) %in% green_tickers,"black","green"),display="block","border-radius" = "4px","background-color"=ifelse(get_ticker(x) %in% green_tickers,"green",NA))),
                                                   Loosers = formatter("span",style = x ~ style(color=ifelse(get_ticker(x) %in% red_tickers,"black","red"),display="block","border-radius" = "4px","background-color"=ifelse(get_ticker(x) %in% red_tickers,"red",NA))),
                                                   Long = formatter("span",style = x ~ style(color=ifelse(get_ticker(x) %in% green_tickers,"black","green"),display="block","border-radius" = "4px","background-color"=ifelse(get_ticker(x) %in% red_tickers,"red",ifelse(get_ticker(x) %in% green_tickers,"green",NA)))),
                                                   Short = formatter("span",style = x ~ style(color=ifelse(get_ticker(x) %in% red_tickers,"black","red"),display="block","border-radius" = "4px","background-color"=ifelse(get_ticker(x) %in% red_tickers,"red",ifelse(get_ticker(x) %in% green_tickers,"green",NA)))),
                                                   ADV = formatter("span",style = x ~ style(color="black",display="block","border-radius" = "4px","background-color"=ifelse(get_ticker(x) %in% red_tickers,"red",ifelse(get_ticker(x) %in% green_tickers,"green",NA))))),
                              row.names=c())
export_formattable(position_widget,'./position_widget.png',width="68%")
