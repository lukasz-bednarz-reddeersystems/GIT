sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(functional)
library(quantmod)
library(RODBC)
library(snow)

declare_local_cluster <- function(ncores){
  cl <- makeCluster(rep('localhost',ncores),type='SOCK')
}

prepare_cluster <- function(cl){
  clusterEvalQ(cl,library(functional))
  clusterExport(cl,list('omit_value','rgr_kernel','instrument_regression'))
  return(cl)
}

get_region_table <- function(){
  SQL <- 'SELECT [lRegionID],[sRegionName],[bMajorRegion] FROM [Research].[dbo].[tRegion]'
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  regions <- sqlQuery(cn,SQL)
  close(cn)
  return(regions)
}

get_instrument_region <- function(){
  SQL <- 'prInstrument_SelectRegionMappings'
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  ins_regions <- sqlQuery(cn,SQL)
  close(cn)
  return(ins_regions)
}

get_bulk_price_data <- function(start,end){
  message("Price data bulk fetch from DB...")
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  SQL <- paste("prInstrumentHistory_SelectByDate '",start,"', '",end,"'",sep="")
  price_data <- sqlQuery(cn,SQL)
  close(cn)
  return(price_data)
}

get_bulk_price_data_by_regions <- function(start,end,regions){
  pd <- get_bulk_price_data(start,end)
  region_map <- get_instrument_region()
  region_map <- region_map[region_map$lRegionID%in%regions,]
  region_price_data <- merge(pd,region_map,by='lInstrumentID')
  return(region_price_data)
}

get_price_data <- function(instruments,start,end){
  ds <- seq(ymd(start),ymd(end),by='1 day')
  ds <- ds[wday(ds)!=7&wday(ds)!=1]
  dates <- merge(data.frame(Date=ds),data.frame(Instrument=instruments),all=TRUE)
  first <- TRUE
  for(ins in instruments){
    pdata <- data_request("instrument_price",data.frame(lInstrumentID=ins,dtDateTime=as.Date(ds)),c("dblClosePrice","dblPreviousClosePrice"))
    pdata <- pdata@data
    colnames(pdata) <- c('Instrument','Date','ClosePrice','PreviousClosePrice')
    if(first){
      rval <- pdata
      first<- FALSE
    }
    else{
      rval <- rbind(rval,pdata)
    }
  }
  return(rval)
}

get_bulk_price_data <- function(start,end){
  message("Price data bulk fetch from DB...")
  cn <- odbcConnect('RAIDSTAGEDB',uid='guy.billings')
  SQL <- paste("prInstrumentHistory_SelectByDate '",start,"', '",end,"'",sep="")
  price_data <- sqlQuery(cn,SQL)
  close(cn)
  return(price_data)
}

get_stock_returns <- function(instruments,start,end){
  rtn <- get_price_data(instruments,start,end)
  rtn$Return <- (rtn$ClosePrice/rtn$PreviousClosePrice)-1
  rtn <- rtn[c('Instrument','Date','Return')]
  return(rtn)
}

get_bulk_stock_returns <- function(start,end){
  rtn <- get_bulk_price_data(start,end)
  rtn$Return <- (rtn$dblClosePrice/rtn$dblPreviousClosePrice)-1Ret
  rtn <- rtn[c('lInstrumentID','dtDateTime','Return')]
  colnames(rtn) <- c('Instrument','Date','Return')
  rtn$Date <- as.Date(rtn$Date)
  return(rtn)
}

get_region_stock_returns <- function(start,end,regions){
  rtn <- get_bulk_price_data_by_regions(start,end,regions)
  rtn$Return <- (rtn$dblClosePrice/rtn$dblPreviousClosePrice)-1
  rtn <- rtn[c('lInstrumentID','dtDateTime','Return')]
  colnames(rtn) <- c('Instrument','Date','Return')
  rtn$Date <- as.Date(rtn$Date)
  return(rtn)
}

get_factor_returns <- function(start,end,factors){
  ds <- seq(ymd(start),ymd(end),by='1 day')
  factor_data <- data_request("risk_factor_returns",data.frame(dtDateTime=as.Date(ds)),c("sFactorName","dblChangePercent"))
  factor_rtns <- factor_data@data
  factor_rtns <- merge(factor_rtns,data.frame(sFactorName=factors),by=c('sFactorName'))
  factor_rtns$sFactorName <- as.character(factor_rtns$sFactorName)
  factor_rtns$dtDateTime  <- as.Date(factor_rtns$dtDateTime)
  factor_rtns$dblChangePercent <- factor_rtns$dblChangePercent/100
  colnames(factor_rtns) <- c('FactorName','Date','Return')
  return(factor_rtns)
}

get_risk_factor_returns <- function(start,end){
  factors <- c('rValue','rStrength','rGrowth','rSize','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')
  factor_rtns <- get_factor_returns(start,end,factors)
  factor_rtns$FactorName <- substr(factor_rtns$FactorName,2,nchar(factor_rtns$FactorName))
  return(factor_rtns)
}

get_sector_returns <- function(start,end){
  sectors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SX1P','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')
  return(get_factor_returns(start,end,sectors))
}

get_FX <- function(start,end){
  pairs <- c('JPY/USD','GBP/USD','EUR/USD','CNY/USD','RUB/USD','ZAR/USD','HKD/USD','AUD/USD','DKK/USD','NOK/USD','SEK/USD','CHF/USD','ILS/USD','PLN/USD','HUF/USD','TRY/USD')
  getFX(pairs,from=start,to=end)
  get_pairs <- gsub('/','',pairs)
  rval <- data.frame(Date=as.Date(rownames(as.data.frame(get(get_pairs[1])))))
  for(pair in get_pairs){
    rval <- cbind(rval,as.data.frame(get(pair)))
  }
  return(rval)
}

get_FX_returns <- function(start,end){
  fx <- get_FX(as.Date(start)-1,end)
  nmes <- setdiff(colnames(fx),'Date')
  new_nmes <- gsub(".USD","",nmes)
  rtn <- fx
  rtn <- rtn[order(rtn$Date),]
  for(n in 1:length(new_nmes)){
    rtn[new_nmes[n]] <- c(NA,(rtn[2:nrow(rtn),nmes[n]]/rtn[1:(nrow(rtn)-1),nmes[n]])-1)
  }
  rtn <- rtn[c('Date',new_nmes)]
  rtn <- rtn[rtn$Date>(as.Date(start)-1),]
  return(rtn)
}

get_commodities <- function(start,end){
  getSymbols('DCOILWTICO',src='FRED') 
  oil <- as.data.frame(DCOILWTICO)
  oil <- cbind(Date=as.Date(rownames(oil)),oil)
  colnames(oil) <- c('Date','WTICASH')
  oil <- oil[oil$Date>=as.Date(start)&oil$Date<=as.Date(end),]
  return(oil)
}

get_commodity_returns <- function(start,end){
  rtn <- get_commodities(as.Date(start)-1,end)
  rtn$WTI <- c(NA,(rtn[2:nrow(rtn),'WTICASH']/rtn[1:(nrow(rtn)-1),'WTICASH'])-1)
  rtn <- rtn[c('Date','WTI')]
  rtn <- rtn[rtn$Date>(as.Date(start)-1),]
  return(rtn)
}

get_trader_allocation <- function(trader,start,end){
  dts <- unique(format(seq(ymd(start),ymd(end),by='1 day'),'%Y-%m'))
  first <- TRUE
  for(d in dts){
    cd <- paste(as.character(d),'-01',sep="")
    a <- data_request("trader_allocation",data.frame(lUserID=trader,dtDateFrom=c(as.Date(cd),as.Date(cd))),c('dblValue'))
    if(first){
      allocation <- a@data
      first <- FALSE 
    }
    else{
      allocation <- rbind(allocation,a@data)
    }
  }
  colnames(allocation) <- c('TraderID','Date','Allocation')
  return(allocation)
}

build_portfolio_history <- function(trader,start,end){
  holdings_data <- position_composite_factory(as.integer(trader),as.Date(start),as.Date(end))
  holdings_data <- holdings_data@data@data[c('Name','Trader','UserID','Direction','InstrumentID','Date','MarketValue')]
  colnames(holdings_data) <- c('Strategy','Trader','TraderID','Direction','InstrumentID','Date','MarketValue')
  allocation    <- get_trader_allocation(trader,start,end)
  allocation$Month <- format(allocation$Date,'%Y-%m')
  holdings_data$Month <- format(holdings_data$Date,'%Y-%m')
  holdings_data <- merge(holdings_data,allocation[c('TraderID','Allocation','Month')],by=c('Month','TraderID'))
  holdings_data$Weight <- holdings_data$MarketValue/holdings_data$Allocation
  return(holdings_data)
}

pivot_frame <- function(frame,pivot_on,value_col,date_col){
  pvals <- unique(frame[[pivot_on]])
  first <- TRUE
  for(p in pvals){
    if(first){
      df <- frame[frame[pivot_on]==p,c(date_col,value_col)]
      colnames(df) <- c('Date',p)
      rval <- df
      first <- FALSE
    }
    else{
      df <- data.frame(x=frame[frame[pivot_on]==p,value_col])
      colnames(df) <- c(p)
      rval <- cbind(rval,df)
    }
  }
  return(rval)
}

omit_value <- function(values_arr){
  values_arr <- unlist(values_arr)
  return(is.na(values_arr)|is.infinite(values_arr)|is.nan(values_arr))
}

rgr_kernel <- function(xi,data){
  omit_rows <- omit_value(xi)|omit_value(data$Return)
  rval <- tryCatch({
                lm.fit(x=cbind(1,xi[!omit_rows]), y=data$Return[!omit_rows])
              },error=function(cond){
                message(paste("Beta timeseries regression failed:",cond))
              })
  return(rval)
}

instrument_regression <- function(ins,all_data){
    data <- all_data[all_data$Instrument==ins,]
    data <- data[setdiff(colnames(data),'Date')]
    rgr <- apply(data[setdiff(colnames(data),c('Instrument','Return'))], 2, Curry(rgr_kernel,data=data))
    if(length(rgr)>0){
      rval <- unlist(Map(function(x)x[[1]][2],rgr))
      names(rval) <- gsub('.x2','',names(rval))
      rval <- cbind(Instrument=ins,as.data.frame(t(rval)))  
    }
    else{
      nmes <- setdiff(colnames(data),c('Instrument','Return'))
      df <- data.frame(t(rep(NA,length(nmes))))
      rval <- cbind(Instrument=ins,df)   
      colnames(rval) <- c('Instrument',nmes)
    }
    return(rval)
}

#Expects stock return frame with columns Instrument, Date and Return
#Expects factor frame with column for each factor and Date.
stock_betas <- function(stocks,factors,cl=NULL){
  fnames <- setdiff(colnames(factors),'Date')
  all_data <- merge(stocks,factors,by='Date')
  all_data[is.na(all_data)] <- 0
  all_data <- cbind(all_data[c('Date','Instrument')],data.frame(Map(function(x)log(x+1),all_data[setdiff(colnames(all_data),c('Instrument','Date'))])))
  instruments <- unique(stocks$Instrument)
  if(length(cl)==0){
    all_beta <- apply(data.frame(Instrument=instruments),1,Curry(instrument_regression,all_data=all_data))
  }
  else{
    all_beta <- parApply(cl,data.frame(Instrument=instruments),1,Curry(instrument_regression,all_data=all_data))  
  }
  all_beta <- Reduce(function(x,y)rbind(x,y),all_beta)
  return(all_beta)
}

#Point in time estimation of risk model:
#Expects stock return frame with columns Instrument and Return
#Expects stock betas in a frame with one column for Instrument and a column for each factor
#Expects stock weights in frame with columns for Instrument and Weight
x_sectional_model <- function(stock_return,stock_betas){
  weighted_return <- merge(stock_return,stock_betas,by='Instrument')
  weighted_return$PfoRtn <- log(weighted_return$Return+1)
  factors <- setdiff(colnames(stock_betas),'Instrument')
  omit_rows <- omit_value(weighted_return$PfoRtn)
  weighted_return <- weighted_return[!omit_rows,]
  xmdl <- lm(paste("PfoRtn ~ ",paste(factors,collapse=" + "),sep=""),weighted_return)
  implied_rtn <- xmdl[[1]][2:length(xmdl[[1]])]
  implied_rtn[is.na(implied_rtn)] <- 0
  estimates <- t(as.matrix(implied_rtn))%*%t(as.matrix(stock_betas[2:ncol(stock_betas)]))
  estimates <- data.frame(Instrument=stock_betas$Instrument,mdlReturn=t(estimates))
  resid_returns <- merge(weighted_return,estimates,by='Instrument')
  resid_returns$Return <- resid_returns$PfoRtn - resid_returns$mdlReturn
  return(list(log_implied_rtn=implied_rtn,residual_log_rtn=resid_returns[c('Instrument','Return')]))
}

#Point in time composite model computed against preceeding block residuals:
#Expects stock return frame with columns Instrument and Return
#Expects stock weights in frame with columns for Instrument and Weight
#Factor blocks is a list of dataframes with one column for Instrument and a column for each factor
composite_model <- function(stock_returns_on_date,factor_blocks){
  rtn <- stock_returns_on_date
  first <- TRUE
  for(fb in factor_blocks){
    m <- x_sectional_model(rtn,fb)
    if(first){
      all_implied <- as.data.frame(t(m[[1]]))
      first <- FALSE
    }
    else{
      all_implied <- cbind(all_implied,as.data.frame(t(m[[1]]))) 
    }
    rtn <- m[[2]]
  }
  return(list(log_implied_rtns=all_implied,log_residual_returns=rtn))
}

#Compute the factor correlation matrix from the implied factor returns
factor_correlation <- function(implied_factor_return){
  return(cor(implied_fct_rtns[setdiff(colnames(implied_fct_rtns),'Date')]))
}

stock_correlation <- function(betas,implied_returns,factor_correlation_matrix,risk_model_date){
  instruments <- betas[betas$Date==risk_model_date,'Instrument']
  ir <- implied_fct_rtns[implied_fct_rtns$Date==risk_model_date,setdiff(colnames(implied_fct_rtns),'Date')]
  beta_matrix <- betas[betas$Date==risk_model_date,setdiff(colnames(betas),c('Instrument','Date'))]
  beta_matrix <- data.frame(mapply('*',beta_matrix,ir))
  beta_matrix <- as.matrix(beta_matrix)
  stock_correlation <- beta_matrix%*%factor_correlation_matrix%*%t(beta_matrix)
  rownames(stock_correlation) <- instruments
  colnames(stock_correlation) <- instruments
  return(stock_correlation)
}

factor_visualisation_data <- function(matrix,con_threshold){
  matrix[!upper.tri(matrix)] <- NA
  matrix <- t(matrix)
  market_factors <- c('Value','Strength','Growth','Size','StreetSentiment','PriceMomentum1M','PriceMomentum12M','TrendExtension','Earnings','Volatility')
  fx_factors <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')
  commodities <- c('WTI')
  sectors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SX1P','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')
  df <- as.data.frame(matrix)
  factors <- colnames(df)
  nfirst <- TRUE
  ntve_net <- NA
  pfirst <- TRUE
  ptve_net <- NA
  first <- TRUE
  for(factor in factors){
    if(factor %in% market_factors){
      grp <- 1
      l <- length(market_factors)
    }
    else if(factor %in% fx_factors){
      grp <- 2
      l <-length(fx_factors)
    }
    else if(factor %in% commodities){
      grp <- 3
      l <- length(commodities)
    }
    else{
      grp <- 4
      l <- length(sectors)
    }
    pv <- 10*df[!is.na(df[factor])&df[factor]>con_threshold,factor]
    if(sum(!is.na(pv))>0){
      ptve_connections <- data.frame(factor,src=which(colnames(df)==factor)-1,tgt=which(df[factor]>con_threshold)-1,value=pv)
      ptve_connections <- ptve_connections[!is.na(ptve_connections$value),]  
      if(pfirst){
        ptve_net <- ptve_connections
        pfirst <- FALSE
      }
      else{
        ptve_net <- rbind(ptve_net,ptve_connections)
      }
    }
    nv <- 10*abs(df[!is.na(df[factor])&df[factor]<(-1*con_threshold),factor])
    if(sum(!is.na(nv))>0){
      ntve_connections <- data.frame(src=which(colnames(df)==factor)-1,tgt=which(df[factor]<(-1*con_threshold))-1,value=nv)
      ntve_connections <- ntve_connections[!is.na(ntve_connections$value),]      
      if(nfirst){
        ntve_net <- ntve_connections
        nfirst <- FALSE
      }
      else{
        ntve_net <- rbind(ntve_net,ntve_connections)
      }
    }
    nodes <- data.frame(name=factor,group=grp,size=1)
    if(first){
      all_nodes <- nodes
      first <- FALSE
    }
    else{
      all_nodes <- rbind(all_nodes,nodes)
    }
  }
  return(list(postive_net=ptve_net,negative_net=ntve_net,nodes=all_nodes))
}
#forceNetwork(Links = cnet[[1]][c('src','tgt','value')], Nodes = cnet[[3]],
#                           Source = "src", Target = "tgt",
#                           Value = "value", NodeID = "name",
#                           Group = "group", opacity = 0.7, zoom=TRUE)

stock_visualisation_data <- function(matrix,con_threshold){
  matrix[!upper.tri(matrix)] <- NA
  matrix <- t(matrix)
  df <- as.data.frame(matrix)
  factors <- colnames(df)
  nfirst <- TRUE
  ntve_net <- NA
  pfirst <- TRUE
  ptve_net <- NA
  first <- TRUE
  for(factor in factors){
    pv <- 10*df[!is.na(df[factor])&df[factor]>con_threshold,factor]
    if(sum(!is.na(pv))>0){
      ptve_connections <- data.frame(factor,src=which(colnames(df)==factor)-1,tgt=which(df[factor]>con_threshold)-1,value=pv)
      ptve_connections <- ptve_connections[!is.na(ptve_connections$value),]  
      if(pfirst){
        ptve_net <- ptve_connections
        pfirst <- FALSE
      }
      else{
        ptve_net <- rbind(ptve_net,ptve_connections)
      }
    }
    nv <- 10*abs(df[!is.na(df[factor])&df[factor]<(-1*con_threshold),factor])
    if(sum(!is.na(nv))>0){
      ntve_connections <- data.frame(src=which(colnames(df)==factor)-1,tgt=which(df[factor]<(-1*con_threshold))-1,value=nv)
      ntve_connections <- ntve_connections[!is.na(ntve_connections$value),]      
      if(nfirst){
        ntve_net <- ntve_connections
        nfirst <- FALSE
      }
      else{
        ntve_net <- rbind(ntve_net,ntve_connections)
      }
    }
    nodes <- data.frame(name=factor,group=1,size=1)
    if(first){
      all_nodes <- nodes
      first <- FALSE
    }
    else{
      all_nodes <- rbind(all_nodes,nodes)
    }
  }
  return(list(postive_net=ptve_net,negative_net=ntve_net,nodes=all_nodes))
}

