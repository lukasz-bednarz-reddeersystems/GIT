#' @include TE.RiskModel.r
NULL


.__DEFAULT_RISK_MODEL_DB__ <- new("RiskModelDefaults")@store_database
.__DEFAULT_RAID_DB_USER__ <- Sys.info()["user"]



declare_local_cluster <- function(ncores){
  cl <- makeCluster(rep('localhost',ncores), type = 'SOCK')
}

prepare_cluster <- function(cl){
  clusterEvalQ(cl,library(functional))
  clusterExport(cl,
                list('omit_value',
                     'rgr_kernel',
                     'rlm_rgr_kernel',
                     'instrument_regression'),
                envir  = as.environment("package:TE.RiskModel"))
  return(cl)
}

get_region_table <- function(){
  SQL <- 'SELECT [lRegionID],[sRegionName],[bMajorRegion] FROM [Research].[dbo].[tRegion]'
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__)
  regions <- sqlQuery(cn,SQL)
  close(cn)
  return(regions)
}

get_instrument_region <- function(){
  SQL <- 'prInstrument_SelectRegionMappings'
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__)
  ins_regions <- sqlQuery(cn,SQL)
  close(cn)
  return(ins_regions)
}

get_bulk_price_data <- function(start,end){
  message("Price data bulk fetch from DB...")
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__)
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

get_stock_returns <- function(instruments,start,end){
  rtn <- get_price_data(instruments,start,end)
  rtn$Return <- (rtn$ClosePrice/rtn$PreviousClosePrice)-1
  rtn <- rtn[c('Instrument','Date','Return')]
  return(rtn)
}

get_bulk_stock_returns <- function(start,end){
  rtn <- get_bulk_price_data(start,end)
  rtn$Return <- (rtn$dblClosePrice/rtn$dblPreviousClosePrice)-1
  rtn <- rtn[c('lInstrumentID','dtDateTime','Return')]
  colnames(rtn) <- c('Instrument','Date','Return')
  rtn$Date <- as.Date(rtn$Date)
  return(rtn)
}

get_region_stock_returns <- function(start,end,regions){
  rtn <- get_bulk_price_data_by_regions(start,end,regions)
  rtn$Return <- (rtn$dblClosePrice/rtn$dblPreviousClosePrice)-1
  rtn <- rtn[c('lInstrumentID','dtDateTime','dblClosePrice','Return')]
  colnames(rtn) <- c('Instrument','Date','ClosePrice','Return')
  rtn$Date <- as.Date(rtn$Date)
  return(rtn)
}



get_risk_factor_returns <- function(start,end){
  factors <- c('rValue','rStrength','rGrowth','rSize','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')

  ds <- seq(ymd(start),ymd(end),by='1 day')
  ds <- ds[wday(ds)!=7&wday(ds)!=1]
  factor_data <- data_request("risk_factor_returns",data.frame(dtDateTime=as.Date(ds)),c("sFactorName","dblChangePercent"))
  factor_rtns <- factor_data@data
  factor_rtns <- merge(factor_rtns,data.frame(sFactorName=factors),by=c('sFactorName'))
  factor_rtns$sFactorName <- as.character(factor_rtns$sFactorName)
  factor_rtns$dtDateTime  <- as.Date(factor_rtns$dtDateTime)
  factor_rtns$dblChangePercent <- factor_rtns$dblChangePercent/100
  colnames(factor_rtns) <- c('FactorName','Date','Return')
  factor_rtns$FactorName <- substr(factor_rtns$FactorName,2,nchar(factor_rtns$FactorName))
  return(factor_rtns)
}

get_sector_returns <- function(start,end){
  sectors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SX1P','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')

  ds <- seq(ymd(start),ymd(end),by='1 day')
  ds <- ds[wday(ds)!=7&wday(ds)!=1]
  factor_data <- data_request("risk_factor_returns",data.frame(dtDateTime=as.Date(ds)),c("sFactorName","dblChangePercentAbsolute"))
  factor_rtns <- factor_data@data
  factor_rtns <- merge(factor_rtns,data.frame(sFactorName=sectors),by=c('sFactorName'))
  factor_rtns$sFactorName <- as.character(factor_rtns$sFactorName)
  factor_rtns$dtDateTime  <- as.Date(factor_rtns$dtDateTime)
  factor_rtns$dblChangePercent <- factor_rtns$dblChangePercent
  colnames(factor_rtns) <- c('FactorName','Date','Return')

  return(factor_rtns)
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
  rtn <- rtn[wday(rtn$Date)!=7&wday(rtn$Date)!=1, ]

  for(n in 1:length(new_nmes)){
    rtn[new_nmes[n]] <- c(NA,(rtn[2:nrow(rtn),nmes[n]]/rtn[1:(nrow(rtn)-1),nmes[n]])-1)
  }
  rtn <- rtn[c('Date',new_nmes)]
  rtn <- rtn[rtn$Date>(as.Date(start)-1),]
  return(rtn)
}

get_commodities <- function(start,end){
  DCOILWTICO <- getSymbols('DCOILWTICO',src='FRED', auto.assign = FALSE)
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

#' Remove values that disrupt regression
#'
#' Clears array from NA's, INF's and NAN's
#'
#' @param values_arr array of values to be cleared
#' @return \code{values_arr} cleared array
#'
#' @export
omit_value <- function(values_arr){
  values_arr <- unlist(values_arr)

  ret <- tryCatch({
    is.na(values_arr)|is.infinite(values_arr)|is.nan(values_arr)|values_arr==0
  }, error = function(cond){
    browser()
    stop(cond)
  })

  return(is.na(values_arr)|is.infinite(values_arr)|is.nan(values_arr)|values_arr==0)
}

#' kernel function to run regression
#'
#' function to apply regression, but also to remove values that
#' will cause regression to fail.
#' using .lm.fit because it is faster
#'
#' @param xi array with x values
#' @param data array with values to be regressed
#' @return \code{rval} list with components returned by lm.fit
#'
#' @export
rgr_kernel <- function(xi,data){
  omit_rows <- omit_value(xi)|omit_value(data$Return)
  rval <- tryCatch({
                .lm.fit(x=cbind(1,xi[!omit_rows]), y=data$Return[!omit_rows])
              },error=function(cond){
                message(paste("Beta timeseries regression failed:",cond))
              })
  return(rval)
}

#' kernel function to run regression
#'
#' function to apply robust linear regression, but also to remove values that
#' will cause regression to fail.
#' using rlm
#'
#' @param xi array with x values
#' @param data array with values to be regressed
#' @return \code{rval} list with components returned by lm.fit
#'
#' @export
rlm_rgr_kernel <- function(xi,data){
  omit_rows <- omit_value(xi)|omit_value(data$Return)
  rval <- tryCatch({
    rlm(x=cbind(1,xi[!omit_rows]), y=data$Return[!omit_rows])
  },error=function(cond){
    message(paste("Beta timeseries regression failed:",cond))
  })
  return(rval)
}

#' Perform regressions for one instrument
#'
#' function to apply regression for all factors
#'
#' @param ins "integer" instrument id
#' @param all_data "data.frame" with all instruments returns
#' @return \code{rval} "data.frame" with all instruments betas in columns named after factor names
#'
#' @export
instrument_regression <- function(ins,all_data, beta_estimator = new("BetaEstimator.OLS")){
    data <- all_data[all_data$Instrument==ins,]
    data <- data[setdiff(colnames(data),'Date')]

    if (is(beta_estimator, "BetaEstimator.RLM")){
      kernel <- Curry(rlm_rgr_kernel,data=data)
    }
    else {
      kernel <- Curry(rgr_kernel,data=data)
    }

    rgr <- apply(data[setdiff(colnames(data),c('Instrument','Return'))], 2, kernel)
    if(length(rgr)>0){
      rval <- unlist(Map(function(x)x[["coefficients"]][2],rgr))
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



#Computes betas of each stock to each factor via timeseries regression
#Expects stock return frame with columns Instrument, Date and Return
#Expects factor frame with column for each factor and Date.
stock_betas <- function(stocks,factors,cl=NULL, beta_estimator = new("BetaEstimator.OLS")){

  fnames <- setdiff(colnames(factors),'Date')
  all_data <- merge(stocks,factors,by='Date')
  #all_data[is.na(all_data)] <- 0
  all_data <- cbind(all_data[c('Date','Instrument')],
                    data.frame(Map(function(x)suppressWarnings(log1p(x)),
                                   all_data[setdiff(colnames(all_data),
                                                    c('Instrument','Date'))])))
  # remove stock with zero returns and value below 1 USD
  include <- aggregate(Return ~ Instrument, data = all_data, function(x){ !all(x == 0, na.rm = TRUE)})
  include <- merge(include,
                   aggregate(ClosePrice ~ Instrument, data = all_data, function(x){ !all(x < 1, na.rm = TRUE)}))

  instruments <- unique(include$Instrument[include$Return &include$ClosePrice])

  if(length(cl)==0){
    all_beta <- apply(data.frame(Instrument=instruments),1,Curry(instrument_regression,all_data=all_data, beta_estimator))
  }
  else{
    all_beta <- parApply(cl,data.frame(Instrument=instruments),1,Curry(instrument_regression,all_data=all_data, beta_estimator))
  }
  all_beta <- Reduce(function(x,y)rbind(x,y),all_beta)
  return(all_beta)
}

#' Cross sectional factor return computation
#'
#' Point in time estimation of risk model:
#' Cross sectional regression of stock returns against raw factor returns
#' to produce the implied factor returns
#' Expects stock return frame with columns Instrument and Return
#' Expects stock betas in a frame with one column for Instrument and a column for each factor
#' Expects stock weights in frame with columns for Instrument and Weight
#'
#' @param stock_return "data.frame" with stock returns
#' @param stock_betas "data.frame" with stock betas
#' @return \code{x_sectional_model} list with two components :\cr
#' \code{x_sectional_model$log_implied_rtn} - "data.frame" with log of implied factor returns\cr
#' \code{x_sectional_model$log_implied_rtn} - "data.frame" with log of stock residual returns
x_sectional_model <- function(stock_return,stock_betas){
  weighted_return <- merge(stock_return,stock_betas,by='Instrument')
  weighted_return$PfoRtn <- log1p(weighted_return$Return)
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

#' Compute composite model for blocks of factors
#'
#' Point in time composite model computed against preceeding block residuals:
#' Single cross sectional regression model is built for each factor block using the
#' preceeding block residuals
#' Expects stock return frame with columns Instrument and Return
#' Expects stock weights in frame with columns for Instrument and Weight
#' Factor blocks is a list of dataframes with one column for Instrument and a column for each factor
#'
#' @param stock_returns_on_date "data.frame" with stock returns for date
#' @param factor_blocks list containing "data.frame" with returns of factors within a block
#' @return \code{x_sectional_model} list with two components :\cr
#' \code{x_sectional_model$log_implied_rtn} - "data.frame" with log of implied factor returns\cr
#' \code{x_sectional_model$log_implied_rtn} - "data.frame" with log of stock residual returns
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

#' Compute the factor correlation matrix from the implied factor returns
#'
#' @param implied_fct_rtns data.frame with implied factor returns
#' @return \code{factor_cor} "data.frame" with factor correlation
factor_correlation <- function(implied_fct_rtns){
  return(cor(implied_fct_rtns[setdiff(colnames(implied_fct_rtns),'Date')]))
}

#' Compute stock covariance matrix
#'
#' Use the stock betas and the factor correlation matrix to generate the
#' stock correlation matrix
#'
#' @param betas "data.frame" with factor betas
#' @param implied_fct_rtns "data.frame" with implied factor returns
#' @param factor_correlation_matrix "data.frame" with factor correlation matrix
#' @param risk_model_date "Date" with risk model date
#' @return \code{stock_corr} "matrix" with stock correlation
stock_correlation <- function(betas,implied_fct_rtns,factor_correlation_matrix,risk_model_date){
  instruments <- betas[betas$Date==risk_model_date,'Instrument']
  ir <- implied_fct_rtns[implied_fct_rtns$Date==risk_model_date,setdiff(colnames(implied_fct_rtns),'Date')]
  beta_matrix <- betas[betas$Date==risk_model_date,setdiff(colnames(betas),c('Instrument','Date'))]
  beta_matrix <- data.frame(mapply('*',beta_matrix,ir))
  beta_matrix <- as.matrix(beta_matrix)
  stock_corr <- beta_matrix%*%factor_correlation_matrix%*%t(beta_matrix)
  rownames(stock_corr) <- instruments
  colnames(stock_corr) <- instruments
  return(stock_corr)
}

#' Factor covariance from correlation
#'
#' Compute the factor covariance matrix by scaling the correlaiton matrix by the factor
#' returns standard deviation
#'
#' @param correlation "data.frame" with factor correlation
#' @param factor_stdev "data.frame" with factor's standard deviation
#' @return \code{correlation} "matrix" with factor correlation
#' @export
factor_covariance <- function(correlation,factor_stdev){
  vrnce <- as.matrix(factor_stdev)%*%t(as.matrix(factor_stdev))
  if(!nrow(vrnce)>1)vrnce <- t(as.matrix(factor_stdev))%*%as.matrix(factor_stdev)
  return(vrnce*as.matrix(correlation))
}


#' Compute portfolio variance decomposition
#'
#' This computes the variance of the log returns. This is not identical to the variance of the
#' returns. However we take the variance of the log returns as equal to the variance of the
#' returns beacuse the Taylor expansion of the variance of the log returns gives var[log(R)] ~ var[R]/mean[R]^2
#' and since the mean return will generally be close to one, this is a reasonable approximation.
#' If the mean return is above 1, then the risk measure will slightly underestimate risk, whereas if
#' mean return is lower than 1, risks will be slightly over estimated.
#'
#' @param weight "data.frame" with portfolio constituents weights
#' @param betas "data.frame" with portfolio instrument's betas
#' @param factor_covariance "matrix" with factor covariance
#' @param columns "character" list of factors to account when computing decomposition.
#' Default is NULL. If missing or NULL all factors will be used
#'
#' @export
pfo_variance_decomposition <- function(weight,betas,factor_covariance,columns=NULL){
  wtbt <- merge(betas,weight,by='Instrument')
  weight_matrix <- as.matrix(wtbt['Weight'])
  if(length(columns)>0){
    beta_matrix <- as.matrix(wtbt[intersect(setdiff(colnames(wtbt),c('Instrument','Weight')),columns)])
    fct <- factor_covariance[columns,columns]
  }
  else{
    beta_matrix <- as.matrix(wtbt[setdiff(colnames(wtbt),c('Instrument','Weight'))])
    fct <- factor_covariance
  }
  market_risk <- t(weight_matrix)%*%beta_matrix%*%as.matrix(fct)%*%t(beta_matrix)%*%weight_matrix
  return(market_risk)
}



