library(hashFunction)
library(zoo)

al <- list(Sold...Bought.Date="Sold...Bought.Date",Sold.Date="Sold...Bought.Date",Col5="Sold...Bought.Date")
column_name_aliases  <- function(df,alias_list=al){
  nmes <- colnames(df)
  sub <- nmes[nmes%in%names(alias_list)]
  colnames(df)[nmes%in%sub] <- unlist(alias_list[sub])
  return(df)
}


extract_from_workbook <- function(wb,month,data_tables,data,first,dc){
  all_sheets <- getSheets(wb)
  data_sheets <- all_sheets[grep(month,all_sheets)]
  for(sh in data_sheets){
    t<-readWorksheet(wb,sheet=sh)  
    st_date <- readWorksheet(wb,sheet=sh,startRow=3,endRow=3,startCol = 2,endCol = 2,header=FALSE)
    en_date <- readWorksheet(wb,sheet=sh,startRow=4,endRow=4,startCol = 2,endCol = 2,header=FALSE)
    for(extract in names(data_tables)){
      params <- data_tables[[extract]]
      #str <- which(tolower(t[,params[['start_header_col']]])==tolower(params[['start_header']]))
      #enr <- which(tolower(t[,params[['end_header_col']]])==tolower(params[['end_header']]))
      str <- min(grep(tolower(params[['start_header']]),tolower(t[,params[['start_header_col']]])))
      enr <- min(grep(tolower(params[['end_header']]),tolower(t[,params[['end_header_col']]])))
      if(extract=='Hedges'&is.infinite(str)){
        str <- (enr+params[['end_offset']])-params[['start_offset']]
      } else if(length(str)==0||length(enr)==0||is.infinite(str)||is.infinite(enr)){
        stop("Read error")
      }
      df <- readWorksheet(wb,sheet=sh,startRow=(str+params[['start_offset']]),endRow=(enr+params[['end_offset']]),startCol = params[['start_col']],endCol = params[['end_col']],header=TRUE,forceConversion = TRUE)
      df <- column_name_aliases(df)
      df <- df[!unlist(Map(function(z)Reduce(function(x,y)x&&y,is.na(df[z,])||(nchar(gsub(' ','',df[z,]))==0)),1:nrow(df))),]
      message(paste("Sheet:",sh,"extract:",extract))
      if(length(nrow(df))>0)
      { 
        if(nrow(df)>0){
          df$StartDate <- st_date[[1]]
          df$EndDate <- en_date[[1]]
          if(first[[extract]]){
            data[[extract]] <- df 
            first[[extract]] <- FALSE
          } else{
            bdf <- tryCatch({rbind.fill(data[[extract]],df)},error=function(cond){message(paste("Bind failed:",cond))
              message(paste(capture.output(df),"\n",sep=""))})
            if(length(bdf)>0)data[[extract]] <- bdf
          }      
        } else {
          message("No rows!")
        } 
      }
    }
  }
  return(list(data=data,state=first))
}

dir_findr <- function(z){
  dex <- grepl('[0-9].',z)&(nchar(z)==6)
  return(z[dex])
}

subset_positions <- function(data){
  psn_data <- data[['Position']]
  psn_data <- psn_data[!is.na(psn_data$Ticker),]
  psn_data <- psn_data[c('Ticker','Company.Name','Position','Col4','Net.Position...of.NAV','Geography','Sector','Currency','Current.Price...local.','Current.Value...local.','Current.Price..US..','Current.Value...US..','EndDate')]
  names(psn_data) <- c('Instrument','Name','Position','Type','PctNAV','Country','Sector','Currency','ClosePriceLocal','MarketValueLocal','ClosePrice','MarketValue','TradeDate')
  psn_data$Trader <- 'CM'
  psn_data$Strategy <- 'Aperios'
  hdg <- data[['Hedges']]
  hdg <- hdg[c('Short.Hedges','Col2','Col3','Col6','Col8','Col17','EndDate')]
  colnames(hdg) <- c('Instrument','Name','Position','Country','Currency','MarketValue','TradeDate')
  hdg$Trader <- 'CM'
  hdg$Strategy <- 'HEDGE'
  psn_data <- rbind.fill(psn_data,hdg)
  psn_data$PsnLong <- psn_data$MarketValue >= 0
  return(psn_data)
}

subset_trades <- function(data){
  buy_data <- data[['Buys']]
  buy_data <- buy_data[!is.na(buy_data$Bought.Positions),]
  buy_data <- buy_data[c('Bought.Positions','Col3','Col4','Sold...Bought.Date','Col8','Col9','Col12','Col14','Col16')]
  colnames(buy_data) <- c('Instrument','TradeShares','Type','TradeDate','Currency','MidOnEntryLocal','ValueLocal','MidOnEntry','ValueUSD')
  buy_data$Long <- 1
  sell_data <- data[['Sells']]
  sell_data <- sell_data[!is.na(sell_data$Sold.Positions),]
  sell_data <- sell_data[c('Sold.Positions','Col3','Col4','Sold...Bought.Date','Col8','Col9','Col12','Col14','Col16')]
  colnames(sell_data) <- c('Instrument','TradeShares','Type','TradeDate','Currency','MidOnEntryLocal','ValueLocal','MidOnEntry','ValueUSD')
  sell_data$Long <- 0
  all_trades <- rbind(buy_data,sell_data)
  all_trades <- unique(all_trades)
  all_trades$TradeID <- apply(all_trades[c('Instrument','TradeDate','ValueUSD','Long')],1,function(x)murmur3.32(paste(x[[1]],as.character(x[[2]]),x[[3]],x[[4]],sep="")))
  return(all_trades)
}

#not needed since position not static as I had thought
build_market_value <- function(df){
  mnths <- unique(substr(df$TradeDate,1,7))
  first <- TRUE
  for(ins in unique(df$Instrument)){
    for(month in mnths){
      sd <- df[grepl(month,df$TradeDate)&df$Instrument==ins,]
      sd <- sd[!unlist(Map(function(z)Reduce(function(x,y)x&&y,is.na(sd[z,])||(nchar(gsub(' ','',sd[z,]))==0)),1:nrow(sd))),]
      sd <- sd[order(sd$TradeDate),]
      signed_values <- ifelse(sd$PsnLong,((-1)^(1+sd$Long)),-1*((-1)^(1+sd$Long)))*sd$ValueUSD
      signed_shares <- ifelse(sd$PsnLong,((-1)^(1+sd$Long)),-1*((-1)^(1+sd$Long)))*sd$TradeShares
      signed_values[is.na(signed_values)] <- 0
      signed_shares[is.na(signed_shares)] <- 0
      sd$Position <- sd$Position + cumsum(signed_shares) - signed_shares[1]
      sd$MarketValue <- sd$MarketValue +cumsum(signed_values) - signed_values[1]
      if(first){
        output <- sd
        first <- FALSE
      } else {
        output <- rbind(output,sd)
      }
    }  
  }
  return(output)
}

compute_daily_pl <- function(psn_trade_data){
  first <- TRUE
  for(ins in unique(psn_trade_data$Instrument)){
    sd <- psn_trade_data[psn_trade_data$Instrument==ins,]
    sd <- sd[order(sd$TradeDate),]
    signed_values <- unlist(Map(function(x)ifelse(x==1,((-1)^(1+x)),-1*((-1)^(1+x))),sd$Long))*sd$ValueUSD
    signed_values[is.na(signed_values)] <- 0
    sd$TodayPL <- c(NA,diff(sd$MarketValue)) - signed_values
    if(first){
      output <- sd
      first <- FALSE
    } else{
      output <- rbind(output,sd)
    }
  }
  return(output)
}

trade_history <- function(psn_data,trd_data){
  th <- merge(psn_data,trd_data,by=c('Instrument','Type','TradeDate','Currency'),all.x=TRUE)
  th$Position <- as.numeric(th$Position)
  th$MarketValue <- as.numeric(th$MarketValue)
  #message("Building position history...")
  #th <- build_market_value(th)
  message("Computing daily PL...")
  th <- compute_daily_pl(th)
  return(th)
}

compute_rolling_fn <- function(data,new_col,applied_to,lookback,fn){
   window_vals <- rollapply(data[[applied_to]],lookback,fn,na.pad=TRUE,by.column=FALSE)
   data <- cbind(data,x=window_vals)
   colnames(data)[colnames(data)=='x'] <- new_col
   return(data)
}

get_commodities <- function(start,end){
  getSymbols('EEM') 
  oil <- as.data.frame(EEM)
  oil <- cbind(Date=as.Date(rownames(oil)),oil)
  colnames(oil) <- c('Date','EEM')
  oil <- oil[oil$Date>=as.Date(start)&oil$Date<=as.Date(end),]
  return(oil)
}

get_commodity_returns <- function(start,end){
  rtn <- get_commodities(as.Date(start)-1,end)
  rtn$EEM <- c(NA,(rtn[2:nrow(rtn),'EEM']/rtn[1:(nrow(rtn)-1),'EEM'])-1)
  rtn <- rtn[c('Date','EEM')]
  rtn <- rtn[rtn$Date>(as.Date(start)-1),]
  return(rtn)
}

