library(fmsb)
data_path <- "c:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/dk_report_data.rds"
test_data <- readRDS(data_path)

pcnt2num <- function(value){
  nc <- nchar(value)
  return(as.numeric(substr(value,1,nc-1)))
}

trans_dataframe <- function(data_frame){
  mtrx <- t(data_frame)
  df <- as.data.frame(mtrx)
  colnames(df) <- as.character(as.matrix(df[1,]))
  df <- df[2:nrow(df),]
  return(df)
}

build_data <- function(test_data,idx,tcol,pct=TRUE,min=0,max=100){
  cnt <- 1
  for(c in idx){
    
    d <- test_data[[c]]
    if(c<length(test_data)){
      d[,2] <- apply(d,1,function(x,i=c)paste(names(test_data[i]),x[1],x[2],collapse=""))
      d <- d[,c(2,tcol)]
    }
    else{
      d <- d[,c(1,tcol-1)]
    }
    
    d <- trans_dataframe(d)
    for(c in 1:ncol(d)){
      if(pct){
        d[,c] <- pcnt2num(as.matrix(d[,c]))  
      }
      else{
        d[,c] <- as.numeric(as.matrix(d[,c]))  
      }
    }
    
    if(cnt == 1){
      all_d <- d
    }
    else{
      all_d <- cbind(all_d,d)  
    }
    cnt <- cnt+1
    
  }
  all_d <- rbind(min,all_d)
  all_d <- rbind(max,all_d)
  return(all_d)
}

#idx <- (1:5)*2
idx <- c(4)

#Process 1m Trades
trades_1m <- build_data(test_data,idx,3,pct=FALSE,min=0,max=150)

#Process 3m Trades
trades_3m <- build_data(test_data,idx,4,pct=FALSE,min=0,max=150)

#Process 3m Trades

#Process 1m Hit rate
all_d_1m <- build_data(test_data,idx,5)

#Process 3m Hit rate
all_d_3m <- build_data(test_data,idx,6)

#process 1m Win/loss 
wl_1m <- build_data(test_data,idx,7,pct=FALSE,min=0,max=2.5)

#process 3m Win/loss 
wl_3m <- build_data(test_data,idx,8,pct=FALSE,min=0,max=2.5)

#par(mfrow=c(2,2))
par(new=F)
radarchart(trades_1m,vlcex=0.8,pcol=2)
par(new=T)
radarchart(trades_3m,vlcex=0.8,pcol=1)

par(new=F)
radarchart(all_d_1m,vlcex=0.8,pcol=2)
par(new=T)
radarchart(all_d_3m,vlcex=0.8,pcol=1)

par(new=F)
radarchart(wl_1m,vlcex=0.8,pcol=2)
par(new=T)
radarchart(wl_3m,vlcex=0.8,pcol=1)

