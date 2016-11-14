# library(moments)

event_prox <- function(compute_object,event){
  #expect input slot to contain a data.frame
  #with columns: dtDateTime, rDaysSinceLast<Event>, rDaysToNext<Event>
  #for each of the transactions in the leg
  #populates output with a data.frame with a row
  #for each transaction date and the log quotient to days
  #since last <Event> and days to next <Event>.
  cnames <- c(paste("rDaysSinceLast",event,sep=""),paste("rDaysToNext",event,sep=""))
  input <- compute_object@input
  ep <- input[cnames[2]]/unlist(Map(rm_zero_null,input[cnames[1]]))

  output <- data.frame(dtDateTime=input$dtDateTime,DistanceToEvent=ep)
  colnames(output) <- c("dtDateTime",paste("DistanceTo",event,sep=""))

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
      message(sprintf("Error in event_prox() when computing %s : %s",
                      class(compute_object),
                      cond))
  })


  return(compute_object)
}

earn_prox <- function(compute_object){
  return(event_prox(compute_object,"Results"))
}

eps_rev <-  function(compute_object){
  #expect input slot to contain a data.frame
  #with columns: dtDateTime, rNumberOfEPSDowngradesSinceNumbersFY1, rNumberOfEPSUpgradesSinceNumbersFY1
  #for each of the transactions in the leg
  #populates output with a data.frame with a row
  #for each transaction date and the log quotient of
  #EPS upgrades to downgrades for that row
  input <- compute_object@input
  eps_rev <- input$rNumberOfEPSUpgradesSinceNumbersFY1/unlist(Map(rm_zero_null,input$rNumberOfEPSDowngradesSinceNumbersFY1))
  output <- data.frame(dtDateTime=input$dtDateTime,EPSRevision=eps_rev)

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in eps_rev() when computing %s : %s",
                    class(compute_object),
                    cond))
  })

  return(compute_object)
}

trade_accumulator <-  function(compute_object,dir_in,acc_fn,column,na.rm=TRUE){
  #Accumlator function to compute short term accumulations around a trade
  #the callback acc_fn actually computes the value in each case.

  if(length(compute_object@window)==0)stop("Attempt to accumulate feature with no window property.")
  input <- compute_object@input
  if(dir_in==TRUE){
    limit <- min(input$DateTime)
  }
  else{
    limit <- max(input$DateTime)
  }


  cml_in_out <- c()


  for(trade in compute_object@dates){
    if(dir_in==TRUE){
      prc    <- input[input$DateTime<trade,]
      wdow   <- max(limit,trade-compute_object@window)
      prc_win<- prc[prc$DateTime>=wdow,c(column)]

    }
    else{
      prc    <- input[input$DateTime>=trade,]
      wdow   <- min(limit,trade+compute_object@window)
      prc_win<- prc[prc$DateTime<=wdow,c(column)]

    }
    if(length(prc_win) == 1){
      #Multiplicative quantities such as compound averages will require
      #zeros to be removed.
      if(na.rm==TRUE)prc_win <- unlist(Map(rm_zero_null,prc_win))
      cml_in_out <- c(cml_in_out,acc_fn(prc_win))
    }
    else if(length(prc_win) > 1){
      #Multiplicative quantities such as compound averages will require
      #zeros to be removed.
      if(na.rm==TRUE)prc_win <- unlist(Map(rm_zero_null,prc_win))
      cml_in_out <- c(cml_in_out,acc_fn(prc_win))
    }
    else{
      cml_in_out <- c(cml_in_out,NA)
    }
  }


  output <- tryCatch({

      unique(data.frame(dtDateTime=compute_object@dates,WindowValue=unlist(Map(rm_inf,cml_in_out))))
      }, error = function(cond){
        message(paste("Failed to set accumulated value near trade:",cond))
        unique(data.frame(dtDateTime=compute_object@dates,WindowValue = NA))
  })

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in trade_accumulator() when computing %s : %s",
                    class(compute_object),
                    cond))
  })
  return(compute_object)
}

#Place a user defined token next to each listed date to permit features that
#mark specific trade dates
date_marker <- function(compute_object,on_dates,tokens){
  if(length(on_dates)!=length(tokens))stop("Must have equal number of dates and tokens to hightlight dates.")
  input <- compute_object@input
  output <- data.frame(dtDateTime=compute_object@dates,Token=NA)
  for(i in 1:length(on_dates)){
    output[output$dtDateTime==on_dates[i],'Token'] <- tokens[i]
  }

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in date_marker() when computing %s : %s",
                    class(compute_object),
                    cond))
  })

  return(compute_object)
}

open_close <- function(compute_object){
  dates  <- unique(c(min(compute_object@dates,na.rm=TRUE),max(compute_object@dates,na.rm=TRUE)))
  tokens <- c('Open','Close')[1:length(dates)]
  return(date_marker(compute_object,dates,tokens))
}

rtn_accumulator <- function(prc_win){prod(prc_win[2:(length(prc_win))]/prc_win[1:(length(prc_win)-1)])}

rtn_in <- function(compute_object){
  co <- trade_accumulator(compute_object,TRUE,rtn_accumulator,'ClosePrice')

  output <- getFeatureComputationOutput(co)

  output$WindowValue <- (output$WindowValue - 1)*10000

  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in rtn_in() when computing %s : %s",
                    class(co),
                    cond))
  })

  return(co)
}

rtn_out <- function(compute_object){
  co <- trade_accumulator(compute_object,FALSE,rtn_accumulator,'ClosePrice')

  output <- getFeatureComputationOutput(co)

  output$WindowValue <- (output$WindowValue - 1)*10000

  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in rtn_out() when computing %s : %s",
                    class(co),
                    cond))
  })
  return(co)
}

pnl_accumulator <- function(pnl_win){sum(pnl_win[1:length(pnl_win)],na.rm=TRUE)}

pnl_in <- function(compute_object){
  return(trade_accumulator(compute_object,TRUE,pnl_accumulator,'TodayPL',na.rm=FALSE))
}

pnl_out <- function(compute_object){
  return(trade_accumulator(compute_object,FALSE,pnl_accumulator,'TodayPL',na.rm=FALSE))
}

ptv_pnl_in <- function(compute_object){
  return(trade_accumulator(compute_object,TRUE,function(x)pnl_accumulator(x)>0,'TodayPL',na.rm=FALSE))
}

ptv_pnl_out <- function(compute_object){
  return(trade_accumulator(compute_object,FALSE,function(x)pnl_accumulator(x)>0,'TodayPL',na.rm=FALSE))
}

prc_inf <- function(compute_object){
  input <- compute_object@input
  data  <- data.frame(x=input$ClosePrice,y=input$MarketValue/input$ClosePrice)
  if(nrow(data)>0 && ncol(data)==2){
    output  <- data.frame(DateTime=input$DateTime,Influence=cor(as.matrix(data))[1,2])

    compute_object <- tryCatch({
      .setFeatureComputationOutput(compute_object, output)
    }, error = function(cond){
      message(sprintf("Error in prc_inf() when computing %s : %s",
                      class(compute_object),
                      cond))
    })
  }
  return(compute_object)
}

activity_accumulator <- function(act_win,in_to_trade){
    if(in_to_trade){
      norm <- length(act_win)
    }
    else{
      norm <-1
    }
    sum(diff(abs(act_win))/abs(act_win[norm]),na.rm=TRUE)
}

act_in <- function(compute_object){
  return(trade_accumulator(compute_object,TRUE,function(x)activity_accumulator(x,TRUE),'PSize',na.rm=FALSE))
}

act_out <- function(compute_object){
  return(trade_accumulator(compute_object,FALSE,function(x)activity_accumulator(x,FALSE),'PSize',na.rm=FALSE))
}

p_size <- function(object,trade_dates,instrument,strategy,daily_data){
  object@computation@dates <- trade_dates
  df <- data.frame(PSize=daily_data@data$MarketValue/daily_data@data$ClosePrice)
  if(nrow(df)==0){
    df <- data.frame(PSize=rep(NA,nrow(daily_data@data)))
  }
  data <- cbind(daily_data@data,df)
  object@computation <- setComputationData(object@computation,data)
  return(object)
}

dly_dtr_colm_fn <- function(compute_object,columns,fn){
  res <- apply(compute_object@input[columns],1,fn)
  res <- data.frame(DateTime=compute_object@input$DateTime,Result=res)

  output <- subset(res,res$DateTime %in% compute_object@dates)

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in prc_inf() when computing %s : %s",
                    class(compute_object),
                    cond))
  })

  return(compute_object)
}

mkt_cap <- function(compute_object){
  return(dly_dtr_colm_fn(compute_object,c("ClosePrice","OutstandingShares"),prod))
}

psn_rtn_in <- function(compute_object){
  co <- trade_accumulator(compute_object,TRUE,prod,'Rtn')

  output <- getFeatureComputationOutput(co)

  output$WindowValue <- tryCatch({
    (output$WindowValue - 1)*10000
  }, error = function(cond){
    message(sprintf("Error when computing psn_rtn_out() for object of class %s",
                    class(compute_object)))
  })


  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in psn_rtn_in() when computing %s : %s",
                    class(co),
                    cond))
  })

  return(co)
}

psn_rtn_out <- function(compute_object){
  co <- trade_accumulator(compute_object,FALSE,prod,'Rtn')
  output <- getFeatureComputationOutput(co)

  output$WindowValue <- tryCatch({
    (output$WindowValue - 1)*10000
  }, error = function(cond){
    message(sprintf("Error when computing psn_rtn_out() for object of class %s",
                    class(compute_object)))
  })

  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in psn_rtn_out() when computing %s : %s",
                    class(co),
                    cond))
  })


  return(co)
}

psn_update <- function(object,trade_dates,instrument,strategy,daily_data){
    object@computation@dates <- trade_dates
    if(nrow(daily_data@data)>=2){
      df <- as.numeric(unlist(daily_data@data$TodayPL[2:length(daily_data@data$TodayPL)]/abs(daily_data@data$MarketValue[1:(length(daily_data@data$TodayPL)-1)])))
      df <- c(NA,as.numeric(unlist(Map(rm_inf_zero,df)))+1)
      df <- data.frame(Rtn=df)
    }
    else{
      df <- data.frame(Rtn=NULL)
    }
    if(nrow(df)==0){
      df <- data.frame(Rtn=rep(NA,nrow(daily_data@data)))
    }
    data <- cbind(daily_data@data,df)
    return(data)
}

age_accumulator <- function(act_win){sum(abs(act_win)>0,na.rm=TRUE)}

age <- function(compute_object){
  return(trade_accumulator(compute_object,FALSE,age_accumulator,'MarketValue',na.rm=FALSE))
}

rtn_vol <- function(prc_win){sd(prc_win[2:(length(prc_win))]/prc_win[1:(length(prc_win)-1)],na.rm=TRUE)}

vol_in <- function(compute_object){
  co <- trade_accumulator(compute_object,TRUE,rtn_vol,'ClosePrice')

  output <- getFeatureComputationOutput(co)

  output$WindowValue <- output$WindowValue*10000

  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in vol_in() when computing %s : %s",
                    class(co),
                    cond))
  })

  return(co)
}

vol_out <- function(compute_object){
  co <- trade_accumulator(compute_object,FALSE,rtn_vol,'ClosePrice')

  output <- getFeatureComputationOutput(co)

  output$WindowValue <- output$WindowValue*10000

  co <- tryCatch({
    .setFeatureComputationOutput(co, output)
  }, error = function(cond){
    message(sprintf("Error in vol_in() when computing %s : %s",
                    class(co),
                    cond))
  })

  return(co)
}

rtn_skw <- function(prc_win){skewness(prc_win[2:(length(prc_win))]/prc_win[1:(length(prc_win)-1)],na.rm=TRUE)}

skew_in <- function(compute_object){
  return(trade_accumulator(compute_object,TRUE,rtn_skw,'ClosePrice',na.rm=FALSE))
}

skew_out <- function(compute_object){
  return(trade_accumulator(compute_object,FALSE,rtn_skw,'ClosePrice',na.rm=FALSE))
}

mavg_price <- function(compute_object){
  return(trade_accumulator(compute_object,TRUE,function(x)mean(x,na.rm=TRUE),'ClosePrice',na.rm=FALSE))
}

trade_comparison <- function(compute_object,column,fn,prev_trade=TRUE){

  rval <- NULL

  dates <- unique(compute_object@dates)

  if(nrow(compute_object@input)>1){
    for(d in 1:length(dates)){
      dex <- which(compute_object@input$DateTime == dates[d],TRUE)

      rw <- data.frame(DateTime=compute_object@dates[d],Comparison = NA)
      if (length(dex > 0)){
        if(prev_trade && dex > 0){
          rw <- tryCatch({
            data.frame(DateTime=compute_object@dates[d],Comparison=fn(compute_object@input[dex-1,column],compute_object@input[dex,column]))
          }, error = function(cond){
            message(sprintf("error when computing trade_comparison for object of class %s", class(compute_object)))
          })
        } else if (!prev_trade && (dex + 1) <= nrow(compute_object@input) )
        {
          rw <- data.frame(DateTime=compute_object@dates[d],Comparison=fn(compute_object@input[dex,column],compute_object@input[dex+1,column]))
        } else {
          rw <- data.frame(DateTime=compute_object@dates[d],Comparison = NA)
        }
      }
      if(d==1){
        rval <- rw
      }
      else{
        rval <- rbind(rval,rw)
      }
    }
  }

  output <- rval

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in trade_comparison() when computing %s : %s",
                    class(compute_object),
                    cond))
  })

  return(compute_object)
}

off_side <- function(compute_object){
  return(trade_comparison(compute_object,'ClosePrice',function(x,y){y<x}))
}

is_new <- function(x,y){
  if(length(x)==0)x <- NA
  if(length(y)==0)y <- NA
  if(!is.na(x))if(x==0)x <- NA
  if(!is.na(y))if(y==0)y <- NA
  if((!is.na(x))&&(!is.na(y))){
    rval <- FALSE
  }
  else if ((is.na(x))&&(!is.na(y))){
    rval <- TRUE
  }
  else if ((!is.na(x))&&(is.na(y))){
    rval <- FALSE
  }
  else{
    rval <- NA
  }
  return(rval)
}

is_close <- function(x,y){
  return(is_new(y,x))
}

new_psn <- function(compute_object){
 return(trade_comparison(compute_object,'MarketValue',is_new))
}

close_psn <- function(compute_object){
 return(trade_comparison(compute_object,'MarketValue',is_close))
}

pl_hit <- function(compute_object){
  input <- merge(compute_object@input,data.frame(DateTime=compute_object@dates),by='DateTime')

  output <- tryCatch({
    data.frame(DateTime=input$DateTime,Hit1D=input$TodayPL>0)
  }, error = function(cond){
    browser
    message(sprintf("Error in pl_hit() when computing %s : %s",
                    class(compute_object),
                    cond))
  })

  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, output)
  }, error = function(cond){
    message(sprintf("Error in pl_hit() when computing %s : %s",
                    class(compute_object),
                    cond))
  })


  return(compute_object)
}
