sourceTo("../lib/referencedata/referenceobject.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
  f <- function(...) order(...,decreasing=decreasing)
  i <- do.call(f,x[by])
  x[i,,drop=FALSE]
}


########################################################################
#
# pass_thru_computation
#
########################################################################
pass_thru_computation <- function(input){
  return(input)
}


########################################################################
#
# row_mean_computation
#
########################################################################
row_mean_computation <- function(input){
  return(cbind(input,data.frame(RowMean = rowMeans(input))))
}

########################################################################
#
# flat_dates
#
########################################################################
flat_dates <- function(history_data){
  
  required_colnms <- c('InstrumentID','Date','Weight')
  
  if(!has_required_columns(history_data, required_colnms)) {
    stop(paste("flat_dates() requires following columns :", required_colnms))
    
  }
  
  history_data <- sort(history_data, by = "Date")
  instruments <- unique(history_data$Instrument)
  
  first <- TRUE
  for(ins in instruments){
    ins_data <- unique(history_data[history_data$Instrument==ins,c('Strategy','Date','Weight')])
    
    if(nrow(ins_data)>1){
      ins_data <- unique(ins_data[c('Date','Weight')])
      flat <- ins_data[c(FALSE,(
                                  (ins_data$Weight[2:nrow(ins_data)]==0|is.na(ins_data$Weight[2:nrow(ins_data)]))
                                  & (!is.na(ins_data$Weight[1:(nrow(ins_data)-1)])&ins_data$Weight[1:(nrow(ins_data)-1)]!=0)
                                )
                          ),
                        ,]
      fd <- flat[,'Date']
    } else {
      fd <- ins_data$Date 
    }
    if(length(fd)==0)fd <- NA
    if(first){
      flat_dates <- data.frame(InstrumentID=ins,FlatDate=as.Date(fd))
      first <- FALSE
    } 
    else{
      fdf <- data.frame(InstrumentID=ins,FlatDate=as.Date(fd))
      flat_dates <- rbind(flat_dates,fdf)
    }
  
  }
  return(flat_dates)

}

########################################################################
#
# days_since_last_flat
#
########################################################################
days_since_last_flat <- function(history_data){
  
  required_colnms <- c('InstrumentID','Date','Weight')
  if(!has_required_columns(history_data, required_colnms)) {
    stop(paste("days_since_last flat requires following columns :", required_colnms))
  }
  dslf <- data.frame(DaysSinceLastFlat = rep(NA, nrow(history_data)))
    
  fd <- flat_dates(history_data)
  
  instruments <- unique(fd$Instrument[!is.na(fd$FlatDate)])
  
  for(ins in instruments){
  
    ins_fd <- fd[fd$InstrumentID == ins, "FlatDate"]
    ins_idx <- (history_data$InstrumentID == ins)
    ins_data <- history_data[ins_idx,]
    
    ins_dslf <- sapply(ins_data$Date, function(x){ifelse(length(ins_fd[ins_fd<=x]) == 0, 
                                                         NA_integer_,  
                                                         as.integer(x -max(ins_fd[ins_fd<=x]))[[1]])
                                                  })
    
    dslf$DaysSinceLastFlat[ins_idx] <- ins_dslf
  }
  
  if (nrow(history_data) != nrow(dslf)) {
    message("Computed days_since_last_flat produced different number of rows that original data")
    stop("Failure when computing days_since_last_flat")
  }
  
  history_data <- cbind(history_data, dslf)
  
  return(history_data)
  
}