#' @include referencedata.r
NULL

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

########################################################################
#
# compound a timeseries of excess return
#
########################################################################
tseries_excess_compound <- function(input){
  required_colnms <- c('Date')
  if(!has_required_columns(input, required_colnms)) {
    stop(paste("tseries_compound requires following columns :", required_colnms))
  }
  input <- input[order(input$Date),]
  compound_on <- setdiff(colnames(input),'Date')
  for(col in compound_on){
    input[[col]] <- input[[col]] + 1
    input[[col]][is.na(input[[col]])] <- 1
    crtn <- exp(cumsum(log(input[[col]])))
    input[[paste(col,'_cmpnd',sep="")]] <- crtn
  }

  return(input)

}

########################################################################
#
# compute moving averages of a timeseries
#
########################################################################
tseries_mavgs <- function(input,mavgs=c(20,50)){

  required_colnms <- c('Date')
  if(!has_required_columns(input, required_colnms)) {
    stop(paste("tseries_mavgs requires following columns :", required_colnms))
  }
  input <- input[order(input$Date),]
  n_rows <- nrow(input)

  mavg_on <- setdiff(colnames(input),'Date')
  for(col in mavg_on){
    for(m in mavgs){

      if (m <= n_rows) {
        mavg <- rollapplyr(input[[col]],
                          width=m,
                          FUN=function(x)mean(x,na.rm=TRUE),
                          partial = FALSE,
                          fill = NA)
      }
      else {
        mavg <- rep(NA_real_, n_rows)
      }

      input[[paste(col,'_',m,'_mavg',sep="")]] <- mavg
    }
  }
  return(input)
}

########################################################################
#
# compute spread between two moving averages
#
########################################################################
tseries_mavgs_spread <- function(input,mavgs=c(20,50)){

  required_colnms <- c('Date')
  if(!has_required_columns(input, required_colnms)) {
    stop(paste("tseries_mavgs requires following columns :", required_colnms))
  }
  if(length(mavgs)!=2)stop("Can only compute mavg spread between two moving averages.")
  mavgs <- sort(mavgs)

  input <- input[order(input$Date),]
  cn <- colnames(input)
  mavg_cols <- cn[grep('mavg',cn)]
  #base_cols <- setdiff(cn,c(mavg_cols,'Date'))
  base_cols <- unique(sub("_[0-9]+_mavg$", "",mavg_cols, perl = TRUE))
  for(col in base_cols){
    input[[paste(col,'_mavg_sprd',sep="")]] <- input[[paste(col,'_',mavgs[1],'_mavg',sep='')]]-input[[paste(col,'_',mavgs[2],'_mavg',sep='')]]
    input[[paste(col,'_mavg_sprd_df',sep="")]] <- c(NA,diff(input[[paste(col,'_mavg_sprd',sep="")]]))
  }
  return(input)
}

########################################################################
#
# fcompute regression trend indicator
#
########################################################################
rsquared_trend_ind <- function(input, lookback  = 50){

  required_colnms <- c('Date')
  if(!has_required_columns(input, required_colnms)) {
    stop(paste("rsquared_trend_ind requires following columns :", required_colnms))
  }

  kernel <- function(x){
    c <- seq(length(x));
    lm.1 <- lm(x~c);
    ret <- c(a=as.numeric(lm.1$coefficients[2]),
                b = as.numeric(lm.1$coefficients[1]),
                rsq = summary(lm.1)$r.squared)
    return(ret)
  }

  input <- input[order(input$Date),]

  mavg_on <- setdiff(colnames(input),'Date')
  for(col in mavg_on){

    n_rows <- nrow(input)

    if (lookback <= n_rows) {
      mavg <- rollapplyr(input[[col]],
                           width=lookback,
                           FUN=kernel,
                           partial = TRUE)
      mavg <- as.data.frame(mavg)
      mavg[seq(min(n_rows, lookback) -1),] <- NA_real_
    }
    else {


      mavg$a <- rep(NA_real_, n_rows)
      mavg$b <- rep(NA_real_, n_rows)
      mavg$rsq <- rep(NA_real_, n_rows)
    }


    mavg$ti <- "FLAT"
    mavg$ti[mavg$rsq > 0.50 & mavg$a > 0] <- "UP"
    mavg$ti[mavg$rsq > 0.50 & mavg$a < 0] <- "DOWN"
    input[[paste(sub("_cmpnd", "", col),'_ti',sep="")]] <- factor(mavg$ti,
                                                                  levels = c("FLAT", "DOWN", "UP"))


  }
  return(input)


}
########################################################################
#
# compute quantiles of timeseries
#
########################################################################
ftiler <- function(data,ntiles=4){

  required_colnms <- c('Date')
  if(!has_required_columns(data, required_colnms)) {
    stop(paste("tseries_mavgs requires following columns :", required_colnms))
  }
  ntile_on <- setdiff(colnames(data),"Date")
  for(on in ntile_on){
    data[[paste(on,'_ftile',sep='')]] <- with(data[on],
                                              cut(as.numeric(unlist(data[on])),
                                                  breaks=unique(quantile(data[on],
                                                                         probs=seq(0,1, by=1/ntiles),
                                                                         na.rm=TRUE)),
                                                  labels=FALSE,
                                                  include.lowest=TRUE))
  }
  return(data)
}

########################################################################
#
# encode factor states
#
########################################################################
factor_space_encoder <- function(data,state_cols = c("ti", "ftile")){

  required_colnms <- c('Date')
  if(!has_required_columns(data, required_colnms)) {
    stop(paste("tseries_mavgs requires following columns :", required_colnms))
  }
  ntile_on <- setdiff(colnames(data),"Date")
  for (col in state_cols){
    ntile_on <- sub(paste0("_", col), "", ntile_on)
  }

  ntile_on <- unique(ntile_on)

  sub_space_vectors <- list()

  # generate factor space
  for (state in state_cols){
    sub_space_vectors[[state]] <- unique(unlist(data[,paste(ntile_on, state, sep="_")]))
  }

  space_states <- expand.grid(sub_space_vectors)

  space_states <- sort(apply(space_states, 1, function(x){paste(x, collapse="_")}))


  for(on in ntile_on){

    factor_substate_cols <- paste(on, state_cols, sep="_")

    states <- apply(data[factor_substate_cols], 1, function(x){paste(x, collapse="_")})

    states <- factor(states, levels = space_states)

    data[[paste(on,'_state',sep='')]] <- states
  }

  return(data)
}
