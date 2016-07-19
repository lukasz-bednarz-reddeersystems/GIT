sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

clean_rtn <- function(data,cols){
  dexs <- rep(TRUE,nrow(data))
  for(c in cols){
    dexs <- dexs&(!is.na(data[[c]])&!is.infinite(data[[c]])&!is.nan(data[[c]])&data[[c]]!=0)
  }
  return(data[dexs,])
}

weight_helper <- function(long){
  out <- long
  out[!is.na(out)] <- ((-1)^(1+out[!is.na(out)]))
  return(out)
}

core_activity <- function(position_history,tau,new_only=TRUE){
  if(new_only){
    #Remove positions that are from previous periods or where the 
    #age could not be computed
    position_history <- position_history[!is.na(position_history$PsnAge),]
    position_history <- position_history[position_history$PsnAge>-1,]
  }
  position_history$ValueUSD[is.na(position_history$ValueUSD)] <- 0
  instruments <- unique(position_history$Instrument[!is.na(position_history$Instrument)])
  position_history$Indicator <- !is.na(position_history$TradeID)
  first <- TRUE
  for(ins in instruments){
    ph <- position_history[position_history$Instrument==ins,]
    ph$ValueUSD <-  abs(ph$ValueUSD)
    #If multiple trades in the same strategy on one day, only the Value traded should add
    #Take the direction of the trade as the value weighted direction
    ph$DirValue <- ph$ValueUSD*weight_helper(ph$Long)
    reduce_trades <- aggregate(ph[c('DirValue','ValueUSD','Indicator')],list(TradeDate=ph$TradeDate,Strategy=ph$Strategy),function(x)sum(x,na.rm=TRUE))
    reduce_trades$Long <- reduce_trades$DirValue>=0
    reduce_trades$Long[reduce_trades$ValueUSD==0] <- NA
    reduce_trades <- reduce_trades[c('TradeDate','Strategy','ValueUSD','Indicator','Long')]
    colnames(reduce_trades) <- c('TradeDate','Strategy','ValueUSD','NTrades','Long')
    #Now if the same instrument traded in more than one strategies, the Market values and PL should
    #be added
    reduce_strats <- merge(ph[c('Strategy','Instrument','TradeDate','TodayPL','MarketValue')],reduce_trades,by=c('TradeDate','Strategy'))
    reduce_strats$DirValue <- reduce_strats$ValueUSD*weight_helper(reduce_strats$Long)
    reduce_strats <- aggregate(reduce_strats[c('DirValue','ValueUSD','NTrades','TodayPL','MarketValue')],list(TradeDate=reduce_strats$TradeDate),function(x)sum(x,na.rm=TRUE))
    reduce_strats$Long <- reduce_strats$DirValue>=0
    reduce_strats$Long[reduce_strats$ValueUSD==0] <- NA
    reduce_strats <- reduce_strats[c('TradeDate','ValueUSD','NTrades','TodayPL','MarketValue','Long')]
    ph <- merge(ph[c('Instrument','TradeDate','Name','PsnAgeCategory','PsnAge','ClosePrice')],reduce_strats,by=c('TradeDate'))
    ph <- ph[order(ph$TradeDate),]
    ph$Weight <- ph$MarketValue/ph$ClosePrice
    long_nas <- is.na(ph$Long)
    ph$Long[long_nas] <- 0
    act <- c((-1)^(ph$Long[1]+1)*ph$ValueUSD[1])
    if(is.na(ph$Weight[1]))ph$Weight[1] <- act
    cw  <- ph$Weight[1]
    if(length(ph$ValueUSD)>1){
      for(i in 1:(length(ph$ValueUSD)-1)){
        act[i+1] <- act[i] + ((-1)^(ph$Long[i+1]+1))*ph$ValueUSD[i+1] - (1/tau)*act[i]
        mvchk <- ifelse(is.na(ph$MarketValue[i+1]),FALSE,ph$MarketValue[i+1]>0)
        if(is.na(ph$Weight[i+1]))ph$Weight[i+1] <- ifelse(mvchk,(ph$Weight[i] + ((-1)^(ph$Long[i+1]+1))*ph$ValueUSD[i+1]),NA)
        cw[i+1]  <- cw[i] - (1/tau)*(cw[i]-ph$Weight[i+1])
      }  
    }
    else{
      act <- ((-1)^(ph$Long[1]+1))*ph$ValueUSD[1]
      cw  <- ph$Weight[1]
    }
    ph$Activity <- act
    ph$CoreWeight <- cw
    ph$ActiveWeight <- ph$Weight - ph$CoreWeight
    ph$Long[long_nas] <- NA
    if(first){
      new_history <- ph  
      first <- FALSE
    }
    else{
      new_history <- rbind(new_history,ph)
    }
  }
  return(new_history)
}

portfolio_decomposition <- function(history_data,new_only=TRUE){
  #NB: Function below computes min dates from the first trade date, not from the
  #start of the position history: MAY CAUSE PROBLEMS
  psn_hstry <- categorise_psn_ages(history_data)
  position_history <- psn_hstry[[1]]
  #min_dates <- psn_hstry[[2]]
  #position_history <- market_rel_pl(position_history)
  
  #activity_frame <- core_activity(position_history,median(unique(position_history$PsnAge),na.rm=TRUE)/10,new_only)
  activity_frame <- core_activity(position_history,as.numeric(median(position_history$PsnAge[position_history$PsnAge >=0 ],na.rm=TRUE)/10),new_only)
  activity_frame$PsnLong <- activity_frame$MarketValue > 0
  activity_frame$BuyHold <- NA
  
  for(ins in unique(activity_frame$Instrument)){
    activity_frame$StockReturn[activity_frame$Instrument==ins] <- c(NA,activity_frame[activity_frame$Instrument==ins,'ClosePrice'][2:sum(activity_frame$Instrument==ins)]/abs(activity_frame[activity_frame$Instrument==ins,'ClosePrice'][1:(sum(activity_frame$Instrument==ins)-1)]))  
    activity_frame$Return[activity_frame$Instrument==ins] <- c(NA,activity_frame[activity_frame$Instrument==ins,'TodayPL'][2:sum(activity_frame$Instrument==ins)]/abs(activity_frame[activity_frame$Instrument==ins,'MarketValue'][1:(sum(activity_frame$Instrument==ins)-1)]))+1  
    activity_frame$BuyHold[activity_frame$Instrument==ins] <- activity_frame[activity_frame$Instrument==ins,]$Weight[!is.na(activity_frame[activity_frame$Instrument==ins,]$Weight)&!is.infinite(activity_frame[activity_frame$Instrument==ins,]$Weight)&!is.nan(activity_frame[activity_frame$Instrument==ins,]$Weight)&abs(activity_frame[activity_frame$Instrument==ins,]$Weight)>0][1]
  }
  normalise <- aggregate(activity_frame[c('Weight','BuyHold')],list(TradeDate=activity_frame$TradeDate,PsnLong=activity_frame$PsnLong),function(x)sum(x,na.rm=TRUE))
  colnames(normalise) <- c('TradeDate','PsnLong','WeightNormalise','BuyHoldNormalise')
  normalise$WeightNormalise <- abs(normalise$WeightNormalise)
  normalise$BuyHoldNormalise <- abs(normalise$BuyHoldNormalise)
  activity_frame <- merge(activity_frame,normalise,by=c('TradeDate','PsnLong'))
  activity_frame$Weight <- activity_frame$Weight/activity_frame$WeightNormalise
  activity_frame$BuyHold <- activity_frame$BuyHold/activity_frame$BuyHoldNormalise
  activity_frame$ActiveWeight <- activity_frame$ActiveWeight/activity_frame$WeightNormalise
  activity_frame$CoreWeight <- activity_frame$CoreWeight/activity_frame$WeightNormalise
  
  activity_frame$Core <- abs(activity_frame$ActiveWeight)<(abs(activity_frame$Weight)*0.05)
  activity_frame$TotalReturn  <- activity_frame$Weight*(activity_frame$Return-1)
  
  activity_frame$CoreReturn   <- activity_frame$CoreWeight*(activity_frame$Return-1)
  activity_frame$TradedReturn <- activity_frame$ActiveWeight*(activity_frame$Return-1)
  
  activity_frame$PassiveReturn<- activity_frame$BuyHold*(activity_frame$Return-1)
  activity_frame$ActiveReturn <- activity_frame$TotalReturn-activity_frame$PassiveReturn
  
  activity_frame$CorePassive<- (activity_frame$CoreWeight/activity_frame$Weight)*(activity_frame$Return-1)*activity_frame$BuyHold
  activity_frame$CoreActive <- activity_frame$CoreWeight*(activity_frame$Return-1)-activity_frame$CorePassive
  activity_frame$TradedPassive<- (activity_frame$ActiveWeight/activity_frame$Weight)*(activity_frame$Return-1)*activity_frame$BuyHold
  activity_frame$TradedActive <- activity_frame$ActiveWeight*(activity_frame$Return-1)-activity_frame$TradedPassive
  
  activity_frame$CorePL   <- (activity_frame$CoreWeight/activity_frame$Weight)*activity_frame$TodayPL
  activity_frame$TradedPL <- (activity_frame$ActiveWeight/activity_frame$Weight)*activity_frame$TodayPL
  
  activity_frame$Exposure <- abs(activity_frame$Weight)*activity_frame$WeightNormalise
  activity_frame$CoreExposure <- abs(activity_frame$CoreWeight)*activity_frame$WeightNormalise
  activity_frame$TradedExposure <- abs(activity_frame$ActiveWeight)*activity_frame$WeightNormalise
  
  activity_frame$PassivePL <- (activity_frame$PassiveReturn/activity_frame$TotalReturn)*activity_frame$TodayPL
  activity_frame$ActivePL <- (activity_frame$ActiveReturn/activity_frame$TotalReturn)*activity_frame$TodayPL
  
  activity_frame$PassiveExposure <- abs(activity_frame$Weight*(activity_frame$PassiveReturn/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  activity_frame$ActiveExposure <- abs(activity_frame$Weight*(activity_frame$ActiveReturn/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  
  activity_frame$CorePassiveExp <- abs(activity_frame$Weight*(activity_frame$CorePassive/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  activity_frame$CoreActiveExp <- abs(activity_frame$Weight*(activity_frame$CoreActive/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  activity_frame$TradedPassiveExp <- abs(activity_frame$Weight*(activity_frame$TradedPassive/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  activity_frame$TradedActiveExp <- abs(activity_frame$Weight*(activity_frame$TradedActive/activity_frame$TotalReturn))*activity_frame$WeightNormalise
  
  activity_frame$CorePassivePL <- (activity_frame$CorePassive/activity_frame$TotalReturn)*activity_frame$TodayPL
  activity_frame$CoreActivePL <- (activity_frame$CoreActive/activity_frame$TotalReturn)*activity_frame$TodayPL
  activity_frame$TradedPassivePL <- (activity_frame$TradedPassive/activity_frame$TotalReturn)*activity_frame$TodayPL
  activity_frame$TradedActivePL <- (activity_frame$TradedActive/activity_frame$TotalReturn)*activity_frame$TodayPL
  
  rm_inf_nan <- function(x){
    x[is.infinite(x)] <- NA
    x[is.nan(x)] <- NA
    return(x)
  }
  activity_frame <- data.frame(Map(rm_inf_nan,activity_frame))

  return(activity_frame)
}

compute_overlaps <- function(pfo){
  instruments <- unique(pfo$Instrument)
  dates <- unique(pfo$TradeDate)
  max_score <- length(instruments)*length(dates)
  total <- 0
  core  <- 0
  active<- 0
  act_cr<- 0
  for(ins in instruments){
    total <- total + sum(pfo$Instrument==ins)
    core  <- core + sum(pfo$Instrument==ins&pfo$Core==TRUE)
    active<- active + sum(pfo$Instrument==ins&pfo$Core==FALSE)
    act_cr<- act_cr + length(intersect(pfo[pfo$Instrument==ins&pfo$Core==FALSE,'Instrument'],pfo[pfo$Instrument==ins&pfo$Core==TRUE,'Instrument']))
  }
  return(data.frame(Total=total/max_score,Core=core/max_score,Active=active/max_score,ActiveCore=act_cr/max_score))
}

compute_return_rank <- function(pfo){
  dates <- unique(pfo$TradeDate)
  inner_dates <- dates
  max_score <- ((length(dates)^2)-length(dates))/2
  total_rho <- 0
  core_rho <-
  active_rho <- 0
  for(dte_outer in dates){
    outer <- pfo[pfo$TradeDate==dte_outer,]
    for(dte_inner in inner_dates){
      if(dte_inner != dte_outer){
        inner <- pfo[pfo$TradeDate==dte_inner,]
        common<- merge(outer,inner,by='Instrument')
        core_common<- merge(outer[outer$Core==TRUE,],inner[inner$Core==TRUE,],by='Instrument')
        actv_common<- merge(outer[outer$Core==FALSE,],inner[inner$Core==FALSE,],by='Instrument')
        total_rho <- total_rho + tryCatch({
                                    cor.test(~ TotalReturn.x + TotalReturn.y,data=common,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                    message(cond)
                                    return(0)
                                  })
        core_rho <- core_rho + tryCatch({
                                    cor.test(~ TotalReturn.x + TotalReturn.y,data=core_common,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                    message(cond)
                                    return(0)
                                  })
        active_rho <- active_rho + tryCatch({
                                    cor.test(~ TotalReturn.x + TotalReturn.y,data=actv_common,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                    message(cond)
                                    return(0)
                                  })
      }
    }
    inner_dates <- inner_dates[inner_dates!=dte_outer]
  }
  return(data.frame(Total=total_rho/max_score,Core=core_rho/max_score,Active=active_rho/max_score))
}

pfo_daily_regression <- function(data_list){
  first <- TRUE
  for(d in data_list){
    if(first){
      all_data <- d
      first <- FALSE
    }
    else{
      all_data <- cbind(all_data,d[,2])
    }
  }
  names(all_data) <- c('TradeDate',names(data_list))
  alpha_mat <- matrix(rep(NA,length(data_list)^2),nrow=length(data_list),ncol=length(data_list))
  colnames(alpha_mat) <- paste(names(data_list),".y",sep="")
  rownames(alpha_mat) <- paste(names(data_list),".x",sep="")
  beta_mat <- alpha_mat
  cor_mat <- alpha_mat
  inner_names <- names(data_list)
  for(no in names(data_list)){
    for(ni in inner_names){
      if(ni!=no){
        d <- all_data[c(no,ni)]
        d <- d[!is.na(d[no])&!is.infinite(d[[no]])&!is.na(d[ni])&!is.infinite(d[[ni]]),]
        ft <- tryCatch({
                          lm(formula(paste(no,"~",ni)),d)
                       },error=function(cond){
                          message(cond)
                          return(list(list(NA,NA)))
                       })
        alpha_mat[[paste(ni,".x",sep=""),paste(no,".y",sep="")]] <- ft[[1]][[1]]
        beta_mat[[paste(ni,".x",sep=""),paste(no,".y",sep="")]]  <- ft[[1]][[2]]
        cor_mat[[paste(ni,".x",sep=""),paste(no,".y",sep="")]] <- cor(d[c(no)],d[c(ni)])
      }
    }
    inner_names <- inner_names[inner_names!=no]
  }
  rval <- list(alpha_mat,beta_mat,cor_mat)
  names(rval) <- c("Alpha","Beta","Correlation")
  return(rval)
}