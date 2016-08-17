compute_pl_cumulant <- function(all_pnl,raw_pnl,pltype,strat,first,colname,offtype=NULL){
  if(length(offtype)==0){
    strat_pnl <- raw_pnl[raw_pnl$Strategy==strat&raw_pnl$PLType==pltype,]  
  } else {
    strat_pnl <- raw_pnl[raw_pnl$Strategy==strat&raw_pnl$PLType==pltype&raw_pnl$OffType==offtype,]  
  }
  strat_pnl <- strat_pnl[order(strat_pnl$TradeDate),]
  strat_pnl$CumulativePL <- cumsum(strat_pnl[colname])
  if(first){
    all_pnl <- strat_pnl
  } else {
    all_pnl <- rbind(all_pnl,strat_pnl)
  }
  return(all_pnl)
}

multi_aggregate <- function(data,aggregate_columns,over_columns,with_fn,generic_column){
  if(length(aggregate_columns)>0){
    agg <- aggregate(data[aggregate_columns],over_columns,with_fn)
    if(length(aggregate_columns)>1){
      first <- TRUE
      for(col in aggregate_columns){
        other_cols <- setdiff(colnames(agg),aggregate_columns)
        un <- agg[c(col,other_cols)]
        colnames(un) <- c(generic_column,other_cols)
        un <- cbind(PLType=col,un)
        if(first){
          unrolled <- un 
          first <- FALSE
        } else {
          unrolled <- rbind(unrolled,un)
        }
      }
    } else {
      colnames(agg)[colnames(agg)==aggregate_columns] <- generic_column
      agg <- cbind(PLType=aggregate_columns,agg)
      unrolled <- agg
    }
  } else {
    unrolled <- data[data[names(aggregate_columns)[1]]==NULL,]
  }
  return(unrolled)
}