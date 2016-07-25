gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

gm_mean_bps <- function(x, na.rm=TRUE){
	non_bps <- (x/10000)+1
	gm_non_bps <- gm_mean(non_bps)
	gm_bps <- (gm_non_bps-1)*10000
	return(gm_bps)
}

rm_inf <- function(x){
  if(length(x)>0){
    if(x==Inf || x==-Inf || is.nan(x) || is.na(x)){
      rval=NA
    }else{
      rval=x
    }
  }
  else{
    rval = NA
  }
  return(rval)
}

rm_inf_zero <- function(x){
  if(length(x)>0){
    if(x==Inf || x==-Inf || is.nan(x) || is.na(x) || x==0){
      rval=NA
    }else{
      rval=x
    }
  }
  else{
    rval = NA
  }
  return(rval)
}

rm_zero_null <- function(x,rval=1){
  if(length(x)==0){rval}
  else if(x==0 || is.na(x)){rval}
  else{x}
}