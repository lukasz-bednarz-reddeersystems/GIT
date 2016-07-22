library(lubridate)

#Generate data frame of start and end dates for a user.
#Used to initialise ppmodel objects.
#trader is the user id.
#end is the most recent time (included, i.e. not upto this date, but inclusive)
#interval is the date decrement
#lookback is the number of decrements including the end date
#if calendar is true, it is assumed that end is the final day of a 
#calendar period (eg. month,week) and the date intervals are adjusted to
#span the calendar interval.
key_generator <- function(trader,end,interval,lookback,calendar=TRUE,lookback_unit='months',cols=c('id','start','end')){
	if(calendar){
		e <- end+1
	}
	else{
		e <- end
	}
	ends   <- seq(as.Date(e), length=lookback, by = paste("-",interval," ",lookback_unit,sep=""))-1
	s      <- seq(as.Date(e), length=2, by = paste("-",interval," ",lookback_unit,sep=""))[2]-1
	starts <- seq(s+1, length=lookback, by = paste("-",interval," ",lookback_unit,sep=""))-1
	keys <- data.frame(a=trader,b=starts)
	keys <- cbind(keys,ends)
	colnames(keys) <- cols
	return(keys)
}

three_monthly_lookback <- function(trader){
	days_this_month <- as.numeric(format(Sys.Date(),'%d'))
	end <- Sys.Date() - days_this_month 
	return(key_generator(trader,end,1,3))
}

 last_month <- function(trader){
	days_this_month <- as.numeric(format(Sys.Date(),'%d'))
	end <- Sys.Date() - days_this_month 
	return(key_generator(trader,end,1,1))	
}

dated_three_monthly_lookback <- function(trader,date){
	tryCatch({
			rdate <- as.Date(date)
		}, error = function(cond){
			stop(paste("Error when setting dated_three_monthly_lookback module key function for date value",date,":",cond))
		})
	days_this_month <- as.numeric(format(rdate,'%d'))	
	end <- rdate - days_this_month 
	return(key_generator(trader,end,1,3))
}

dated_four_monthly_lookback <- function(trader,date){
	tryCatch({
			rdate <- as.Date(date)
		}, error = function(cond){
			stop(paste("Error when setting dated_four_monthly_lookback module key function for date value",date,":",cond))
		})
	days_this_month <- as.numeric(format(rdate,'%d'))	
	end <- rdate - days_this_month 
	return(key_generator(trader,end,1,4))
}

dated_twelve_monthly_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_twelve_monthly_lookback module key function for date value",date,":",cond))
  })
  days_this_month <- as.numeric(format(rdate,'%d'))	
  end <- rdate - days_this_month 
  return(key_generator(trader,end,1,12))
}

dated_eighteen_monthly_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_twelve_monthly_lookback module key function for date value",date,":",cond))
  })
  days_this_month <- as.numeric(format(rdate,'%d'))	
  end <- rdate - days_this_month 
  return(key_generator(trader,end,1,18))
}

dated_three_day_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  end <- rdate
  return(key_generator(trader,end,3,1,lookback_unit='days'))	
}

dated_full_month <- function(trader,date){
  tryCatch({
      rdate <- as.Date(date)
    }, error = function(cond){
      stop(paste("Error when setting dated_three_monthly_lookback module key function for date value",date,":",cond))
    })
  end <- rdate %m+% months(1)
  day(end) <- 1
  
  end <- end %m-% days(1)

  return(key_generator(trader,end,1,1))
}


dated_one_year_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  
  end <- rdate
  month(end) <- 12
  day(end) <- 31
  
  if (end == rdate) {
    n <- 1
  } else {
    n <- 2
  }
  
  return(key_generator(trader,end,1,n,lookback_unit='years'))	
}

dated_three_year_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  
  end <- rdate
  month(end) <- 12
  day(end) <- 31
  
  if (end == rdate) {
    n <- 3
  } else {
    n <- 4
  }
  
  return(key_generator(trader,end,1,n,lookback_unit='years'))	
}



dated_whole_year_lookback <- function(trader, date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  end <- rdate
  month(end) <- 12
  day(end) <- 31
  
  return(key_generator(trader,end,1,1,lookback_unit='years'))	
}

three_year_lookback <- function(trader){
  today <- Sys.Date()
  
  end <- today
  month(end) <- 12
  day(end) <- 31
  return(key_generator(trader,end,1,3,lookback_unit='years'))	
}

last_year <- function(trader){
  today <- Sys.Date()
  
  end <- today %m-% years(1)
  month(end) <- 12
  day(end) <- 31

  return(key_generator(trader,end,1,1,lookback_unit='years'))	
}

this_year <- function(trader){
  today <- Sys.Date()
  
  end <- today
  month(end) <- 12
  day(end) <- 31
  
  return(key_generator(trader,end,1,1,lookback_unit='years'))	
}

range_years_lookback <- function(trader, start, end){
  tryCatch({
    sdate <- as.Date(start)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",start,":",cond))
  })
  tryCatch({
    edate <- as.Date(end)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",start,":",end))
  })
  
  end <- edate
  month(end) <- 12
  day(end) <- 31
  
  n <- year(end) - year(sdate) + 1
  
  return(key_generator(trader,end,1,n,lookback_unit='years'))	
}
