#' Generate data frame of start and end dates for a user.
#'
#' Generate data frame of start and end dates for a user.
#' Used to initialise ppmodel objects.
#'
#' @param trader integer, the user id
#' @param end  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @param interval  integer, the date decrement
#' @param lookback  integer, the number of decrements including the end date
#' @param calendar  logical, if calendar is true, it is assumed that end is the final day of a
#' calendar period (eg. month,week) and the date intervals are adjusted to span the calendar interval.
#' @param lookback_unit character, name of time unit as recognized by lubridate:
#' c("days", "weeks", "months", "years"), default is "months"
#' @param cols character, name output columns, default is c('id','start','end')
#' @return \code{keys} data.frame with generated keys.

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

#' Generate keys for three months lookback for user
#'
#' Generate three months lookback keys for user with
#' last day of previous month as last date
#'
#' @param trader integer, the user id
#' @return \code{keys} data.frame with generated keys.
#' @export

three_monthly_lookback <- function(trader){
	days_this_month <- as.numeric(format(Sys.Date(),'%d'))
	end <- Sys.Date() - days_this_month
	return(key_generator(trader,end,1,3))
}

#' Generate key for last month for user
#'
#' Generate key for last month for user with
#' last day of previous month as last date
#' and one month interval.
#'
#' @param trader integer, the user id
#' @return \code{keys} data.frame with generated keys.
#' @export

last_month <- function(trader){
	days_this_month <- as.numeric(format(Sys.Date(),'%d'))
	end <- Sys.Date() - days_this_month
	return(key_generator(trader,end,1,1))
}

#' Generate keys for three months lookback for specific date
#'
#' Generate three months lookback keys for user with
#' last day of previous month with regards to date as last date
#' and one month interval.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

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

#' Generate keys for four months lookback for specific date
#'
#' Generate four months lookback keys for user with
#' last day of previous month with regards to date as last date
#' and one month interval.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

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

#' Generate keys for twelve months lookback for specific date
#'
#' Generate twelve months lookback keys for user with
#' last day of previous month with regards to date as last date
#' and one month interval.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

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

#' Generate keys for eighteen months lookback for specific date
#'
#' Generate eighteen months lookback keys for user with
#' last day of previous month with regards to date as last date
#' and one month interval.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_eighteen_monthly_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_eighteen_monthly_lookback module key function for date value",date,":",cond))
  })
  days_this_month <- as.numeric(format(rdate,'%d'))
  end <- rdate - days_this_month
  return(key_generator(trader,end,1,18))
}


#' Generate keys for three days lookback for specific date
#'
#' Generate three days lookback keys for user with one day interval.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_three_day_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  end <- rdate
  return(key_generator(trader,end,3,1,lookback_unit='days'))
}


#' Generate key for full month of specific date
#'
#' Generate one month key for user.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated key.
#' @export

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

#' Generate key for one year lookback for whole year of specific date
#'
#' Generate key for user with one year interval and one year lookback.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_one_year_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })

  end <- rdate
  day(end) <- 1
  end <- end - 1

  # if (end == rdate) {
  #   n <- 1
  # } else {
  #   n <- 2
  # }
  n <- 12

  return(key_generator(trader,end,1,n,lookback_unit='months'))
}


#' Generate keys for whole three years lookback including date year
#'
#' Generate keys for user with one year interval and three year lookback.
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_three_year_lookback <- function(trader,date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })

  end <- rdate
  month(end) <- 12
  day(end) <- 31

  n <- 3 * 12

  return(key_generator(trader,end,1,n,lookback_unit='months'))
}


#' Generate key for whole year lookback from begining to end of date's year
#'
#' Generate key for whole year lookback from begining to end of date's year
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_whole_year_lookback <- function(trader, date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  end <- rdate
  month(end) <- 12
  day(end) <- 31
  num <- month(end)

  return(key_generator(trader,end,1,num,lookback_unit='months'))
}


#' Generate keys for this whole year lookback from begining to end of today()'s year
#'
#' Generate key for whole year lookback from begining to end of this year
#'
#' @param trader integer, the user id
#' @return \code{keys} data.frame with generated keys.
#' @export

three_year_lookback <- function(trader){
  today <- Sys.Date()

  end <- today
  month(end) <- 12
  day(end) <- 31
  num <- 36
  return(key_generator(trader,end,1,num,lookback_unit='months'))
}

#' Generate keys for last whole year lookback from begining to end of last year
#'
#' Generate key for whole year lookback from begining to end of last year
#'
#' @param trader integer, the user id
#' @return \code{keys} data.frame with generated keys.
#' @export

last_year <- function(trader){
  today <- Sys.Date()

  end <- today %m-% years(1)
  month(end) <- 12
  day(end) <- 31
  num <- 12
  return(key_generator(trader,end,1,num,lookback_unit='months'))
}

#' Generate keys for this whole year lookback from begining to end of today()'s year
#'
#' Generate key for whole year lookback from begining to end of this year
#'
#' @param trader integer, the user id
#' @return \code{keys} data.frame with generated keys.
#' @export

this_year <- function(trader){
  today <- Sys.Date()

  end <- today
  month(end) <- 12
  day(end) <- 31
  num <- 12
  return(key_generator(trader,end,1,num,lookback_unit='months'))
}


#' Generate keys for this whole year lookback from begining to date
#'
#' Generate key for whole year lookback from begining to the date month
#'
#' @param trader integer, the user id
#' @param date  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

dated_this_year <- function(trader, date){
  tryCatch({
    rdate <- as.Date(date)
  }, error = function(cond){
    stop(paste("Error when setting dated_three_day_lookback module key function for date value",date,":",cond))
  })
  end <- rdate
  day(end) <- 1
  end <- end -1
  end <- end %m+% months(1)
  num <- month(end)
  return(key_generator(trader,end,1,num,lookback_unit='months'))
}

#' Generate keys for whole including years start and end years
#'
#' Generate keys for whole including years start and end years
#'
#' @param trader integer, the user id
#' @param start  Date, the most distant time (included, i.e. not upto this date, but inclusive)
#' @param end  Date, the most recent time (included, i.e. not upto this date, but inclusive)
#' @return \code{keys} data.frame with generated keys.
#' @export

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

  n <- 12*(year(end) - year(sdate)) + 1

  return(key_generator(trader,end,1,n,lookback_unit='months'))
}

#' Function to generate date only keys over 2 year lookback
#' Generate keys for whole including years start and end years
#'
#' Generate keys for whole including years start and end years
#'
#' @return \code{keys} data.frame with generated keys.
#' @export

two_year_monthly_lookback <- function(){
  end <- Sys.Date()
  day(end) <- 1
  end <- end %m+% days(-1)
  num <- 24
  df <- key_generator(11,end,1,num,lookback_unit='months',cols=c('id','start','end'))
  return(df[c('start','end')])
}
