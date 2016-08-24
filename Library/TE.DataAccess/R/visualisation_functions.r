frequency_plot <- function(object){
	ps <- "barplot(object@visuln_data[[object@aggregate_what]]"
	object@plot_data <- as.data.frame(t(object@visuln_data[[object@aggregate_what]]))
	if(length(object@y_label)>0){
		ps <- paste(ps,",ylab='",object@y_label,"'",sep="")
		rownames(object@plot_data) <- object@y_label
	}
	if(length(object@x_label)>0){
		ps <- paste(ps,",xlab='",object@x_label,"'",sep="")
	}
	if(length(object@x_label_variable)>0){
		ps <- paste(ps,",names.arg=object@visuln_data[[object@x_label_variable]]",sep="")
		colnames(object@plot_data) <- object@visuln_data[[object@x_label_variable]]
	}
	if(length(object@title)>0){
		ps <- paste(ps,",main='",object@title,"'",sep="")
	}
	ps <- paste(ps,")")

	if(object@create_plot){
		tryCatch({
				eval(parse(text=ps))
			},error=function(cond){
				message(paste("Error during visualisation of",class(object)[[1]]))
				message(paste("Command:",ps))
				message(paste("Failed with:",cond))
				message(paste("Values:"),object@visuln_data[[object@aggregate_what]])
				if(length(object@x_label_variable)>0)message(paste(" x-variable names: ",object@visuln_data[[object@x_label_variable]]))
				stop()
			})
	}
	return(object)
}

check_format_key <- function(key){
	rval <- ""
	if(nchar(key)>0){
		rval <- paste(",",key,sep="")
	}
	return(rval)
}

aggregate_timeseries_plot <- function(object){
	snap_cols <- unlist(Map(function(x)paste(object@snapshot_col_pre,x,sep="_"),-object@window:object@window))
  	snap_cols <- gsub("-","m",snap_cols)
	primary_data <- subset(object@visuln_data,object@visuln_data$AggregateType=='primary')
	primary_data <- primary_data[snap_cols]
	primary_format <- check_format_key(object@format_key['primary'])
	secondary_data <- subset(object@visuln_data,object@visuln_data$AggregateType=='secondary')
	secondary_data <- secondary_data[snap_cols]
	y_min <- min(primary_data)
	y_max <- max(primary_data)
	for(r in 1:nrow(secondary_data)){
		secondary_data[r,] <- primary_data[1,] + secondary_data[r,]
		yn <- min(secondary_data[r,])
		if(yn < y_min)y_min <- yn
		yx <- max(secondary_data[r,])
		if(yx > y_max)y_max <- yx
	}
	secondary_format <- check_format_key(object@format_key['secondary'])
	object@plot_data <- rbind(primary_data,secondary_data)
	ps <- paste('plot(',-object@window,':',object@window,',as.numeric(primary_data[snap_cols])',primary_format,sep="")
	if(length(object@y_label)>0){
		ps <- paste(ps,",ylab='",object@y_label,"'",sep="")
	}
	if(length(object@x_label)>0){
		ps <- paste(ps,",xlab='",object@x_label,"'",sep="")
	}
	if(length(object@title)>0){
		ps <- paste(ps,",main='",object@title,"'",sep="")
	}
	ps <- paste(ps,",ylim=c(",y_min,",",y_max,")",sep="")
	if(length(object@options)>0){
	  ps <- paste(ps,", ",object@options,sep="")
	}
	ps <- paste(ps,")",sep="")
	if(object@create_plot){
		tryCatch({
				eval(parse(text=ps))
			},error=function(cond){
				message(paste("Error during visualisation of",class(object)[[1]]))
				message(paste("Command:",ps))
				message(paste("Failed with:",cond))
				message(paste("Values:"),object@visuln_data[[object@aggregate_what]])
				if(length(object@x_label_variable)>0)message(paste(" x-variable names: ",object@visuln_data[[object@x_label_variable]]))
				stop()
			})
	}
	for(r in 1:nrow(secondary_data)){
	 	ps <- paste('lines(',-object@window,':',object@window,',as.numeric(secondary_data[',r,',snap_cols])',secondary_format,")",sep="")
	 	if(object@create_plot){
	 		tryCatch({
	 				eval(parse(text=ps))
	 			},error=function(cond){
	 				message(paste("Error during visualisation of",class(object)[[1]]))
	 				message(paste("Command:",ps))
	 				message(paste("Failed with:",cond))
	 				message(paste("Values:"),object@visuln_data[[object@aggregate_what]])
	 				if(length(object@x_label_variable)>0)message(paste(" x-variable names: ",object@visuln_data[[object@x_label_variable]]))
	 				stop()
	 			})
	 	}
	}
	return(object)
}