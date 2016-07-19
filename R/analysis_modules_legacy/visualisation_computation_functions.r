sourceTo("../lib/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

subset_data <- function(data,by,with,fn=NULL){
  	if(length(with)!=length(by))stop("Must set subset with value for each subset variable if subsetting data.")
  		for(i in 1:length(by)){
  			if(length(fn)==0){
          if(nrow(data)>0)data <- subset(data,data[by[i]]==with[[i]])   
  			}
  			else{
  				if(nrow(data)>0)data <- subset(data,fn[[i]](data[by[i]],with[[i]]))				
  			}
  		}
  	return(data)
}

data_aggregate_and_subset <- function(object,dataset,agg_unique=FALSE){
  if(object@psn_level)dataset<-collapse_data(dataset,c(object@aggregate_what,object@aggregate_by))
  aggregate <- aggregateGroup(dataset,c(object@aggregate_what),c(object@aggregate_by),object@aggregate_fn,agg_unique)  
  if(length(object@subset_by)>0){
  	if(length(object@subset_fn)>0){
  		aggregate <- subset_data(aggregate,object@subset_by,object@subset_with,fn=object@subset_fn)
  	}
  	else{
  		aggregate <- subset_data(aggregate,object@subset_by,object@subset_with)
  	}
  }
  return(aggregate)
}

#Apply list of aggregations sequentially
compound_aggregate_and_subset <- function(object,dataset,agg_unique=FALSE){
  if(object@psn_level)dataset<-collapse_data(dataset,c(object@aggregate_what,object@aggregate_by))
	aggregate <- aggregateGroupCompound(dataset,c(object@aggregate_what),object@aggregate_by,object@aggregate_fn,agg_unique)
  	if(length(object@subset_by)>0){
  		if(length(object@subset_fn)>0){
  			aggregate <- subset_data(aggregate,object@subset_by,object@subset_with,fn=object@subset_fn)
  		}
  		else{
  			aggregate <- subset_data(aggregate,object@subset_by,object@subset_with)
  		}
  	}
  	return(aggregate)
}

compound_aggregate_and_subset_unique <-  function(object,dataset){
	return(compound_aggregate_and_subset(object,dataset,agg_unique=TRUE))
}

data_aggregate_ratio_by_subset <- function(object,dataset){
	if(length(object@subset_with)!=2)stop('To take a subset ratio of a data aggregate subset_with must have length 2.')
	if(class(object@subset_with[[1]])!='list')stop('To take a subset ratio of a data aggregate, supply subset_with as a list of 2 lists.')
  if(object@psn_level)dataset<-collapse_data(dataset,c(object@aggregate_what,object@aggregate_by))
	aggregate <- aggregateGroup(dataset,c(object@aggregate_what),c(object@aggregate_by),object@aggregate_fn)  
	aggregate_subset <- list()
	by <- object@subset_by
	fn <- object@subset_fn
	for(i in 1:2){
		with <- object@subset_with[[i]]
		aggregate_subset[[i]] <- subset_data(aggregate,by,with,fn=fn)	
	}
	colnames(aggregate_subset[[1]])[colnames(aggregate_subset[[1]])==object@aggregate_what] <- paste(object@aggregate_what,"_1",sep="")
	colnames(aggregate_subset[[2]])[colnames(aggregate_subset[[2]])==object@aggregate_what] <- paste(object@aggregate_what,"_2",sep="")
	rm_vars <- by[unlist(Map(function(x,y)x!=y,object@subset_with[[1]],object@subset_with[[2]]))]
	merge_vars <- setdiff(object@aggregate_by,rm_vars)
	aggregate_subset <- merge(aggregate_subset[[1]],aggregate_subset[[2]][c(object@aggregate_by,paste(object@aggregate_what,"_2",sep=""))],by=merge_vars,all.x=TRUE)
	if(nrow(aggregate_subset)>0){
		aggregate_subset <- subset(aggregate_subset,(!is.na(aggregate_subset[paste(object@aggregate_what,"_2",sep="")]))&(aggregate_subset[paste(object@aggregate_what,"_2",sep="")]!=0))
		aggregate_subset[object@aggregate_what] <- aggregate_subset[paste(object@aggregate_what,"_1",sep="")]/aggregate_subset[paste(object@aggregate_what,"_2",sep="")]
	}
	return(aggregate_subset[c(object@aggregate_what,setdiff(object@aggregate_by,rm_vars))])
}

collapse_data <- function(dataset,columns){
  data <- unique(dataset@data[c('Instrument','StrategyID',columns)])
  collapsed <- dataset_factory(c('Instrument','StrategyID'),data)
  return(collapsed)
}

snapshot_aggregate_by_subset <- function(object,dataset){
  snap_cols <- unlist(Map(function(x)paste(object@snapshot_col_pre,x,sep="_"),-object@window:object@window))
  snap_cols <- gsub("-","m",snap_cols)
  #Compute the aggregate over subsets of the timeseries data
  for(fn in 1:length(object@aggregate_fn)){
    aggregate <- aggregateGroup(dataset,snap_cols,c(object@aggregate_by),object@aggregate_fn[[fn]],TRUE)  
    aggregate <- subset_data(aggregate,object@subset_by,object@subset_with,fn=object@subset_fn)  
    aggregate$AggregateType <- names(object@aggregate_fn)[fn]
    if(fn==1){
      all_aggregate <- aggregate
    }
    else{
      all_aggregate <- rbind(all_aggregate,aggregate)
    }
  }
  #Select data for each trajectory in the aggregate as distinguished by unique_by
  data      <- unique(dataset@data[c(object@unique_by,object@aggregate_by,snap_cols)])
  data      <- subset_data(data,object@subset_by,object@subset_with,fn=object@subset_fn)
  output    <- rbind.fill(all_aggregate,data)
  return(output)
}