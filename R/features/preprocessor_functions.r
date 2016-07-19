ftr_gther <- function(compute_object){
  compute_object@output <- dataset_factory(compute_object@key_cols,compute_object@input)  
  return(compute_object)
}

ftiler <- function(compute_object,ntiles,name='Quartile'){
	data <- compute_object@input
	cnames <- colnames(data)
	on <- compute_object@ntile_on
	data$ntile <- with(data[on], cut(as.numeric(unlist(data[on])), breaks=unique(quantile(data[on], probs=seq(0,1, by=1/ntiles), na.rm=TRUE)), include.lowest=TRUE))
	colnames(data) <- c(cnames,paste(on,name,sep=""))
	compute_object@output <- dataset_factory(compute_object@key_cols,data)	
	return(compute_object)
}
quartiler <- function(compute_object){
	return(ftiler(compute_object,4))
}
deciler <- function(compute_object){
	return(ftiler(compute_object,10,name="Decile"))
}
