ppmodel_subset_fn  <- function(by,with){
	if(with>1){
		rval <- by==with
	}
	else{
		rval <- by > 0
	}
	return(rval)
}

element_picker <- function(data_list,panel,element,data_frame_column,fn=NULL){
	if(length(fn)==0)fn <- sum
	cdex <- tryCatch({
				if(class(data_frame_column)[[1]]=='character'){
					which(colnames(data_list[[panel]][[element]])==data_frame_column,TRUE)
				}
				else if(class(data_frame_column)[[1]]=="NULL"){
					1:length(colnames(data_list[[panel]][[element]]))
				}
				else{
					data_frame_column
				}
			}, error = function(cond){
				message(paste("Panel element index failure for input",data_frame_column))
				return(NA)
			})
	rval <- tryCatch({
				fn(as.numeric(data_list[[panel]][[element]][,cdex]))
			}, error = function(cond){
				message(paste("Panel data element selection failure, panel:",panel,"element:",element,"column:",data_frame_column))
				return(NA)
			})
	if(length(rval)==0)rval <- NA
	return(rval)
}

convert_to_rate <- function(item){
	return(item/(item+1))
}