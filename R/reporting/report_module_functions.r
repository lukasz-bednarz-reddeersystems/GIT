identity <- function(a,b)a==b

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
				message(paste("Panel element index failure for input",data_frame_column,":",cond))
				return(NA)
			})
	rval <- tryCatch({
				fn(as.numeric(data_list[[panel]][[element]][,cdex]))
			}, error = function(cond){
				message(paste("Panel data element selection failure, panel:",panel,"element:",element,"column:",data_frame_column,"error:",cond))
				return(NA)
			})
	if(length(rval)==0)rval <- NA
	return(rval)
}

convert_to_rate <- function(item){
	return(item/(item+1))
}

riffle <- function(a,b) {
  if (!all(dim(a) == dim(b))) stop("Riffle: Dimensions do not match")
  return(array(rbind(a, b), dim=c(dim(a)[1], dim(a)[1]+dim(b)[1])))
}

value_or_default <- function(value,default){
	rval <- value
	if(length(value)==0){
		rval <- default
	}
	return(rval)
}

subset_with_append <- function(subset_with,new_value){
	rval <- subset_with
	if(class(subset_with[[1]])=='list'){
		rval[[1]] <- c(rval[[1]],new_value)
		rval[[2]] <- c(rval[[2]],new_value)
	}
	else{
		rval <- c(rval,new_value)
	}
	return(rval)
}