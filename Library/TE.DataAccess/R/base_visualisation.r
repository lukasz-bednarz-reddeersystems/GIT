setClass(
	Class          = "BaseVisualisation",
	representation = representation(
		visuln_data= "data.frame",
		visuln_comp= "function",
		visuln_dspl= "function",
		plot_data  = "data.frame",
		create_plot= "logical", 
		title      = "character",
		vsn_type   = "character",
		data_type  = "character",
		psn_level  = "logical"
	),
	prototype      = prototype(
		create_plot= TRUE,
		psn_level  = FALSE
	)
)

setGeneric("triggerVisualisationComputation",function(object,model_data){standardGeneric("triggerVisualisationComputation")})
setMethod("triggerVisualisationComputation","BaseVisualisation",
		  function(object,model_data){
		  	if(class(model_data)[[1]]!='DataSet')stop("DataSet expected as input to visualisation.")
		  	tryCatch({
		  			object@visuln_data <- object@visuln_comp(object,model_data)	
		  		},error=function(cond){
		  			message(paste("Visualisation data generation failed for",class(object)[[1]],":",cond))
		  			return(object)
		  		})		  	
		  	message(paste(class(object)[[1]],"visualisation data generated."))
		  	return(object)
		  }
)

setGeneric("createVisualisation",function(object){standardGeneric("createVisualisation")})
setMethod("createVisualisation","BaseVisualisation",
		  function(object){
		  	if(nrow(object@visuln_data)>0){
		  		tryCatch({
		  			object <- object@visuln_dspl(object)	
		  		},error=function(cond){
		  			message(paste("Visualisation generation failed for",class(object)[[1]],":",cond))
		  			return(object)
		  		})		  	
		  	}
		  	else{
		  		message(paste("No data for",class(object)[[1]]))
		  		plot(0,0,type='n',axes=FALSE,ann=FALSE)
		  		if(length(object@title)>0){
		  			title(object@title)
		  		}

		  	}
		    return(object)
		  }
)

visualisation_class_factory <- function(name,visuln_comp,visuln_dspl,file_name){
	cs <- paste("setClass(Class='",name,"',prototype=prototype(",sep="")
	cs <- paste(cs,"visuln_comp=visuln_comp,visuln_dspl=visuln_dspl,file_name='",file_name,"'),",sep="")
	cs <- paste(cs,"contains=c('BaseVisualisation'))",sep="")
	tryCatch({
			eval(parse(text=cs))
			message(paste(name,"created."))
		},error=function(cond){
			message(paste("Error during creation of",name,":"))
			message(cs)
			stop()
		})
}