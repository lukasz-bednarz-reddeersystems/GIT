#' @include models_ppmodel.r
#' @include global_configs.r
#' @include visualisation_library.r
NULL


setClass(
	Class          = "Panel",
	representation = representation(
		file_path  = 'character',
		plot_to_file='logical',
		file       = 'character',
		visualns   = 'list',
		parameters = 'character'
	),
	prototype      = prototype(
		plot_to_file=FALSE,
		file       = 'panel_output.jpg',
		file_path  = visualisation_defaults@file_path,
		parameters = 'mfrow=c(1,1)'
	)
)

setClassUnion("NullableFunction",c("NULL","function"))
setClass(
	Class          = "VirtualAnalysisModule",
	representation = representation(
		#ppmdl slot of correct type
		#dynamically added by factory
		panels     = 'list',
		plot_none  = 'logical',
		panel_comp = 'NullableFunction',
		comp_result= 'list',
		comp_name  = 'character'
	),
	prototype = prototype(
	  plot_none = FALSE,
	  panel_comp = NULL
	)
)

setGeneric("getPanelData",function(object,panel_number){standardGeneric("getPanelData")})
setMethod("getPanelData","VirtualAnalysisModule",
		   function(object,panel_number){
		   		panel <- object@panels[[panel_number]]
		   		panel_data <- list()
		   		names <- c();
		   		for(v in 1:length(panel@visualns)){
		   			vsl <- panel@visualns[[v]]
		   			panel_data[[v]] <- vsl@plot_data
		   			names <- c(names,paste(vsl@title,"-",vsl@y_label,"-",vsl@x_label,sep=""))
		   		}
		   		names(panel_data) <- names
		   		return(panel_data)
		   }
)

setGeneric("getModuleData",function(object){standardGeneric("getModuleData")})
setMethod("getModuleData","VirtualAnalysisModule",
		  function(object){
		  	rval <- list()
		  	for(p in 1:length(object@panels)){
		  		rval[[p]] <- getPanelData(object,p)
		  	}
		  	if(length(object@comp_result)>0){
		  		rval[[length(rval)+1]] <- object@comp_result
		  	}
		  	return(rval)
		  }
)

setGeneric("setPanelComputation",function(object,panel_computation,computation_name){standardGeneric("setPanelComputation")})
setMethod("setPanelComputation","VirtualAnalysisModule",
		   function(object,panel_computation,computation_name){
		   		object@comp_name <- computation_name
		   		object@panel_comp <- panel_computation
		   		return(object)
		   }
)

setGeneric("triggerPanelComputation",function(object){standardGeneric("triggerPanelComputation")})
setMethod("triggerPanelComputation","VirtualAnalysisModule",
		   function(object){
		   		message(paste("Trggering panel computations for",object@comp_name,"in",class(object)[[1]]))
		   		in_data <- list()
		   		for(p in 1:length(object@panels)){
		   			in_data[[p]] <- getPanelData(object,p)
		   		}
		   		if(length(in_data)>0 && length(object@panel_comp)>0){
					tryCatch({
						object@comp_result <- object@panel_comp(in_data,object@comp_name)
					}, error=function(cond){
						message(paste("Error when applying panel computation ",object@comp_name," to ",class(object)[[1]],": ",cond,sep=""))
					})
				}
		   		else{
		   			message("Attempt to trigger panel computation when either computation is not defined, or there is no panel data.")
		   		}
		   		return(object)
		   }
)

setGeneric("resetPanels",function(object,panels){standardGeneric("resetPanels")})
setMethod("resetPanels","VirtualAnalysisModule",
	      function(object,panels){
	      	object@panels <- panels
	      	return(object)
	      }
)

setGeneric("updateAnalysisModel",function(object){standardGeneric("updateAnalysisModel")})
setMethod("updateAnalysisModel","VirtualAnalysisModule",
	       function(object){
	       		tryCatch({
	       				object@ppmdl <- runPreProcessorModel(object@ppmdl)
	       			},error = function(cond){
	       				if(length(grep("cannot allocate vector of size",cond[['message']]))>1){
	       					message("Memory allocation failure during update, flushing caches.")
	       					initialise_data_store()
	       					message("Retrying.")
	       					return(runPreProcessorModel(object@ppmdl))
	       				}
	       				else{
	       					stop(paste("Error when updating model",class(object)[[1]],":",cond[['message']]))
	       				}
	       			})
		    	return(object)
		   }
)

setGeneric("recomputeModelFeatures",function(object,features,push_change=NULL){standardGeneric("recomputeModelFeatures")})
setMethod("recomputeModelFeatures","VirtualAnalysisModule",
	       function(object,features,push_change=NULL){
	       	  	message(paste("Recompute features triggered for preprocessor model",class(object@ppmdl)[[1]],"in analysis module",class(object)[[1]]))
	       	  	object@ppmdl <- recomputeFeaturesForAll(object@ppmdl,features,push_change)
	       	  	return(object)
	       }
)

setGeneric("recomputeModelSummaries",function(object,push_change=NULL){standardGeneric("recomputeModelSummaries")})
setMethod("recomputeModelSummaries","VirtualAnalysisModule",
		   function(object,push_change=NULL){
		   		message(paste("Recompute summaries triggered for preprocessor model",class(object@ppmdl)[[1]],"in analysis module",class(object)[[1]]))
		   		object@ppmdl <- recomputeSummaryForAll(object@ppmdl,push_change)
		   		return(object)
		   }
)

setGeneric("runAnalysisModule",function(object){standardGeneric("runAnalysisModule")})
setMethod("runAnalysisModule","VirtualAnalysisModule",
		   function(object){
		   		if(length(object@ppmdl@modeldata@data)==0)stop("No preprocessor model data found, updateAnalysisModel first.")
		   		if(length(object@panels)==0){
		   			message("No visualisations created for module.")
		   		}
		   		else{
		   			message("Creating panel ...")
		   			for(p in 1:length(object@panels)){
		   				panel <- object@panels[[p]]
		   				if(panel@plot_to_file){
		   					fchar <- substr(panel@file_path,nchar(panel@file_path),nchar(panel@file_path))
		   					if(fchar!='/')panel@file_path <- paste(panel@file_path,'/',sep="")
		   					tryCatch({eval(parse(text=paste('jpeg("',panel@file_path,panel@file,'")',sep="")))},error=function(cond){message(paste("Could not set output file:",cond))})
		   				}
		   				tryCatch({eval(parse(text=paste('par(',panel@parameters,')')))},error=function(cond){message(paste("Could not set panel parameters.",cond))})
		   				message("Adding visualisations to panel ...")
		   				if(object@plot_none)message("Plot none flag is set, no graphical output will be produced.")
			   			for(v in 1:length(panel@visualns)){
			   				vsl <- panel@visualns[[v]]
			   				vsl@create_plot <- !object@plot_none
			   				message(paste("Generating visualisation",class(vsl)[[1]]))
			   				vsl <- triggerVisualisationComputation(vsl,object@ppmdl@modeldata)
			   				vsl <- createVisualisation(vsl)
			   				panel@visualns[[v]] <- vsl
			   			}
			   			if(panel@plot_to_file)dev.off()
			   			object@panels[[p]] <- panel

		   			}
		   			if(length(object@panel_comp)>0){
			   				message("Running panel computations ...")
			   				object <- triggerPanelComputation(object)
			   		}
			   	}
			   	return(object)
		   	}
)

setGeneric("togglePlotNone",function(object){standardGeneric("togglePlotNone")})
setMethod("togglePlotNone","VirtualAnalysisModule",
          function(object){
            object@plot_none <- !object@plot_none
            message(paste(class(object)[[1]],"plot none flag is",object@plot_none))
            return(object)
          }
)

setClass(
	Class          = "AnalysisModuleFactory",
	representation = representation(
		name       = 'character',
		ppmdl_class= 'character',
		visualisations = 'list',
		panel_computation = 'NullableFunction',
		computation_name = 'character',
		key_func   = 'function'
	),
	prototype      = prototype(
		panel_computation = NULL
	)
)


#' Method to create AnalysisModules
#'
#' @param object object of class "AnalysisModuleFactory"
#' @param key_func "function" key generator function
#' @return \code{rval} object of class derived from "VirtualAnalysisModule"
#' @export

setGeneric("createAnalysisModule",function(object,key_func){standardGeneric("createAnalysisModule")})

#' @describeIn createAnalysisModule
#' Method to create AnalysisModules
#'
#' @inheritParams createAnalysisModule
#' @return \code{rval} object of class derived from "VirtualAnalysisModule"
#' @export
setMethod("createAnalysisModule","AnalysisModuleFactory",
		  function(object,key_func){
		  	object@key_func <- key_func
		  	tryCatch({object@name},error=function(cond){stop("Class name property not set.")})
          	tryCatch({object@ppmdl_class},error=function(cond){stop("Class ppmodel_class property not set.")})
          	ppmdl_instance <- new(object@ppmdl_class,keys=object@key_func())
          	if(length(tryCatch({object@visualisations},error=function(cond){}))>0){
          		am_prototype<-prototype(ppmdl=ppmdl_instance,panels=object@visualisations)
		  	}
		  	else{
		  		message(paste("No visualisations specified for",class(object)[[1]]))
		  		am_prototype<-prototype(ppmdl=ppmdl_instance)
		  	}
		  	tryCatch({
		  			setClass(Class=object@name,representation=representation(ppmdl=object@ppmdl_class),prototype=am_prototype,contains=c('VirtualAnalysisModule'))
		  			message(paste("Class",object@name,"created."))
		  		},error=function(cond){
		  			message(paste("Error when trying to create AnalysisModule:",cond))
		  			stop(paste("Could not create class",object@name))
		  		})
		    message(paste("Now creating an instance of",object@name))
		    rval <- tryCatch({
		    			new(object@name)
		    	    },error=function(cond){
		    	    	message(paste("Failed to create class instance:",cond))
		    	    	message("Factory instance returned.")
		    	    	return(object)
		    	    })
		    if(length(object@panel_computation)>0){
		    	if(length(object@computation_name)==0)object@computation_name <- paste(class(rval)[[1]],"PanelComputation",sep="")
		    	message(paste("Setting panel computation",object@computation_name,"in",class(rval)[[1]]))
		    	rval <- setPanelComputation(rval,object@panel_computation,object@computation_name)
		    }
		  	return(rval)
		  }
)

