sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/report_module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Report modules manage analysis modules and if required perform simple aggregation and presentation
#across module outputs.
#The above operations require a top level class to trigger analysis retrieval/computation 
#This will be how the reporting interfaces and the engine interface with the report modules 
#Also will handle operations such as comparison between module outputs, such as if comparing 
#traders.

setClass(
	Class          		 = "ReportModule",
	representation 		 = representation(
		analysis_store   = 'AnalysisObjectStore',
		analysis_modules = 'list',
		builders         = 'list',
		store_modules    = 'logical',
		store_prefix     = 'character',
		no_plots         = 'logical',
		build_if_none    = 'logical',
		key_hash		 = 'list'
	),
	prototype 			 = prototype(
		store_prefix     = reporting_defaults@analy_store,
		analysis_modules = list(),
		key_hash         = list(),
		store_modules    = TRUE,
		no_plots         = TRUE,
		build_if_none    = FALSE
	)
)

setGeneric("buildReportModule",function(object,key_func){standardGeneric("buildReportModule")})
setMethod("buildReportModule","ReportModule",
		  function(object,key_func){
		  	nms <- c()
		  	for(b in 1:length(object@builders)){
		  		new_mod <- createAnalysisModule(object@builders[[b]],key_func)
		  		if(object@store_modules){
		  			message(paste(class(object)[[1]],"using an analysis store."))
		  			object@key_hash[[b]] <- as.character(murmur3.32(as.character(key_func())))
		  			object@analysis_store <- analysis_objectstore_factory(paste(object@store_prefix,class(new_mod)[[1]],"_",object@key_hash[[b]],sep=""))
		  			old_mod <- queryAnalysisStore(object@analysis_store,data.frame(key_hash=object@key_hash[[b]],analysis_module=class(new_mod)[[1]]))
		  			if(length(old_mod)==0){
		  				if(object@build_if_none){
		  					message("Analysis not found in store, creating new module...")
		  					object@analysis_modules[[b]] <- new_mod
		  					object@analysis_modules[[b]] <- updateAnalysisModel(object@analysis_modules[[b]])		
		  				}
		  				else{
		  					stop("Analysis module not found in store. Re-run with 'build_if_none' slot TRUE to create new modules.")
		  				}
		  			}
		  			else{
		  				object@analysis_modules[[b]] <- old_mod
		  			}
		  		}
		  		else{
		  			if(!object@build_if_none){
		  				stop(paste(class(object)[[1]],"not configured to use a store or to update new modules."))
		  			}
		  			else{
		  				object@analysis_modules[[b]] <- new_mod
		  				object@analysis_modules[[b]] <- updateAnalysisModel(object@analysis_modules[[b]])		
		  			}
		  		}
		  		if(object@no_plots)object@analysis_modules[[b]] <- togglePlotNone(object@analysis_modules[[b]])
		  		nms <- c(nms,class(object@analysis_modules[[b]])[[1]])
		  	}
		  	names(object@analysis_modules) <- nms
		  	return(object)
		  }
)

setGeneric("runReportModule",function(object){standardGeneric("runReportModule")})
setMethod("runReportModule","ReportModule",
		  function(object){
		  	for(m in 1:length(object@builders)){
		  		object@analysis_modules[[m]] <- runAnalysisModule(object@analysis_modules[[m]])
		  		if(object@store_modules){
		  			object@analysis_store <- updateAnalysisStore(object@analysis_store,object@analysis_modules[[m]],data.frame(key_hash=object@key_hash[[m]],analysis_module=class(object@analysis_modules[[m]])[[1]]))
		  			commitAnalysisStore(object@analysis_store)
		  		}
		  	}
		    return(object)
		  }
)

setGeneric("listAnalysisModules",function(object){standardGeneric("listAnalysisModules")})
setMethod("listAnalysisModules","ReportModule",
		  function(object){
		  	return(unlist(Map(function(x)class(x)[[1]],object@analysis_modules)))
		  }
)		 

setGeneric("getModuleData",function(object,module_name){standardGeneric("getModuleData")})
setMethod("getModuleData","ReportModule",
	      function(object,module_name){
	      	mod <- object@analysis_modules[[module_name]]
	      	if(length(mod)>0){
	      		data <- getAllModuleData(mod)
	      	}
	      	else{
	      		data <- NULL
	      	}
	      	return(data)
	      }
)

setGeneric("getPPModels",function(object){standardGeneric("getPPModels")})
setMethod("getPPModels","ReportModule",
	      function(object){
	      	ppmods <- list()
	      	for(a in 1:length(object@analysis_modules)){
	      		ppmods[[a]] <- object@analysis_modules[[a]]@ppmdl
	      	}
	      	return(ppmods)
		  }
)

setClassUnion("NullablePanelComputationDefinition",c("NULL","PanelComputationDefinition"))
setClass(
	Class          = "ReportModuleFromDefinition",
	representation = representation(
		module_definition = "ReportModuleDefinition",
		periodicity       = "numeric",
		panel_comp_defn   = "NullablePanelComputationDefinition"
	)
	contains 	   = c("ReportModule") 
)

setGeneric("generateReportModule",function(object){standardGeneric("generateReportModule")})
setMethod("generateReportModule","ReportModuleFromDefinition",
		  function(object){

		  }
)

setClass(
	Class 			= "ReportModuleDefinition",
	representation  = representation(
		panel_defns = "list",
		context_callback = "function",
		parameters  = "character"
	)
)

setGeneric("computeNPanes",function(object){standardGeneric("computeNPanes")})
setMethod("computeNPanes","ReportModuleDefinition",
		  function(object){

		  }
)

setGeneric("generatePanels",function(object){standardGeneric("generatePanels")})
setMethod("generatePanels","ReportModuleDefinition",
		  function(object){
		  	#context is potentially a combination of which panel is being generated
		  	#as well as the state of the subset by variables within the panel
		  	#Need to build the context map in here
		  	contexts <- names(panel_defns)
		  	#loop over contexts
		  		#create panel
		  		#callback to append to context map
		  		# - callback has access to the panel level context as well as the 
		  		#   visulaisation contexts within the panel: needs to select correct data
		  		#   for each output row and store as a list of indices.
		  }
)

setClass(
	Class           = "VirtualPanelDefinition"
	representation  = representation(
		target_class= "character"
	)
)

setGeneric("fillSlots",function(object,slot_value_list){standardGeneric("fillSlots")})
setMethod("fillSlots","VirtualPanelDefinition",
		  function(object,slot_value_list){
		  	obj <- new(object@target_class)
		  	objslots <- slotNames(obj)
		  	slots <- names(slot_value_list)
		  	if(length(objslots)<length(slots))stop("Too many slot names.")
		  	for(slt in slots){
		  		if(length(slot_value_list[[slt]])>0){
			  		obj <- tryCatch({
			  					slot(obj,slt) <- slot_value_list[[slt]]
			  				}, error = function(cond){
			  					message(paste("Could not set slot",slt,"in",object@target_class,":",cond))
			  					stop()
			  				})
			  	}
		  	}
		  	return(obj)
		  }
)

setClass(
	Class          = "VirtualPanelComputationDefinition",
	representation = representation(
	  column_names = "character",
	  name         = "character",
	  panel_comp   = "function", #Panel computation base function that is curried with arguments from the definition.
	  context_map  = "list"
	)
)

setGeneric("setContextMap",function(object,key_func){standardGeneric("setContextMap")})
setMethod("setContextMap","VirtualPanelComputationDefinition",
		  function(object,context_map){
		  	object@context_map <- context_map
		  	return(object)
		  }
)
