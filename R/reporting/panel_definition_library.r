#Create FrequencyPlotPanels for all combintions of some set of 
#enumerable variables
#NB may be better to put this code in with the visualisation definition
#itself, FrequencyPlot in this case
setClass(
	Class 			= "VirtualFrequencyPanelDefinition",
	representation  = representation(
		aggregate_what = "character",
		aggregate_by   = "character",
		subset_by      = "character",
		subset_with    = "data.frame"
		aggregate_fn   = "function",
		rotate_x_labels  = 'logical',
		x_label_variable = 'character',
		y_label = 'character',
		x_label = 'character',
		visuln_comp = 'function',
		title = "character",
		enumerate = "list" #list determining which variables to enumerate. 1 element for each of the subset_by variables, with the element name
						   #set to the variable names. the list value is either the single value to appear in subset with, or the allowed
						   #values of the variables which is used for enumeration.
	),
	prototype        = prototype(
		target_class = "FrequencyPlot"
	),
	contains = c("VirtualPanelDefinition")
)

setGeneric("generateSubsetWith",function(object,ratio_values=NULL){standardGeneric("generateSubsetWith")})
setMethod("generateSubsetWith","VirtualFrequencyPanelDefinition",
		  function(object,ratio_values=NULL){
		  	return(expand.grid(object@enumerate))
		  }
)

setGeneric("createName",function(object,subset){standardGeneric("createName")})
setMethod("createName","VirtualFrequencyPanelDefinition",
		  function(object,subset){
		  	title_vars <- c()
		  	for(var in 1:length(enumerate)){
		  		if(length(var)>1)title_vars <- c(title_vars,var)
		  	}
		  	subset <- subset()
		  	data <- riffle(colnames(subset),as.character(as.matrix(subset)))
		  	return(paste(data,colllapse="_"))
		  }
)

setClass(
	Class    = "FrequencyComboPanelDefinition",
	contains = c("VirtualFrequencyPanelDefinition")
)
#Standardised method names incorporating visualisation class name, so that the report module function can 
#auto-generate the correct method name.
setGeneric("generateFrequencyComboPanelDefinitionVisualisations",function(object){standardGeneric("generateFrequencyComboPanelDefinitionVisualisations")})
setMethod("generateFrequencyComboPanelDefinitionVisualisations","FrequencyComboPanelDefinition",
		  function(object){
		  	object@subset_with <- generateSubsetWith(object)
		  	if(nrow(object@subset_with)>16)stop("Maximum number of frequency panel combinations exceeded.")
		  	setslots <- setdiff(slotNames(object),"enumerate")
		  	vslns <- list()
		  	vsln_names <- c()
		  	for(sbs in 1:nrow(object@subset_with)){
		  		slot_value_list <- list()
			  	for(slt in setslots){
			  		slot_value_list[length(slot_value_list)] <- slot(object,slt)
			  	}
			  	slot_value_list["subset_with"] <- object@subset_with[sbs,]
			  	object@subset_by <- names(object@enumerate)	
			  	vsln_names <- c(vsln_names,createName(object,object@subset_with[sbs,]))
			  	#Here the FrquencyPlot object is created
			  	vslns[length(vslns)] <- fillSlots(object,slot_value_list)
		  	}
		  	names(vslns) <- vsln_names
		  	return(vslns)		  	
		  }
)