#Analysis module combines data from other classes and performs computations with that data.
#It produces output data for the results and possibly other objects such as plots.
#VirtualAnalysisModule defines the interface to the class.

setClass(
	Class   =   "VirtualAnalysisModule",
	representation = representation(
		base_data  = "data.frame",
		output_data= "list",
		output_objs= "list",
		"VIRTUAL"
	),
	prototype = prototype(
		base_data  = data.frame(),
		output_data= list(),
		output_objs= list()
	)
)

#Use objects passed as arguments to build the base_data frame, upon which subsequent
#computations are based.
setGeneric("generateBaseData", function(object,...){standardGeneric("generateBaseData")})

#Output data is the results of computation(s) applied to the base data and stored the 
#output data list.
#STYLE: ideally name the items in the list
setGeneric("generateOutputData", function(object,...){standardGeneric("generateOutputData")})

#Output objects are any objects that are created from the output data, for example plots 
#or custom objects.
#STYLE: ideally name the items in the list
setGeneric("generateOutputObjects", function(object,...){standardGeneric("generateOutputObjects")})

#Not designed to be called directly, but rather be parameterised in other functions
setGeneric("getItemFromAnalysis", function(object,item_type,id){standardGeneric("getItemFromAnalysis")})
setMethod("getItemFromAnalysis","VirtualAnalysisModule",
	function(object,item_type,id){
		list <- tryCatch({
					switch(item_type,
							object = object@output_objs,
							data   = object@output_data,
							stop()
						   )
				},error=function(cond){
					message(paste(class(object),":Error trying to get item type ",item_type,": ",cond,sep=""))
				})
		rval <- tryCatch({
					list[[id]]
				},error=function(cond){
					message(paste(class(object),":Error accessing item with id ",id,": ",cond,sep=""))
				})
		return(rval)
	}
)

#Parameterisation of getItemFromAnalysis to get data items
setGeneric("getDataFromAnalysis", function(object,id){standardGeneric("getDataFromAnalysis")})
setMethod("getDataFromAnalysis","VirtualAnalysisModule",function(object,id){return(getItemFromAnalysis(object,'data',id))})

#Parameterisation of getItemFromAnalysis to get object items
setGeneric("getObjectFromAnalysis", function(object,id){standardGeneric("getObjectFromAnalysis")})
setMethod("getObjectFromAnalysis","VirtualAnalysisModule",function(object,id){return(getItemFromAnalysis(object,'object',id))})

#Extension of the virtual class for the case where data is loaded from a simple base history frame
setClass(
	Class  	  = "HistoryAnalysisModule",
	representation("VIRTUAL"),
	contains  = "VirtualAnalysisModule"
)

#Input data is dataframes taken from ppmdl objects for now.
#Ultimately, should pass the whole object and then use methods of the object
#to get the correct data, eg. getTradeRows etc.
#Currently have to change the dataframe in the methods of this object, 
#but this implicit knowledge about structure and use.
#However, the object providing the data should know how that data
#can be provided and should supply methods to do it.
setMethod("generateBaseData","AllocationTurnoverByStrategy",
	function(object,history_data){
		object@base_data <- history_data
		return(object)
    }
)

