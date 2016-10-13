#' @include keymap.r
#' @include global_configs.r
#' @include common_composite_warehouse.r
#' @include features_preprocessor_library.r
NULL

#Apply model computation to the data aggregated from
#all preprocessors
# @exportClass NullableTradeWarehouse
setClassUnion("NullableTradeWarehouse",c("NULL","TradeWarehouse"))
# @exportClass NullableDataSet
setClassUnion("NullableDataSet",c("NULL","DataSet"))
setClass(
	Class = "PPModelComputation",
	representation = representation(
		wh         = "NullableTradeWarehouse",
		ppdata     = "NullableDataSet",
		compute    = "function",
		clear_input= "logical"
	),
	prototype      = prototype(
		clear_input=TRUE
	)
)

setGeneric("setModelComputationData",function(object,data,warehouse){standardGeneric("setModelComputationData")})
setMethod("setModelComputationData","PPModelComputation",
	      function(object,data,warehouse){
	      	object@wh <- warehouse
	      	object@ppdata <- data
	      	return(object)
	      }
)

setGeneric("triggerModelComputation",function(object){standardGeneric("triggerModelComputation")})
setMethod("triggerModelComputation","PPModelComputation",
	      function(object){
	      	object <- tryCatch({
	      			object@compute(object)
	      		},error=function(cond){
	      			message(paste(class(object)[[1]],"computation failure:",cond))
	      		    return(object)
	      		})
	      	if(object@clear_input){
	      		object@wh <- NULL
	      		object@ppdata <- NULL
	      	}
	      	return(object)
	      }
)

pass_thru <- function(x){return(x)}

setClass(
	Class = "PPModelSetupComputation",
	representation = representation(
		output     = "NullableTradeWarehouse"
	),
	contains = c("PPModelComputation")
)
setClass(
	Class = "PPModelSetupPassThruComputation",
	prototype = prototype(
		compute = pass_thru
	),
	contains = c("PPModelSetupComputation")
)

setClass(
	Class = "PPModelPostComputation",
	representation = representation(
		output     = "NullableDataSet"
	),
	contains = c("PPModelComputation")
)
setClass(
	Class = "PPModelPostPassThruComputation",
	prototype = prototype(
		compute = pass_thru
	),
	contains = c("PPModelPostComputation")
)

setClass(
	Class = "PPModelSummaryComputation",
	representation = representation(
		output     = "NullableDataSet"
	),
	contains = c("PPModelComputation")
)
setClass(
	Class = "PPModelSummaryPassThruComputation",
	prototype = prototype(
		compute = pass_thru
	),
		contains = c("PPModelSummaryComputation")
)

#' An S4 class handling batch querries.
#'
#' @export

setClass(
	Class          = "BatchQuery",
	representation = representation(
		batch_keys = "data.frame",
		n_keys     = "numeric",
		all_run    = "logical",
		current_key= "numeric",
		sort_on    = "character"
	),
	prototype      = prototype(
		all_run    = FALSE,
		current_key= 1
	),
	contains       = c("ObjectQuery")
)

setGeneric("resetQuery",function(object){standardGeneric("resetQuery")})
setMethod("resetQuery","BatchQuery",
		  function(object){
		  	object@n_keys <- nrow(object@batch_keys)
		  	object@all_run <- FALSE
		  	object@current_key <- 1
		  	if(length(object@sort_on)>0)object@batch_keys <- object@batch_keys[order(object@batch_keys[object@sort_on]),]
		  	object@values <- as.character(object@batch_keys[object@current_key,])
		  	return(object)
		  }
)

setGeneric("setBatchKeys",function(object,keys){standardGeneric("setBatchKeys")})
setMethod("setBatchKeys","BatchQuery",
		  function(object,keys){
		  	if(!identical(object@fields,colnames(keys)))stop(paste("Batch key columns must match fields:",paste(object@fields,collapse=", ")))
		  	object@batch_keys <- keys
		  	object <- resetQuery(object)
		  	return(object)
		  }
)

setGeneric("advanceBatchKey",function(object){standardGeneric("advanceBatchKey")})
setMethod("advanceBatchKey","BatchQuery",
	      function(object){
	      	if(!object@all_run){
	      		if(object@current_key < object@n_keys){
	      			object@current_key <- object@current_key + 1
	      			object@values <- as.character(object@batch_keys[object@current_key,])
	      		}
	      		else{
	      			object@all_run <- TRUE
	      		}
	      	}
	      	else{
	      		message("All batch key values used.")
	      	}
	      	return(object)
	      }
)

setGeneric("hasBatchRun",function(object){standardGeneric("hasBatchRun")})
setMethod("hasBatchRun","BatchQuery",
		  function(object){
		  	return(object@all_run)
		  }
)

setGeneric("getCurrentKeyNumber",function(object){standardGeneric("getCurrentKeyNumber")})
setMethod("getCurrentKeyNumber","BatchQuery",
		  function(object){
	      	return(object@current_key)
		  }
)

setGeneric("getCurrentKey",function(object){standardGeneric("getCurrentKey")})
setMethod("getCurrentKey","BatchQuery",
	      function(object){
	      	return(object@batch_keys[object@current_key,])
	      }
)

setGeneric("getAllKeys",function(object){standardGeneric("getAllKeys")})
setMethod("getAllKeys","BatchQuery",
	      function(object){
	      	return(object@batch_keys)
	      }
)

setClass(
	Class     = "WarehouseBatchQuery",
	prototype = prototype(
		fields = c('id','start','end'),
		sort_on= 'end'
	),
	contains  = c("BatchQuery")
)



#' An S4 class to represent generic ppmodel set.
#'
#' Pre Processing Model is an object that gathers and
#' processes raw data and aggregates it into one DataSet.
#' Depending on specific derived classes different result
#' can be obtained
#'
#' @slot queries      "WarehouseBatchQuery",
#' @slot warehouse    "NullableTradeWarehouse",
#' @slot ppdata       "DataSet",
#' @slot modeldata    "DataSet",
#' @slot preprocessor "character",
#' @slot preproc_call "nonstandardGenericFunction",
#' @slot features     "character",
#' @slot post_comp    "PPModelPostComputation",
#' @slot setup_comp   "PPModelSetupComputation",
#' @slot remove_wh    "logical",
#' @slot replace_features  "logical",
#' @slot warehouse_store_name  "character",
#' @slot index_by_date_column  "character",
#' @slot summary_comp  "PPModelSummaryComputation"

setClass(
	Class   	    = "PPModel",
	representation  = representation(
		queries     = "WarehouseBatchQuery",
		warehouse   = "NullableTradeWarehouse",
		ppdata      = "DataSet",
		modeldata   = "DataSet",
		preprocessor= "character",
		preproc_call= "nonstandardGenericFunction",
		features    = "character",
		post_comp   = "PPModelPostComputation",
		setup_comp  = "PPModelSetupComputation",
		remove_wh   = "logical",
		replace_features = "logical",
		warehouse_store_name = "character",
		index_by_date_column = "character",
		summary_comp = "PPModelSummaryComputation"
	),
	prototype       = prototype(
		remove_wh   = TRUE,
		replace_features = FALSE,
		queries     = new("WarehouseBatchQuery")
	)
)

#' Initialize method for "PPModel" class
#'
#' @param .Object, object of class "PPModel"
#' @param keys "character" key names for ppmodel dataset and query
#' @return \code{.Object} object of class "PPModel"

setMethod("initialize", "PPModel",
          function(.Object,keys){
            .Object@queries <- setBatchKeys(.Object@queries,keys)
            message(paste("Created pre-processor model ",class(.Object)[[1]]," for ",nrow(keys)," key values.",sep=""))
            .Object
          }
)

setGeneric("closeWarehouse",function(object){standardGeneric("closeWarehouse")})
setMethod("closeWarehouse","PPModel",
		  function(object){
		  	if(object@remove_wh){
				object@warehouse <- NULL
			}
			return(object)
		  }
)

setGeneric("queryPreProcessorModel",function(object,key){standardGeneric("queryPreProcessorModel")})
setMethod("queryPreProcessorModel","PPModel",
		  function(object,key){
		    object@warehouse_store_name <- name_from_key(key)
		  	object <- updateWarehouse(object,key)
		  	object@setup_comp <- tryCatch({
		  								setModelComputationData(object@setup_comp,object@ppdata,object@warehouse)
		  							},error=function(cond){
		  								stop(paste("Error setting model setup computation data in",class(object)[[1]],":",cond))
		  							})
		  	object@setup_comp <- tryCatch({
		  								triggerModelComputation(object@setup_comp)
		  							},error=function(cond){
		  								stop(paste("Error in model setup computation for",class(object)[[1]],":",cond))
		  							})
		  	if(length(object@setup_comp@output)>0)object@warehouse <- object@setup_comp@output
			  object <- updateModel(object)
		  	object@post_comp <- tryCatch({
		  								setModelComputationData(object@post_comp,object@ppdata,object@warehouse)
		  							},error=function(cond){
		  								stop(paste("Error setting model post computation data in",class(object)[[1]],":",cond))
		  							})
		  	object@post_comp <- tryCatch({
		  								triggerModelComputation(object@post_comp)
		  							},error=function(cond){
		  								stop(paste("Error in model post computation for",class(object)[[1]],":",cond))
		  							})
		   if(length(object@post_comp@output)>0)object@ppdata <- object@post_comp@output
		   object <- attachDataToModel(object)
		   object <- closeWarehouse(object)
		   return(object)
		  }
)

setGeneric("attachDataToModel",function(object){standardGeneric("attachDataToModel")})
setMethod("attachDataToModel","PPModel",
		  function(object){
		  	data <- cbind(data.frame(ppModelIndex=getCurrentKeyNumber(object@queries)),object@ppdata@data)
  			if(nrow(object@modeldata@data)==0){
  				message("Initialising model data ...")
  		  		object@modeldata <- dataset_factory(c('ppModelIndex'),data)
	  		 }
	  		 else{
	  		 	message("Attaching new data...")
	  		  	object@modeldata <- tryCatch({
  		  				  aggregateData(object@modeldata,data)
  		  				},error=function(cond){
  		  					stop(paste("Could not aggregate data after",class(object)[[1]],"call:",cond))
  		  				})
  		  	 }
  		  	 object <- setppModelIndex(object)
		  	 return(object)
		 	}
)

setGeneric("setppModelIndex",function(object){standardGeneric("setppModelIndex")})
setMethod("setppModelIndex","PPModel",
	      function(object){
	        rval <- object@modeldata@data
	      	if(length(object@index_by_date_column)>0){
	      	  if(class(object@queries)[[1]]!="WarehouseBatchQuery")stop("Cant sort on key if query not of WarehouseBatchQuery type.")
	      		keys <- getAllKeys(object@queries)
	      		for(k in 1:nrow(keys)){
	      			key <- keys[k,]
	      			rval[rval[[object@index_by_date_column]]>key[['start']]&rval[[object@index_by_date_column]]<=key[['end']],'ppModelIndex'] <- k
	      		}
	      		object@modeldata <- dataset_factory(object@modeldata@key_cols,rval)
	      	}
	      	return(object)
	      }
)

#' run Pre Processor Model computation
#'
#' @param object object of class "PPModel"
#' @return \code{object} object of class "PPModel"
#' @export

setGeneric("runPreProcessorModel",function(object){standardGeneric("runPreProcessorModel")})

#' @describeIn runPreProcessorModel
#' run Pre Processor Model computation
#'
#' @inheritParams runPreProcessorModel
#' @return \code{object} object of class "PPModel"
#' @export
setMethod("runPreProcessorModel","PPModel",
		  function(object){
		    if(length(object@queries)>0){

		      while(!hasBatchRun(object@queries)){
		  			key <- getCurrentKey(object@queries)
		  			object <- queryPreProcessorModel(object,key)
		  			object@queries <- advanceBatchKey(object@queries)
		  		}
		  		object <- updateWarehouse(object,key)
		  		object@summary_comp <- tryCatch({
		  											setModelComputationData(object@summary_comp,object@modeldata,object@warehouse)
		  										},error=function(cond){
		  											stop(paste("Error setting model summary computation data in",class(object)[[1]],":",cond))
		  										})
		  		object <- closeWarehouse(object)
		  		object@summary_comp <- tryCatch({
		  											triggerModelComputation(object@summary_comp)
		  										},error=function(cond){
		  											stop(paste("Error in model summary computation for",class(object)[[1]],":",cond))
		  										})
		  		if(length(object@summary_comp@output)>0)
		  		{
		  			object@modeldata <- object@summary_comp@output
		  		}
		  	}
		  	else{
		  		message("Must set a batch query to run over multiple keys.")
		  	}
		  	return(object)
		  }
)

setGeneric("updateWarehouse",function(object,key){standardGeneric("updateWarehouse")})
setMethod("updateWarehouse","PPModel",
		  function(object,key){
		  	message(paste("Fetching data ... "))
		    object@warehouse_store_name <- name_from_key(key)
		  	object@warehouse <- warehouse_request(object@warehouse_store_name,key[['id']],key[['start']],key[['end']])
            return(object)
		  }
)

setGeneric("recomputeFeaturesForKey",function(object,key,features=NULL,push_change=NULL){standardGeneric("recomputeFeaturesForKey")})
setMethod("recomputeFeaturesForKey","PPModel",
		  function(object,key,features=NULL,push_change=NULL){
		  	if(length(features)==0)features <- object@features
		  	if(length(push_change)==0)push_change <- object@replace_features
		  	ftrs <- paste(features,collapse=", ")
		  	message(paste("Recomputing warehouse trade features:",ftrs))
		  	object <- updateWarehouse(object,key)
		  	object@warehouse <- attachFeatures(object@warehouse,features)
		  	if(push_change){
		  		message("push_change flag is TRUE, all feature changes will be saved to the warehouse.")
		  	}
		  	else{
		  		message("push_change flag is FALSE, only new features will be pushed to the warehouse.")
		  	}
		  	warehouse_push_features(object@warehouse_store_name,object@warehouse,replace_features=push_change)
		  	object <- closeWarehouse(object)
		  	return(object)
		  }
)

setGeneric("recomputeSummaryForKey",function(object,key,push_change=NULL){standardGeneric("recomputeSummaryForKey")})
setMethod("recomputeSummaryForKey","PPModel",
	      function(object,key,push_change=NULL){
			if(length(push_change)==0)push_change <- object@replace_features
			message("Recomputing warehouse summary ...")
		  	object <- updateWarehouse(object,key)
		  	tryCatch({
		  			object@warehouse <- createPositionSummary(object@warehouse)
		  		},error = function(cond){
		  			stop(paste("Call to createPositionSummary failed on key",paste(key,collapse=", "),":",cond))
		  		})
		  	if(push_change){
		  		message("push_change flag is TRUE, new summaries will be saved to the warehouse.")
		  		warehouse_push_summary(object@warehouse_store_name,object@warehouse)
		  	}
		  	else{
		  		message("push_change flag is FALSE, no changes will be pushed to the warehouse.")
		  	}
		  	object <- closeWarehouse(object)
		  	return(object)
		  }
)

setGeneric("recomputeFeaturesForAll",function(object,features=NULL,push_change=NULL){standardGeneric("recomputeFeaturesForAll")})
setMethod("recomputeFeaturesForAll","PPModel",
		  function(object,features=NULL,push_change=NULL){
		  	if(length(object@queries)>0){
		  		for(row in 1:object@queries@n_keys){
		  			object <- recomputeFeaturesForKey(object,object@queries@batch_keys[row,],features,push_change)
		  		}
		  	}
		  	else{
		  		message("Must set a batch query to recompute features over multiple keys.")
		  	}
		  	return(object)
		  }
)

setGeneric("recomputeSummaryForAll",function(object,push_change=NULL){standardGeneric("recomputeSummaryForAll")})
setMethod("recomputeSummaryForAll","PPModel",
		  function(object,push_change=NULL){
		  	if(length(object@queries)>0){
		  		for(row in 1:object@queries@n_keys){
		  			object <- recomputeSummaryForKey(object,object@queries@batch_keys[row,],push_change)
		  		}
		  	}
		  	else{
		  		message("Must set a batch query to recompute summaries over multiple keys.")
		  	}
		  	return(object)
		  }
)

setGeneric("updateModel",function(object){standardGeneric("updateModel")})
setMethod("updateModel","PPModel",
		  function(object){
		  	message(paste("Updating model:",class(object)[[1]]))
		    if(length(object@features)!=length(unique(object@features)))stop(paste("Duplicate features specified in",class(object)[[1]],"check the ppmodel definition."))
		  	if(length(object@features)!=0 && object@features!="" && length(intersect(object@warehouse@features,object@features))!=length(object@features)){
		  		message("Not all features found in warehouse.")
		  		message("Adding features ...")
		  		to_add <- setdiff(object@features,object@warehouse@features)
		  		tryCatch({
		  				object@warehouse <- attachFeatures(object@warehouse,to_add,replace_features=object@replace_features)
		  			}, error = function(cond){
		  				stop(paste("Error when attaching features to warehouse:",cond))
		  			})
		  		tryCatch({
		  				warehouse_push_features(object@warehouse_store_name,object@warehouse,replace_features=object@replace_features)
		  			}, error = function(cond){
		  				stop(paste("Error pushing feature changes to warehouse:",cond))
		  			})
		  	}
		  	message("Running preprocessor...")
		  	pp <- new(object@preprocessor)
		  	pp <- object@preprocessor_call(pp,object@warehouse,object@features)
		  	object@ppdata <- getOutPut(pp)
		  	return(object)
		  }
)

name_from_key <- function(key){
	return(paste(c(as.character(key['id'][[1]]),as.character(key['start'][[1]]),as.character(key['end'][[1]])),collapse="_"))
}

ppmodel_class_factory <- function(type,preprocessor,preprocessor_call,features,setup_computation="PPModelSetupPassThruComputation",post_computation="PPModelPostPassThruComputation",summary_computation="PPModelSummaryPassThruComputation",index_by_date_column=NULL){
	create_str <- paste("setClass(Class='",type,"',prototype=prototype(preprocessor='",preprocessor,"',",sep="")
	create_str <- paste(create_str,"preprocessor_call=",preprocessor_call,",",sep="")
	feature_str<- paste(features,collapse="','")
	create_str <- paste(create_str,"features=c('",feature_str,"')",sep="")
	if(length(setup_computation)>0)create_str <- paste(create_str,",setup_comp=new('",setup_computation,"')",sep="")
	if(length(post_computation)>0)create_str <- paste(create_str,",post_comp=new('",post_computation,"')",sep="")
	if(length(summary_computation)>0)create_str <- paste(create_str,",summary_comp=new('",summary_computation,"')",sep="")
	if(length(index_by_date_column)>0)create_str <- paste(create_str,",index_by_date_column='",index_by_date_column,"'",sep="")
	create_str <- paste(create_str,")",sep="")
	create_str <- paste(create_str,",contains=c('PPModel'))",sep="")
	tryCatch({
				getClass(type)
			},error=function(cond){
				eval(parse(text=create_str))
				message(paste("Preprocessor class",type,"created."))
			})
	return(create_str)
}


