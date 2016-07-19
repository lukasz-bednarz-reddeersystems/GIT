library(plyr)
sourceTo("../features/virtual_feature.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Merge maps are lists with names that are the desired
#destination column names and values that are the column names
#in the original list.
setClass(
	Class = "VirtualAggregateFeature",
	representation = representation(
		data_store    = "character",
		store_columns = "character",
		store_mrg_on  = "list",
		trades        = "numeric",
		trade_mrg_on  = "list",
		gather_method = "nonstandardGenericFunction",
		feature_method= "nonstandardGenericFunction",
		features      = "character",
		feature_mrg_on= "list",
		setup         = "nonstandardGenericFunction"
	),
	prototype = prototype(
		store_mrg_on  = list(),
		trade_mrg_on  = list(),
		feature_mrg_on = list(),
		setup = setComputationData
	),
	contains = c("VirtualFeature")
)
setGeneric("canonicaliseColumns",function(object,trade_data){standardGeneric("canonicaliseColumns")})
setMethod("canonicaliseColumns","VirtualAggregateFeature",
	function(object,trade_data){
		all_aliases <- c(names(object@store_mrg_on),names(object@trade_mrg_on),names(object@feature_mrg_on))
		original_names <- c(unlist(object@store_mrg_on),unlist(object@trade_mrg_on),unlist(object@feature_mrg_on))
		data_names <- colnames(trade_data)
		data_locations <- match(original_names,data_names)
		data_locations <- data_locations[is.na(data_locations)==FALSE]
		name_locations <- match(data_names,original_names)
		name_locations <- name_locations[is.na(name_locations)==FALSE]
		names(trade_data)[data_locations] <- all_aliases[name_locations]
		return(trade_data)
		}
)

setGeneric("generateStoreKeys",function(object,trade_data){standardGeneric("generateStoreKeys")})
setMethod("generateStoreKeys","VirtualAggregateFeature",
	function(object,trade_data){
		#Assume that trade_data columns have already been canonicalised
		keys <- trade_data[names(object@trade_mrg_on)]
		colnames(keys) <- unlist(object@store_mrg_on[names(object@trade_mrg_on)])
		return(keys)
	}
)

#This method needs breaking up
setGeneric("gatherData",function(object,warehouse){standardGeneric("gatherData")})
setMethod("gatherData","VirtualAggregateFeature",
	function(object,warehouse){
	     if(length(object@trades)==0){
				object@trades <- as.numeric(listTrades(warehouse))
			}
			message(paste("Gathering from",length(object@trades),"trades..."))
			trd_data <- tryCatch({
			  object@gather_method(warehouse,object@trades[1])
				}, error=function(cond){
					message("Gather method failed.")
					stop()
				})
			start_on_trade <- 1
			if(length(object@features)>0 && object@features!=""){
				for(i in 1:length(object@trades)){
					feature_data <- object@feature_method(warehouse,object@trades[i],object@features)	
					#Catch case that first feature happens to be empty
					if(nrow(feature_data)>0){ 
						start_on_trade <- i+1
						break
					}
					else if(i==length(object@trades)){						
						stop("Could not initialise feature data: All features empty?")
					}
				}
			}
			for(trade in object@trades[start_on_trade:length(object@trades)]){
			  
				d <- tryCatch({
				  object@gather_method(warehouse,trade)
					}, error=function(cond){
						message("Gather method failed.")
						stop(cond)
					})
				trd_data <- tryCatch({
										rbind.fill(trd_data,d)
									 }, error=function(cond){
									 	stop(paste("Trade data bind failed:",cond))
									 })
				if(length(object@features>0) && object@features!=""){
					fdata <- tryCatch({
							object@feature_method(warehouse,trade,object@features)
						}, error = function(cond){
							message(paste("Feature method failed"))
							stop()
						})
					if(nrow(fdata)>0){
					  feature_data <- tryCatch({
													rbind.fill(feature_data,fdata)
												 }, error=function(cond){
									 				stop(paste("Feature data bind failed:",cond))
									 			 })
					  if (any(is.na(feature_data$DateTime))) {
					    
					  }
					}
				}
			}
			
			trd_data <- canonicaliseColumns(object,trd_data)
			if(length(object@data_store)>0){
				keys <- generateStoreKeys(object,trd_data)
				store_data <- data_request(object@data_store,keys,object@store_columns)
				store_data <- canonicaliseColumns(object,store_data@data)
				trd_data <- merge(trd_data,store_data,by=object@store_mrg_on)
			}
			if(length(object@features>0) && object@features!=""){
				feature_data <- canonicaliseColumns(object,feature_data)
				trd_data <- tryCatch({
					#This merge should be all.x?
					merge(trd_data,feature_data,by=names(object@feature_mrg_on))
				},error = function(cond){
					stop(paste('Merge feature data failed on',class(object)[[1]]))
				})
				
			}
			object@computation <- object@setup(object@computation,trd_data)
			return(object)
		}
)