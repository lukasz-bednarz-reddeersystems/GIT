#' @include objectstore.r
#' @include keymap.r
#' @include global_configs.r
#' @include common_composite_warehouse.r
NULL

#' An S4 class handling queries to Daily Risk Model Objectstore.
#'
#' @export

setClass(
	Class = "DailyRiskModelQuery",
	prototype = prototype(
		fields = c('lookback','name','component')
	),
	contains =c("ObjectQuery")
)

setGeneric("setRiskModelQuery",function(object,key){standardGeneric("setRiskModelQuery")})
setMethod("setRiskModelQuery","DailyRiskModelQuery",
          function(object,key){
            object <- setQueryValuesFromKey(object,key)
            return(object)
          }
)

setGeneric("updateStoredRiskModelKeys",function(object,key){standardGeneric("updateStoredRiskModelKeys")})
setMethod("updateStoredRiskModelKeys","DailyRiskModelQuery",
          function(object,key){
            object <- updateKnownKeys(object,key)
            return(object)
          }
)


#' An S4 class for storing daily risk models.
#'
#' @slot components  "character"
#' @slot risk_model_q "DailyRiskModelQuery"
#'
#' @export

setClass(
	Class = "DailyRiskModelObjectStore",
	representation  = representation(
		components  = "character",
		risk_model_q= "DailyRiskModelQuery"
	),
	prototype       = prototype(
		components  = c('ImpliedFactorReturns','ResidualReturns','Betas','FactorCorrelation','FactorVariance','MarketStyle'),
		data_path   = model_defaults@data_path,
		risk_model_q= new("DailyRiskModelQuery")
	),
	contains = c("VirtualObjectStore")
)


setGeneric("getRiskModelComponents",function(object){standardGeneric("getRiskModelComponents")})
setMethod("getRiskModelComponents","DailyRiskModelObjectStore",
          function(object){
            return(object@components)
          }
)

setGeneric("generateRiskModelKey",function(object,name,lookback,component){standardGeneric("generateRiskModelKey")})
setMethod("generateRiskModelKey","DailyRiskModelObjectStore",
		  function(object,name,lookback,component){
		  	if(!(component%in%object@components)){
		  		stop(paste("Risk model object store: Attempt to retrieve undefined risk model component",component))
		  	}
	        return(data.frame(name=name,lookback=lookback,component=component))
		  }
)

#' @export
setGeneric("queryDailyRiskModelObjectStore",function(object,name,lookback,component){standardGeneric("queryDailyRiskModelObjectStore")})
setMethod("queryDailyRiskModelObjectStore","DailyRiskModelObjectStore",
		  function(object,name,lookback,component){
		  	rm <- NULL
		  	key <- tryCatch({
		  	     generateRiskModelKey(object,name,lookback,component)
		  		   },error=function(cond){
		  		   	stop(paste('Key generation failure during risk model object store query:',cond))
		  		   })
		  	got_data <- isKeyKnown(object@risk_model_q,key)
		  	if(got_data==TRUE){
		  		message("Key found in store.")
		  		object@risk_model_q <- setRiskModelQuery(object@risk_model_q,key)
		  		name <- getIdentifier(object@risk_model_q)
	      	    rm   <- getFromObjectStore(object,name)
		  	}
		  	#If data not found, call the update method
		  	return(rm)
		  }
)

setGeneric("pushRiskModelComponent",function(object,data,name,lookback,component,force=FALSE){standardGeneric("pushRiskModelComponent")})
setMethod("pushRiskModelComponent","DailyRiskModelObjectStore",
		  function(object,data,name,lookback,component){
			  key    <- generateRiskModelKey(object,name,lookback,component)
		  	#Ultimately, each component could be an object so that we know that the correct columns are
		  	#present etc... or use reference data objects here...
		  	if(class(data)[[1]]!='data.frame'){
		  		stop(paste("Risk model object store requires component",component,"to be a data frame."))
		  	}
		    object   <- updateDailyRiskModelObjectStore(object,key,data,force)
		    return(object)
		  }
)

setGeneric("updateDailyRiskModelObjectStore",function(object,key,data,force=FALSE){standardGeneric("updateDailyRiskModelObjectStore")})
setMethod("updateDailyRiskModelObjectStore","DailyRiskModelObjectStore",
		  function(object,key,data,force=FALSE){
		  	if(class(data)[[1]]!='data.frame')stop("RiskModel component must be a data frame.")
		  	object@risk_model_q <- setRiskModelQuery(object@risk_model_q,key)
		    object@risk_model_q <- updateStoredRiskModelKeys(object@risk_model_q,key)
		  	name <- getIdentifier(object@risk_model_q)
	      	data_set <- getFromObjectStore(object,name)
	      	data_set <- initialiseOrAppendData(data_set,data)
		  	object <- placeInObjectStore(object,data_set,name)
       	  	return(object)
		  }
)

setGeneric("getRiskModelQueryID",function(object){standardGeneric("getRiskModelQueryID")})
setMethod("getRiskModelQueryID","DailyRiskModelObjectStore",
	      function(object){
	      	return(paste(object@id,"_objectquery",sep=""))
	      }
)

setGeneric("commitDailyRiskModelObjectStore",function(object){standardGeneric("commitDailyRiskModelObjectStore")})
setMethod("commitDailyRiskModelObjectStore","DailyRiskModelObjectStore",
		  function(object){
		  	object <- placeInObjectStore(object,object@risk_model_q,getRiskModelQueryID(object))
		  	saveObject(object)
		  }
)

setGeneric("getRiskModelComponentOnDate",function(object,name,component,date,lookback=150){standardGeneric("getRiskModelComponentOnDate")})
setMethod("getRiskModelComponentOnDate","DailyRiskModelObjectStore",
		  function(object,name,component,date,lookback=150){
		    risk_comp <- queryDailyRiskModelObjectStore(object,name,lookback,component)
		  	if(component%in%c('ImpliedFactorReturns','ResidualReturns','Betas')){
		  		rdata <- risk_comp@data[risk_comp@data$Date>=(as.Date(date)-lookback)&risk_comp@data$Date<=as.Date(date),]
		  	}
		  	else{
		  		rdata <- risk_comp@data[risk_comp@data$Date==as.Date(date),]
		  	}
		  	if(nrow(rdata)==0){
		  		message(paste("No risk model data found for component",component,'on date',date))
		  	}
		  	return(rdata)
		  }
)


setGeneric("getMostRecentRiskModelDate",function(object,name,lookback=150){standardGeneric("getMostRecentRiskModelDate")})
setMethod("getMostRecentRiskModelDate","DailyRiskModelObjectStore",
		  function(object,name,lookback=150){
		  	comp <- queryDailyRiskModelObjectStore(object,name,lookback,'FactorCorrelation')
		  	if(nrow(comp@data)>0){
		  		date <- max(comp@data$Date)
		  	}
		  	else{
		  		date <- NULL
		  	}
		  	return(date)
		  }
)

setGeneric("getMostRecentRiskModelBetasDate",function(object,name,lookback=150){standardGeneric("getMostRecentRiskModelBetasDate")})
setMethod("getMostRecentRiskModelBetasDate","DailyRiskModelObjectStore",
          function(object,name,lookback=150){
            comp <- queryDailyRiskModelObjectStore(object,name,lookback,'Betas')
            if(nrow(comp@data)>0){
              date <- max(comp@data$Date)
            }
            else{
              date <- NULL
            }
            return(date)
          }
)


setGeneric("reInitializeRiskModelComponents",function(object, name, lookback = 150,
                                           components = NULL){standardGeneric("reInitializeRiskModelComponents")})
setMethod("reInitializeRiskModelComponents",
          signature(object = "DailyRiskModelObjectStore", name = "character", lookback = "numeric"),
          function(object, name, lookback = 150, components = NULL){

          if (is.null(name)) {
            name <- getID(object)
          }

          if (is.null(components)) {
            components <- object@components
          }

          for(component in components){
             key    <- generateRiskModelKey(object,name,lookback,component)
             object@risk_model_q <- setRiskModelQuery(object@risk_model_q,key)
             object@risk_model_q <- updateStoredRiskModelKeys(object@risk_model_q,key)
             #Need to handle differences in key columns more formally
             if(component%in%c('ResidualReturns','Betas')){
               ds <- new("DataSet",key_cols=c('Date','Instrument'),unique_rows=TRUE,indexed=TRUE)
             }
             else{
               ds <- new("DataSet",key_cols=c('Date'),unique_rows=TRUE,indexed=TRUE)
             }
             object <- placeInObjectStore(object,ds,getIdentifier(object@risk_model_q))
          }

            return(object)
          })



setGeneric("copyRiskModelHistory",function(object, source_rmstr, name_in_source, date, lookback = 150,
                                          cmp_to_update = c('ImpliedFactorReturns','ResidualReturns','Betas','FactorCorrelation','FactorVariance','MarketStyle'),
                                          force = FALSE){standardGeneric("copyRiskModelHistory")})
setMethod("copyRiskModelHistory",
          signature(object = "DailyRiskModelObjectStore", source_rmstr = "DailyRiskModelObjectStore", name_in_source = "character", date = "Date", lookback = "numeric"),
          function(object, source_rmstr, name_in_source, date, lookback = 150,
                  cmp_to_update = c('ImpliedFactorReturns','ResidualReturns','Betas','FactorCorrelation','FactorVariance','MarketStyle'),
                  force = FALSE) {

            updated = FALSE
            max_source_stored_date <- getMostRecentRiskModelDate(source_rmstr,getID(source_rmstr),lookback)

            for (cmp_name in cmp_to_update) {
              if (cmp_name %in% c('ImpliedFactorReturns','ResidualReturns','Betas')) {
                cmp_data <- getRiskModelComponentOnDate(source_rmstr,name_in_source,cmp_name,date,lookback)
                obj_curr_cmp_data <- getRiskModelComponentOnDate(object,getID(object),cmp_name,date,lookback)

                max_stored_date <- (-Inf)
                if (!is.null(obj_curr_cmp_data) & nrow(obj_curr_cmp_data) > 0) {
                  max_stored_date <- max(obj_curr_cmp_data$Date)

                }
                if ((max_stored_date > (date - lookback)) || (length(max_stored_date) > 0)){
                  if (!is.null(max_stored_date)) {
                    cmp_data <- cmp_data[cmp_data$Date > max_stored_date,]
                  }
                }

              } else {
                cmp_data <- getRiskModelComponentOnDate(source_rmstr,name_in_source,cmp_name,max_source_stored_date,lookback)

              }

              if (nrow(cmp_data) > 0 ){
                updated = TRUE
                object <- pushRiskModelComponent(object,cmp_data,getID(object),lookback,cmp_name, force)
              }

            }
            if (updated) commitDailyRiskModelObjectStore(object)
            return(object)
          })


risk_model_objectstore_factory <- function(name,lookback=150){
	message("Initialising risk model store...")
	rmstr <- new("DailyRiskModelObjectStore",id=name)
	pth <- getPath(rmstr)
	if(file.exists(pth)){
		message(paste("Found risk model store at",pth))
		rmstr <- loadObject(rmstr)
		rmstr@risk_model_q <- getFromObjectStore(rmstr,getRiskModelQueryID(rmstr))
	}
	else{
		for(component in rmstr@components){
			key    <- generateRiskModelKey(rmstr,name,lookback,component)
			rmstr@risk_model_q <- setRiskModelQuery(rmstr@risk_model_q,key)
			rmstr@risk_model_q <- updateStoredRiskModelKeys(rmstr@risk_model_q,key)
			#Need to handle differences in key columns more formally
			if(component%in%c('ResidualReturns','Betas')){
				ds <- new("DataSet",key_cols=c('Date','Instrument'),unique_rows=TRUE,indexed=TRUE)
			}
			else{
				ds <- new("DataSet",key_cols=c('Date'),unique_rows=TRUE,indexed=TRUE)
			}
			rmstr <- placeInObjectStore(rmstr,ds,getIdentifier(rmstr@risk_model_q))
		}
		message(paste("No previous store data found at",pth,"new store created."))
	}
	return(rmstr)
}

get_risk_model_component <- function(model_prefix,date,component,lookback=150){
	name  <- paste(model_prefix,format(date,'%Y-%m'),sep="_")
	rmstr <- risk_model_objectstore_factory(name,lookback)
	cmp <- queryDailyRiskModelObjectStore(rmstr,name,lookback,component)
	cmp <- cmp@data
	if(nrow(cmp)==0)message("Query returned no data: Has this risk model objectstore been built?")
	return(cmp)
}

set_risk_model_component <- function(model_prefix,date,component,data,lookback=150){
	name  <- paste(model_prefix,format(date,'%Y-%m'),sep="_")
	rmstr <- risk_model_objectstore_factory(name,lookback)
	rmstr <- pushRiskModelComponent(rmstr,data,name,lookback,component)
	commitDailyRiskModelObjectStore(rmstr)
	message("Changes commited")
}

get_risk_model_component_on_date <- function(model_prefix,date,component,lookback=150){
	name  <- paste(model_prefix,format(date,'%Y-%m'),sep="_")
	rmstr <- risk_model_objectstore_factory(name,lookback)
	cmp <- getRiskModelComponentOnDate(rmstr,name,component,date,lookback)
	if(nrow(cmp))message(paste("No data was found for risk model on date",date))
	return(cmp)
}

#' Get most recent risk model store
#'
#' Finds most recent risk model objectstore containg model
#' on querried date or earlier
#'
#' @param model_prefix character
#' @param date Date date for model
#' @param lookback integer lookback of model in days
#' @return \code{rm_str} if risk model objectstore found returns
#' object of class "DailyRiskModelObjectStore" otherwise NULL
#' @export

get_most_recent_model_objectstore <- function(model_prefix,date = today()-1,  lookback = 150) {

  date <- as.Date(date)
  months <- unique(format(seq(from = date, by = -1, length.out = 150),'%Y-%m' ))
  retv <- NULL

  for (month in months) {
    rm_name  <- paste(model_prefix,month,sep="_")
    rmstr <- new("DailyRiskModelObjectStore",id=rm_name)
    pth <- getPath(rmstr)

    if(file.exists(pth) ){
      message(paste("Found risk model store at",pth))
      rmstr <- loadObject(rmstr)
      rmstr@risk_model_q <- getFromObjectStore(rmstr,getRiskModelQueryID(rmstr))
      rm_date_last <- getMostRecentRiskModelDate(rmstr,rm_name,lookback)

      if (length(rm_date_last)==0) {
        next()
      } else {
        retv <- rmstr
        break()
      }
    }
  }

  return(retv)

}



