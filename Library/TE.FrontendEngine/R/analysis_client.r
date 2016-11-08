#' @include TE.FrontendEngine.r
NULL

#############################################
#
# AnalysisClient Class
# Stores instances of the AnalysisBlock class
#
#############################################

#' Clas wrapping access to Analysis block objectstore
#'
#' Implements methods to access analysis objectstore
#' for specific Analysis classes
#'
#' Inherits from : "VirtualDataSourceClient"
#'
#' @slot analysis_class "character" name of the class of the analyzer
#' @slot analysis_block object of class "VirtualAnalysisBlock"

setClass(
  Class                = "VirtualAnalysisObjectstoreClient",
  #To allow override of default actions of the block aggregator,
  #put slots in here that can hold new modificaiton functions
  slots                = c(
    analysis_class = "character",
    analysis_block = "VirtualAnalysisBlock",
    aggregate_type = "character"),
  prototype = list(
    key_cols = c("analysis_class", "TraderID", "start", "end"),
    aggregate_type = "AggregateAnalysisBlock"
  ),
  contains = c("VirtualDataSourceClient",
               "VirtualTradeIDKeyExpander",
               "VIRTUAL")
)

# To instantiate the object the analysis class slot should be set

#' Returns name of the underlying analysis block class.
#'
#' @param object : object of type "VirtualAnalysisObjectstoreClient"
#' @export
setGeneric("getAnalysisClass", function(object){standardGeneric("getAnalysisClass")})

#' @describeIn getAnalysisClass
#' Returns name of the underlying analysis block class.
#'
#' @inheritParams getAnalysisClass
#' @return \code{analysis_class} "character" name of the analysis class
#'
#' @export
setMethod("getAnalysisClass",
          signature(object = "VirtualAnalysisObjectstoreClient"),
          function(object){
            return(object@analysis_class)
          }
)

#' Returns contained analysis block object
#'
#' @param object : object of type "VirtualAnalysisObjectstoreClient"
#' @export
setGeneric("getAnalysisBlock", function(object){standardGeneric("getAnalysisBlock")})

#' @describeIn getAnalysisBlock
#' Returns contained analysis block object
#'
#' @inheritParams getAnalysisBlock
#' @return \code{analysis_block} object of class "VirtualAnalysisBlock"
#'
#' @export
setMethod("getAnalysisBlock",
          signature(object = "VirtualAnalysisObjectstoreClient"),
          function(object){
            return(object@analysis_block)
          }
)


#' Sets contained analysis block object to new value
#'
#' @param object : object of type "VirtualAnalysisObjectstoreClient"
#' @param analysis_block object of class "VirtualAnalysisBlock"
#'
#' @return \code{object} object of type "VirtualAnalysisObjectstoreClient"
setGeneric(".setAnalysisBlock", function(object, analysis_block){standardGeneric(".setAnalysisBlock")})

setMethod(".setAnalysisBlock",
          signature(object = "VirtualAnalysisObjectstoreClient",
                    analysis_block = "VirtualAnalysisBlock"),
          function(object, analysis_block){
            object@analysis_block <- analysis_block

            return(object)
          }
)


#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualAnalysisObjectstoreClient'.
#' @param key_values "data.frame" with keys specifying data query.
#' @param force "logical" should the block be computed for given keys if not present in store.
#' @param replace "logical" should the block be replaced by new computed value
#' @return \code{object} object of class 'VirtualAnalysisObjectstoreClient'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualAnalysisObjectstoreClient",
                    key_values = "data.frame"),
          function(object, key_values, force=FALSE, replace = FALSE){
            #If a key is passed in without a TraderID column it will be expanded to
            #query for all available trader IDs.
            #IF the column is present, the query is not expanded.
            object <- expandKeys(object,key_values)
            key_values <- getExpanderKeys(object)

            analysis <- getAnalysisClass(object)
            key_with_class <- cbind(data.frame(analysis_class = analysis), key_values)

            colnames(key_with_class) <- getDataSourceQueryKeyColumnNames(object)
            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_with_class)

            if(hasQueryBeenExpanded(object)){
              object <- tryCatch({
                            .setAnalysisBlock(object, new(paste("Aggregate",analysis,sep="")))
                        }, error = function(cond){
                            message("No customised aggregate class defined for",analysis,"using default",object@aggregate_type)
                            return(.setAnalysisBlock(object, new(object@aggregate_type)))
                        })
              for(id in unique(key_values[[getValueName(object)]])){
                key <- key_with_class[key_with_class[[getValueName(object)]]==id,]
                object <- .updateAndAggregate(object,key,force,replace)
              }
              object <- .modifyVisualisationProperties(object)
            } else {
              object <- .updateAndAggregate(object,key_with_class,force,replace)
            }

            object <- .finalizeClientQuery(object)

            return(object)
          }
)

#' Updates and aggregates data as appropriate depending on whether the request query was expanded
#'
#' @param object object of type "VirtualAnalysisObjectstoreClient"
#' @param object key_with_class of type "data.frame"
#' @param object force of type logical
#' @param object replace of type logical
#' @return \code{object} object of type "VirtualAnalysisObjectstoreClient"
setGeneric(".updateAndAggregate", function(object, key_with_class, force, replace){standardGeneric(".updateAndAggregate")})

setMethod(".updateAndAggregate",
          signature(object = "VirtualAnalysisObjectstoreClient",
                    key_with_class = "data.frame",
                    force = "logical",
                    replace = "logical"),
          function(object, key_with_class, force, replace){

            analysis <- getAnalysisClass(object)

            store_id <- get_analysis_objectstore_name(key_with_class,trader_col=colnames(key_with_class)[2])

            analysis_store <- analysis_objectstore_factory(store_id)

            store_key <- TE.DataAccess:::key_from_analysis_objectstore_name(store_id)

            analysis_block <- queryAnalysisStore(analysis_store,store_key)

            # This is temporary incase of access of older store files where
            # there is a bug in a way that kh is generated leading to non-unique hashes as
            # applying as.character to data.frame will map
            if (is.null(analysis_block)) {
              key_values <- key_with_class[setdiff(colnames(key_with_class),'analysis_class')]
              kh <- as.character(murmur3.32(as.character(key_values)))
              analysis_block <- queryAnalysisStore(analysis_store,data.frame(key_hash=kh,analysis_class=object@analysis_class))
              kh <- hash_data_frame(key_values, algo = "murmur32")
            }

            analysis_key <- key_with_class[setdiff(colnames(key_with_class), "analysis_class")]
            if (is.null(analysis_block) || replace) {
              if(force || replace){
                analysis_block <- new(analysis)
                analysis_block <- dataRequest(analysis_block,analysis_key)
                analysis_block <- Process(analysis_block)
                analysis_store <- updateAnalysisStore(analysis_store,analysis_block,store_key, TRUE)
                analysis_store <- commitAnalysisStore(analysis_store)
                object <- .setOrAggregateAnalysisBlock(object,analysis_block,analysis_key)
              }
              else{
                stop(message(paste("No instance of",analysis,"found in store, either build it, check the key, or run with force=TRUE.")))
              }
            } else {
              object <- .setOrAggregateAnalysisBlock(object, analysis_block,analysis_key)
            }

            #Take visualisations settings from final block, which should be the same class as the others
            ggplot <- getOutputGGPlot(analysis_block)
            existing_analysis_block <- getAnalysisBlock(object)
            if(length(ggplot)>0){
              existing_analysis_block <- TE.AnalysisClasses:::.setOutputGGPlot(existing_analysis_block,ggplot)  
            }
            ui_options <- getOutputFrontendData(analysis_block)
            if(length(ui_options)>0){
              existing_analysis_block <- TE.AnalysisClasses:::.setOutputFrontendData(existing_analysis_block,ui_options)  
            }
            object <- .setAnalysisBlock(object,existing_analysis_block)

            query_data <- getOutputGGPlotData(analysis_block)
            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", store_id, "returned zero row data.frame"))
              stop(paste("Query sent to", store_id, "returned zero row data.frame"))
            }
            return(object)
          }
)

#' sets or aggregates data depending on whether the request query was expanded
#'
#' @param object object of type "VirtualAnalysisObjectstoreClient"
#' @param object analysis_block of type "VirtualAnalysisBlock"
#' @param object key of type "data.frame"
#' @return \code{object} object of type "VirtualAnalysisObjectstoreClient"
setGeneric(".setOrAggregateAnalysisBlock", function(object, analysis_block, key){standardGeneric(".setOrAggregateAnalysisBlock")})

setMethod(".setOrAggregateAnalysisBlock",
          signature(object = "VirtualAnalysisObjectstoreClient",
                    analysis_block = "VirtualAnalysisBlock",
                    key = "data.frame"),
          function(object, analysis_block, key){

            if(!hasQueryBeenExpanded(object)){
              object <- .setAnalysisBlock(object, analysis_block)
            } else {
              existing_analysis_block <- getAnalysisBlock(object)
              if((class(existing_analysis_block)[[1]]!=object@aggregate_type)&&(class(existing_analysis_block)[[1]]!=paste("Aggregate",getAnalysisClass(object),sep=""))){
                stop("Type error when trying to aggregate blocks.")
              }
              aggregate_on_id <- unique(key[getValueName(object)])
              if(nrow(aggregate_on_id)>1)stop("Multiple keys when trying to aggregate blocks.")
              query_data <- getOutputGGPlotData(analysis_block)
              existing_analysis_block <- aggregateAnalysisData(existing_analysis_block,query_data,aggregate_on_id)
              object <- .setAnalysisBlock(object, existing_analysis_block)
            }
            return(object)
          }
)

#' modify the visualisation properties of the module
#'
#' @param object object of type "VirtualAnalysisObjectstoreClient"
#' @return \code{object} object of type "VirtualAnalysisObjectstoreClient"
setGeneric(".modifyVisualisationProperties", function(object){standardGeneric(".modifyVisualisationProperties")})

setMethod(".modifyVisualisationProperties",
          signature(object = "VirtualAnalysisObjectstoreClient"),
          function(object){
            existing_analysis_block <- getAnalysisBlock(object)
            existing_analysis_block <- modifyGGPlotObject(existing_analysis_block)
            existing_analysis_block <- modifyGGPlotFormat(existing_analysis_block)
            existing_analysis_block <- modifyUIOptions(existing_analysis_block)
            object <- .setAnalysisBlock(object, existing_analysis_block)
            return(object)
          }
)

#' finalise data
#'
#' @param object object of type "VirtualAnalysisObjectstoreClient"
#' @return \code{object} object of type "VirtualAnalysisObjectstoreClient"
setGeneric(".finalizeClientQuery", function(object){standardGeneric(".finalizeClientQuery")})

setMethod(".finalizeClientQuery",
          signature(object = "VirtualAnalysisObjectstoreClient"),
          function(object){
            existing_analysis_block <- getAnalysisBlock(object)
            query_data <- getOutputGGPlotData(existing_analysis_block)
            object <- TE.RefClasses:::.setRequiredVariablesNames(object, colnames(query_data))
            object <- TE.RefClasses:::.setStoredVariablesNames(object, colnames(query_data))
            object <- setReferenceData(object, query_data)
            return(object)
          }
)

