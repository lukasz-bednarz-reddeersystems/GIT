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
  slots                = c(
    analysis_class = "character",
    analysis_block = "VirtualAnalysisBlock"),
  prototype = list(
    key_cols = c("analysis_class", "id", "start", "end")

  ),
  contains = c("VirtualDataSourceClient", "VIRTUAL")
)

#To instantiate the object the analysis class slot should be set

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
setGeneric("getAnalysisBlock", function(object,...){standardGeneric("getAnalysisBlock")})

#' @describeIn getAnalysisBlock
#' Returns contained analysis block object
#'
#' @inheritParams getAnalysisBlock
#' @return \code{analysis_class} "character" name of the analysis class
#'
#' @export
setMethod("getAnalysisBlock",
          signature(object = "VirtualAnalysisObjectstoreClient"),
          function(object){
            return(object@analysis_block)
          }
)


#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualAnalysisObjectstoreClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualAnalysisObjectstoreClient'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualAnalysisObjectstoreClient", key_values = "data.frame"),
          function(object, key_values, force=FALSE){

            key <- key_values
            analysis <- getAnalysisClass(object)
            key_with_class <- cbind(data.frame(analysis_class = analysis), key_values)
            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_with_class)

            store_id <- get_analysis_objectstore_name(key_with_class,trader_col='id')

            analysis_store <- analysis_objectstore_factory(store_id)
            kh <- as.character(murmur3.32(as.character(key_values)))
            analysis_block <- queryAnalysisStore(analysis_store,data.frame(key_hash=kh,analysis_module=object@analysis_class))

            if (is.null(analysis_block)) {
              if(force){
                analysis_block <- new(analysis)
                analysis_block <- dataRequest(analysis_block,key)
                analysis_store <- updateAnalysisStore(analysis_store,analysis_block,key)
                analysis_store <- commitAnalysisStore(analysis_store)
                query_data     <- getOutputlGGPlotData(analysis_block)
              }
              else{
                stop(message(paste("No instance of",analysis,"found in store, either build it, check the key, or run with force=TRUE.")))
              }
            } else {
              query_data <- getOutputGGPlotData(analysis_block)
              object@analysis_block <- analysis_block
            }

            if (nrow(query_data) == 0) {
              query_data <- .generateDataFilledWithNA(object, trader, start, end)
            }

            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }

            # forcing new variables set
            object <- .setRequiredVariablesNames(object, colnames(query_data))
            object <- .setStoredVariablesNames(object, colnames(query_data))

            # storing Reference data internaly
            object <- setReferenceData(object, query_data)

            return(object)
          }
)

#blockRequest
