#' @include datasource_client.r
#' @include risk_model_component.r
NULL

#########################################
#
# VirtualRiskModelObjectstoreClient Class
#
#########################################

#' List of risk model objectstore keys
risk_model_objectstore_keys <-  c("Date")

devtools::use_data(risk_model_objectstore_keys,
                   overwrite = TRUE)

#' Virtual S4 class for access to Risk Model Objectstore.
#'
#' This is handler class that is to be inherited
#' by other classes handling Risk Model Objects
#'
#' Inherits from "VirtualDataSourceClient" and "VirtualRiskModelHandler"
#'
#' @slot component    "character", name of the component of risk model

setClass(
  Class                = "VirtualRiskModelObjectstoreClient",
  slots = c(
    component          = "character" # name of component in Risk Model
    ),
  prototype = list(
    key_cols        = risk_model_objectstore_keys,
    key_values      = data.frame(Date = as.Date(character()))
  ),
  contains = c("VirtualDataSourceClient","VirtualRiskModelFactorDependentComponent", "VIRTUAL")
)



#' initialize method for "VirtualRiskModelObjectstoreClient" derived classes
#'
#' initializes required column names from the values obtained from contained risk model
#'
#' @param .Object object of class derived from "VirtualRiskModelObjectstoreClient"
#' @export

setMethod("initialize",
          "VirtualRiskModelObjectstoreClient",
          function(.Object){

            .Object <- callNextMethod()

            return(.Object)
          }
)

#' Get Risk Model Component Name
#'
#' Returns name of the component that given class is accessing
#'
#' @param object object of class 'VirtualRiskModelObjectstoreClient'.
#' @return \code{component} 'character', name of the component beeing accessed.
#' @export

setGeneric("getRiskModelObjectstoreComponentName", function(object){standardGeneric("getRiskModelObjectstoreComponentName")})

#' @describeIn getRiskModelObjectstoreComponentName
#' Get Risk Model Component Name
#'
#' Returns name of the component that given class is accessing
#'
#' @inheritParams getRiskModelObjectstoreComponentName
#' @return \code{component} 'character', name of the component beeing accessed.
#' @export
setMethod("getRiskModelObjectstoreComponentName",
          signature(object = "VirtualRiskModelObjectstoreClient"),
          function(object){
            return(object@component)
          }
)


#' Generate specific objectstore keys
#'
#' Transforms original query key values to risk model objectstore specific keys
#'
#' @rdname private_generateQueryKeyValues
#' @param object object of class 'VirtualRiskModelObjectstoreClient'.
#' @param key_values 'data.frame' with original query keys to be transformed .
#' @return \code{query_key_values} 'data.frame', transformed, component specific query keys.

setGeneric(".generateQueryKeyValues", function(object, key_values){standardGeneric(".generateQueryKeyValues")})
setMethod(".generateQueryKeyValues",
          signature(object = "VirtualRiskModelObjectstoreClient",
                    key_values = "data.frame"),
          function(object, key_values){

            query_key_values <- unique(key_values["Date"])
            query_key_values$start <- query_key_values$Date - day(query_key_values$Date )
            query_key_values$end <- (query_key_values$Date - day(query_key_values$Date ) +
                                    days_in_month(query_key_values$Date))
            query_key_values <- unique(query_key_values[c("start", "end")])

            return(query_key_values)
          }
)

setMethod(".generateDataFilledWithNA",
          signature(object = "VirtualRiskModelObjectstoreClient"),
          function(object){

            ret_vars <- getDataSourceReturnColumnNames(object)
            key_vals <- getDataSourceQueryKeyValues(object)

            diff <- setdiff(ret_vars, colnames(key_vals))

            ret_data <- cbind(key_vals, data.frame(t(rep(NA,length(diff)))))

            colnames(ret_data) <- c(ret_vars)

            return(ret_data)
          }
)


#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualRiskModelObjectstoreClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualRiskModelObjectstoreClient'.
#' @export
setMethod("dataRequest",
          signature(object = "VirtualRiskModelObjectstoreClient", key_values = "data.frame"),
          function(object, key_values){

            object <- .setDataSourceQueryKeyValues(object, key_values)

            key_values <- getDataSourceQueryKeyValues(object)

            colnames(key_values) <- .translateDataSourceColumnNames(object, colnames(key_values))

            model_prefix <- getRiskModelName(object)
            lookback     <- getRiskModelLookback(object)
            component    <- getRiskModelObjectstoreComponentName(object)

            query_key_vals <- .generateQueryKeyValues(object, key_values)
            merge_cols   <- colnames(key_values)

            first <- TRUE
            browser()
            for(key_idx in seq(nrow(query_key_vals))) {

              start        <- as.Date(query_key_vals$start[key_idx])
              end          <- as.Date(query_key_vals$end[key_idx])

              rm_str       <- get_most_recent_model_objectstore(model_prefix, end, lookback)

              if (is.null(rm_str)) {
                next
                }

              name         <- getID(rm_str)

              query_data   <- queryDailyRiskModelObjectStore(rm_str,name,lookback,component)

              query_data   <- getData(query_data)

              query_data   <- query_data[query_data$Date >= start & query_data$Date <= end, ]

              if (first) {
                ret_data <- query_data
                first <- FALSE
              }
              else {

                ret_data <- rbind(ret_data, query_data[!(query_data$Date %in% unique(ret_data$Date)), ])
              }

            }


            if (0 == nrow(ret_data)) {
              message(paste("Object", class(object), "in dataRequest() returned zero row data.frame"))
              ret_data <- .generateDataFilledWithNA(object)
            } else {

              ret_data$Index <- seq(nrow(ret_data))

              ret_data   <- merge(key_values,
                                    ret_data,
                                    all.x = TRUE, sort = FALSE)

              ret_data <- ret_data[order(ret_data$Index), setdiff(colnames(ret_data), "Index")]
            }


            # translating column names
            colnames(ret_data) <- .translateDataSourceColumnNames(object, colnames(ret_data))

            # storing Reference data internaly
            object <- setReferenceData(object, ret_data)

            return(object)
          }
)

