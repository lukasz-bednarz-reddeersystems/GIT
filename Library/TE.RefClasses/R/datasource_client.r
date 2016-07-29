#' @include referencedata.r
#' @include datasource_client_functions.r
NULL

####################################
#
# VirtualDataSourceClient Class
#
####################################

#' Virtual S4 class implementing handling of data access.
#'
#' Implements handling of access to data sources.
#' Handles queries and implements accessors to
#' internal data.
#' Inherits from "VirtualReferenceData"
#' @slot key_cols           = "character",  query keys column names
#' @slot key_values         = "data.frame",  query keys
#' @slot values             = "character",  columns that neeed to be returned from datastore
#' @slot column_name_map    = "hash",  perl-type hash containing mapping of source column names to output column names
#' @slot non_na_cols        = "character",  columns upon which we remove rows to forced to have non NA values
#' @slot factorized_cols    = "character",  factor columns that are expanded to  new columns
#' @slot factorization_keys = "character", key columns that are used as a unique keys in factorization

setClass(
  Class                = "VirtualDataSourceClient",
  slots = c(
    key_cols           = "character", # query keys column names
    key_values         = "data.frame", # query keys
    values             = "character", # columns that neeed to be returned from datastore
    column_name_map    = "hash", # perl-type hash containing mapping of source column names to output column names
    non_na_cols        = "character", # columns upon which we remove rows to forced to have non NA values
    factorized_cols    = "character", # factor columns that are expanded to  new columns
    factorization_keys = "character" # key columns that are used as a unique keys in factorization
  ),
  prototype            = list(
    non_na_cols        = character(),
    factorized_cols    = character(),
    factorization_keys = character()
  ),

  contains = c("VirtualReferenceData", "VIRTUAL")
)

#' Runs necessary postprocessing routines
#'
#' Private method to handle cleaning of raw data from datasource
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{object} object of class 'VirtualDataSourceClient'.

setGeneric(".PostProcessResultsData", function(object,...){standardGeneric(".PostProcessResultsData")})
setMethod(".PostProcessResultsData",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object)
          }
)


#' Fills missing data with NA's
#'
#' Private method to fill data for missing keys with NA's
#' has to be implemented in derived classes if it is to be used
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{object} object of class 'VirtualDataSourceClient'.

setGeneric(".generateDataFilledWithNA", function(object, trader, start, end,...){standardGeneric(".generateDataFilledWithNA")})



#' Get query key column names
#'
#' Private method to return key column names
#' that are used to query raw data source
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{key_cols} character vector with list of key column names
#' @export

setGeneric("getDataSourceQueryKeyColumnNames", function(object,...){standardGeneric("getDataSourceQueryKeyColumnNames")})
setMethod("getDataSourceQueryKeyColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@key_cols)
          }
)

#' Get returned source data column names
#'
#' Returns names of the columns that will be returned from data source
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{values} character vector with list of returned source column names
#' @export

setGeneric("getDataSourceReturnColumnNames", function(object,...){standardGeneric("getDataSourceReturnColumnNames")})
setMethod("getDataSourceReturnColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@values)
          }
)

#' Get non - NA's column names
#'
#' Returns column names that are required to have non-na values
#' rows where these columns have NA's will be removed
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{non_na_cols} character vector with list of returned column names
#' @export

setGeneric("getNonNAColumnNames", function(object,...){standardGeneric("getNonNAColumnNames")})
setMethod("getNonNAColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@non_na_cols)
          }
)


#' Are any non - NA's column names defined in object
#'
#' Returns true if object has columns that are required to have non-na-values
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{has_non_na_cols} logical
#' @export

setGeneric("hasNonNAColumnNames", function(object,...){standardGeneric("hasNonNAColumnNames")})
setMethod("hasNonNAColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(length(object@non_na_cols) > 0)
          }
)


#' Store values of query keys
#'
#' Private method to set values of key values datatable
#' with keys that will be matched in datastore query
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @param key_values data.frame with columns matching key column names()
#' @return \code{object} object of class 'VirtualDataSourceClient'.

setGeneric(".setDataSourceQueryKeyValues", function(object, key_values, ...){standardGeneric(".setDataSourceQueryKeyValues")})
setMethod(".setDataSourceQueryKeyValues",
          signature(object = "VirtualDataSourceClient", key_values = "data.frame"),
          function(object, key_values) {

            key_cols <- getDataSourceQueryKeyColumnNames(object)

            if (! has_required_columns(key_values, key_cols)) {
              message(paste("Object", class(object), "in .setDataSourceQueryKeyValues()"))
              message(paste("Query key_values set do not contain required column names"))
              message(paste("Required columns are: ", paste0(key_cols, collapse = ", ")))
              stop("Invalid column names of query keys passed to .setDataSourceQueryKeyValues().")
            } else if (0 == nrow(key_values)) {
              message(paste("Object", class(object), "in .setDataSourceQueryKeyValues()"))
              message(paste("Query key_values has zero rows"))
              stop("Zero row query keys data.frame passed to .setDataSourceQueryKeyValues().")
            }
            else {


              object@key_values <- key_values[key_cols]
              return(object)
            }
          }
)


#' Retreive values of query keys
#'
#' Get values of key values datatable
#' with keys used to  match in datasource query
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{has_non_na_cols} logical
#' @export

setGeneric("getDataSourceQueryKeyValues", function(object,...){standardGeneric("getDataSourceQueryKeyValues")})
setMethod("getDataSourceQueryKeyValues",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@key_values)
          }
)


#' Retreive datasource column name map
#'
#' Returns hash with mapping of datasource column names
#' to reference data class column names
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{column_name_map} hash column name map of the datasource to output names
#' @export

setGeneric("getDataSourceClientColumnNameMap", function(object,...){standardGeneric("getDataSourceClientColumnNameMap")})
setMethod("getDataSourceClientColumnNameMap",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@column_name_map)
          }
)


#' Translate datasource column names
#'
#' Private method to translate raw data column names
#' to required output names
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @param colnames character vector with list of raw data column names
#' @return \code{ret_colnames} data source column names translated to
#' output column names as required by "required Variables"

setGeneric(".translateDataSourceColumnNames", function(object, colnames, ...){standardGeneric(".translateDataSourceColumnNames")})
setMethod(".translateDataSourceColumnNames",
          signature(object = "VirtualDataSourceClient", colnames = "character"),
          function(object, colnames){

            colnames_map <- getDataSourceClientColumnNameMap(object)

            names_to_translate <- intersect(colnames, names(colnames_map))
            idx <- (colnames %in% names_to_translate)

            ret_colnames <- colnames
            ret_colnames[idx] <- values(colnames_map[names_to_translate])[names_to_translate]

            return(ret_colnames)
          }
)


#' Remove rows with na's
#'
#' Private method to remove rows in reference data
#' where non-NA column names are NA
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{object} object of class 'VirtualDataSourceClient'.

setGeneric(".removeNAReferenceData", function(object,...){standardGeneric(".removeNAReferenceData")})
setMethod(".removeNAReferenceData",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            non_na_cols <- getNonNAColumnNames(object)
            ref_data <- getReferenceData(object)

            is_na <- rep(FALSE, nrow(ref_data))

            is_na <- tryCatch({
              for (col in non_na_cols) {
                is_na <- is_na | (is.na(ref_data[col]) & !all(is.na(ref_data[col])))
              }

              is_na
            }, error = function(cond) {
              stop(paste("Error ocured when removing NA's in .removeNAReferenceData()",cond) )
            }
            )

            if (any(is_na)){
              object <- setReferenceData(object, ref_data[!is_na,])
            }

            return(object)
          }
)


#' Request data from data source
#'
#' Generic method to request data from data source.
#' Needs to be implemented in derived classes to work
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'VirtualDataSourceClient'.
#' @export

setGeneric("dataRequest", function(object, key_values, ...){standardGeneric("dataRequest")})


#' Get factorized columns names
#'
#' Returns names of columns that are going to be factorized.
#' Factorization is unstacking operation where individual
#' factor values of factorized columns are turned into separate
#' set of columns spanning factor levels with logical values
#' indicating if given factor is present for unique set of
#' remaining factorization key columns
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{factorized_cols} character vector with list of factorized column names
#' @export

setGeneric("getFactorColumnNames", function(object,...){standardGeneric("getFactorColumnNames")})
setMethod("getFactorColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@factorized_cols)
          }
)

#' Are there any columns to  factorize ?
#'
#' Returns TRUE if there are some columns to factorize
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{has_factorized_cols} logical
#' @export

setGeneric("hasFactorColumnNames", function(object,...){standardGeneric("hasFactorColumnNames")})
setMethod("hasFactorColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(length(object@factorized_cols) > 0 )
          }
)

#' Get factorization keys
#'
#' Returns names of columns that are going to be used as unique keys
#' during factorizations.
#' Factorization is unstacking operation where individual
#' factor values of factorized columns are turned into separate
#' set of columns spanning factor levels with logical values
#' indicating if given factor is present for unique set of
#' remaining factorization key columns
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{factorization_keys} character vector with list of key columns used in factorization
#' @export

setGeneric("getFactorizationKeyColumnNames", function(object,...){standardGeneric("getFactorizationKeyColumnNames")})
setMethod("getFactorizationKeyColumnNames",
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@factorization_keys)
          }
)


#' Perform factorization transformation
#'
#' Private method to perform unstacking of factor columns
#' Expands internal factor columns to logical columns
#' with factor levels as column names
#'
#' @param object object of class 'VirtualDataSourceClient'.
#' @return \code{object} object of class 'VirtualDataSourceClient'.

setGeneric(".transformReferenceData", function(object,...){standardGeneric(".transformReferenceData")})
setMethod(".transformReferenceData",
          signature(object = "VirtualDataSourceClient"),
          function(object){

            colnames_map <- getDataSourceClientColumnNameMap(object)
            factor_cols <- getFactorColumnNames(object)
            key_names <- getFactorizationKeyColumnNames(object)
            values <- getRequiredVariablesNames(object)

            ref_data <- getReferenceData(object)
            tr_data <- ref_data[setdiff(values, factor_cols)]
            tr_data <- tryCatch({
              for (col in factor_cols) {
                if (any(!is.na(ref_data[col]))) {
                  rv_data <- factor_transform(ref_data[c(key_names, col)], key_names, col)
                  by_cols <- intersect(colnames(rv_data), colnames(tr_data))
                  tr_data <- merge(tr_data, rv_data, by = by_cols, all.x = TRUE)
                }
              }

              tr_data
            }, error = function(cond) {
              stop(paste("Error ocured when transforming reference data in .transformReferenceData()",cond) )
            }
            )

            # ref_data <- cbind(ref_data[setdiff(colnames(ref_data), factor_cols)],
            #                   tr_data[setdiff(colnames(tr_data), colnames(ref_data))])

            ref_data <- unique(tr_data)

            object <- .setRequiredVariablesNames(object, colnames(ref_data))
            object <- .setStoredVariablesNames(object, colnames(ref_data))

            object <- setReferenceData(object, ref_data)

            return(object)
          }
)
