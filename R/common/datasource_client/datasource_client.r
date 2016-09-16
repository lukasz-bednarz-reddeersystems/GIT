sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/datasource_client/datasource_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plyr)
library(hash)


####################################
#
# VirtualDataSourceClient Class
#
####################################

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

if (!isGenericS4(".PostProcessResultsData")){
  setGeneric(".PostProcessResultsData", function(object,...){standardGeneric(".PostProcessResultsData")})
}
# Runs Necessary postprocessing routines
#
# Args:
#   object : object of type "VirtualReferenceData"
# Returns:
#   object : object of type "VirtualReferenceData"

setMethod(".PostProcessResultsData",  
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object)
          }
)


if (!isGenericS4(".generateDataFilledWithNA")) {
 setGeneric(".generateDataFilledWithNA", function(object, trader, start, end,...){standardGeneric(".generateDataFilledWithNA")})
}
# Returns Data.frame filled with NA's.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   ret_data : frame filled with na's



setGeneric("getDataSourceQueryKeyColumnNames", function(object,...){standardGeneric("getDataSourceQueryKeyColumnNames")})
# Returns key column names that are used to  which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   list of key column names

setMethod("getDataSourceQueryKeyColumnNames",  
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@key_cols)
          }
)

setGeneric("getDataSourceReturnColumnNames", function(object,...){standardGeneric("getDataSourceReturnColumnNames")})
# Returns names of the columns that will be returned from datastore
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   return table column names

setMethod("getDataSourceReturnColumnNames",  
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@values)
          }
)

setGeneric("getNonNAColumnNames", function(object,...){standardGeneric("getNonNAColumnNames")})
# Returns column names that are required to have non-na values
# rows where these columns have NA's will be removed.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name

setMethod("getNonNAColumnNames", 
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@non_na_cols)
          }
)


setGeneric("hasNonNAColumnNames", function(object,...){standardGeneric("hasNonNAColumnNames")})
# Returns true if object has columns that are required to have non-na-values
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   has_non_na_cols

setMethod("hasNonNAColumnNames", 
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(length(object@non_na_cols) > 0)
          }
)

setGeneric(".setDataSourceQueryKeyValues", function(object, key_values, ...){standardGeneric(".setDataSourceQueryKeyValues")})
# Set values of key values datatable with keys that will be matched in datastore query
#
# Args:
#   object : object of type "VirtualDataSourceClient"
#   key_values : data.frame with columns matching key column names()
# Returns:
#   object : object of type "VirtualDataSourceClient"

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

setGeneric("getDataSourceQueryKeyValues", function(object,...){standardGeneric("getDataSourceQueryKeyValues")})
# Returns name of the datastore which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name

setMethod("getDataSourceQueryKeyValues",  
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@key_values)
          }
)




setGeneric("getDataSourceClientColumnNameMap", function(object,...){standardGeneric("getDataSourceClientColumnNameMap")})
# Returns name of the datastore which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#    column name map of the datasource to output names

setMethod("getDataSourceClientColumnNameMap",  
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@column_name_map)
          }
)


setGeneric(".translateDataSourceColumnNames", function(object, colnames, ...){standardGeneric(".translateDataSourceColumnNames")})
# Returns name of the datastore which will be querried when asked to fill data.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   data source column names translated to output column names as required by "required Variables"

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

setGeneric(".removeNAReferenceData", function(object,...){standardGeneric(".removeNAReferenceData")})
# removes rows in reference data where non-NA column names are NA
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   object : object of type "VirtualDataSourceClient"

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

if (!isGenericS4("dataRequest")) {
  setGeneric("dataRequest", function(object, key_values, ...){standardGeneric("dataRequest")})
}
# Sets internal data to values returned from datastore.
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name



if (!isGenericS4("getFactorColumnNames")){
  setGeneric("getFactorColumnNames", function(object,...){standardGeneric("getFactorColumnNames")})
}
# Returns names of factorized column names
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name

setMethod("getFactorColumnNames", 
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@factorized_cols)
          }
)


if (!isGenericS4("hasFactorColumnNames")){
  setGeneric("hasFactorColumnNames", function(object,...){standardGeneric("hasFactorColumnNames")})
}
# returns logical value indicating if object has columns that need to be factorized and
# transformed into logical columns
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name

setMethod("hasFactorColumnNames", 
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(length(object@factorized_cols) > 0 )
          }
)


if (!isGenericS4("getFactorizationKeyColumnNames")){
  setGeneric("getFactorizationKeyColumnNames", function(object,...){standardGeneric("getFactorizationKeyColumnNames")})
}
# Returns names of factorized column names
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   datastore_name

setMethod("getFactorizationKeyColumnNames", 
          signature(object = "VirtualDataSourceClient"),
          function(object){
            return(object@factorization_keys)
          }
)

if (!isGenericS4(".transformReferenceData")){
  setGeneric(".transformReferenceData", function(object,...){standardGeneric(".transformReferenceData")})
}
# expands internal factor columns to logical columns with factor levels as column names 
#
# Args:
#   object : object of type "VirtualDataSourceClient"
# Returns:
#   object : object of type "VirtualDataSourceClient"

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
