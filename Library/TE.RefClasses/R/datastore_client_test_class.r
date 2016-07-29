#' @include datastore_client.r
NULL

####################################
#
# TestDataStoreClient Class
#
####################################

#' Concrete S4 class for testing of DataStoreClient.
#'
#' Test class for testing VirtualDataStoreClient
#' functionality. Do not use.
#'
#' Inherits from "VirtualDataStoreClient"
#' @export

setClass(
  Class             = "TestDataStoreClient",
  slots             = c("test_df"),
  prototype = list(
    datastore_name  = "test_datastore",
    key_cols        = c("lA", "dtB"),
    values          = c("lA","dtB","lC","sD","sE"),
    key_values      = data.frame(lA = numeric(), dtB = as.Date(character())),
    required_colnms = c("B","C","D","E"),
    factorized_cols    = "D",
    factorization_keys = c("B","C","E"),
    column_name_map = hash(c("lA","dtB","lC","sD","sE"),
                           c("A","B","C","D","E")),
    test_df         = data.frame(lA = 1:10,
                                 dtB = seq(from = as.Date("2016-05-01"), to = as.Date("2016-05-10"), by = "1 day"),
                                 lC = 1:10,
                                 sD = letters[1:10],
                                 sE = LETTERS[1:10],
                                 stringsAsFactors = FALSE)
  ),
  contains = c("VirtualDataStoreClient")
)


setMethod("dataRequest",
          signature(object = "TestDataStoreClient", key_values = "data.frame"),
          function(object, key_values){
            object <- .setDataSourceQueryKeyValues(object,key_values)
            datastore <- getDataStoreName(object)
            key_names <- getDataSourceQueryKeyColumnNames(object)
            values <- getDataSourceReturnColumnNames(object)
            colnames_map <- getDataSourceClientColumnNameMap(object)
            test_df <- object@test_df

            # data request sent to dataplex
            query_data <- merge(test_df, key_values, by = key_names, sort = FALSE)


            if (0 == nrow(query_data)) {
              message(paste("Object", class(object), "in dataRequest()"))
              message(paste("Query sent to", datastore, "returned zero row data.frame"))
              stop(paste("Query sent to", datastore, "returned zero row data.frame"))
            }

            # translating column names (attention! hash is unordered storage)
            colnames(query_data) <- values(colnames_map[colnames(query_data)])[colnames(query_data)]

            # storing Reference data internaly
            object <- setReferenceData(object, query_data)


            if(hasNonNAColumnNames(object)){
              object <- .removeNAReferenceData(object)

            }

            if(hasFactorColumnNames(object)){
              object <- .transformReferenceData(object)

            }

            return(object)
          }
)
