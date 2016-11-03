#' @include datastore_client.r
NULL

####################################
#
# DealerData Class
#
####################################

#' Concrete S4 class for handling Dealer Data.
#'
#' Test class for testing VirtualDataStoreClient
#' functionality. Do not use.
#'
#' Inherits from "VirtualDataStoreClient"
#' @export

setClass(
  Class             = "DealerData",
  prototype      = list(
    datastore_name = "dealing_datastore",
    key_cols        = c("TraderID", "Date"),
    values          = c("lTraderID","dtTradeDate","lInstrumentID","sTradeRationale","sInputDirection"),
    required_colnms = c("Date","InstrumentID","Rationale","InputDirection"),
    factorized_cols = c("InputDirection"),
    factorization_keys = c("Date","InstrumentID"), # key columns that are used as a unique keys in factorization
    non_na_cols     = character(),
    column_name_map = hash("lTraderID"       = "TraderID",
                           "dtTradeDate"     = "Date",
                           "TraderID"        = "lTraderID",
                           "Date"            = "dtTradeDate",
                           "lInstrumentID"   = "InstrumentID",
                           "sTradeRationale" = "Rationale",
                           "sInputDirection" = "InputDirection"),
    key_values      = data.frame(TraderID = integer(),
                                 Date = as.Date(character()))
    ),

  contains = c("VirtualDataStoreClient")
)


