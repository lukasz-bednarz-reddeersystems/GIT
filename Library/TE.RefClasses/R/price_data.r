#' @include rodbc_client.r
NULL

####################################
#
# PriceData Class
#
####################################


setClass(
  Class     = "PriceDataSQLQuery",
  prototype = list(
    key_cols       = c("lInstrumentID", "dtDateTime"),
    key_values     = data.frame(lInstrumentID = integer(),
                                dtDateTime = as.Date(character())),
    query_parser   = parse_instrument_date_keys,
    results_parser = convert_column_class,
    arguments    = c("@sInstrumentIDs", "@dtFrom", "@dtTo"),
    procedure    = "prInstrumentHistory_SelectByInstrList"
  ),
  contains  = c("VirtualSQLProcedureCall")
)


setClass(
  Class             = "VirtualPriceData",
  prototype      = list(
    required_colnms = c('InstrumentID','Date','ClosePrice')
  ),

  contains = c("VirtualReferenceData")
)

#' Concrete S4 class handling PriceData
#'
#' @export

setClass(
  Class             = "PriceData",
  prototype      = list(
    key_cols        = c("lInstrumentID", "dtDateTime"),
    values          = c("lInstrumentID", "dtDateTime","dblClosePrice","dblPreviousClosePrice",
                        "dblVolume", "lOutstandingShares","dbl30DayAvgVol" ),
    column_name_map = hash(c("lInstrumentID", "dtDateTime","dblClosePrice","dblPreviousClosePrice",
                             "dblVolume", "lOutstandingShares","dbl30DayAvgVol" ),
                           c('InstrumentID','Date','ClosePrice',"PreviousClosePrice",
                             "Volume", "OutstandingShares","AvgVol30Day")),
    key_values      = data.frame(lInstrumentID = integer(),
                                 dtDateTime = as.Date(character())),
    sql_query       = new("PriceDataSQLQuery")
  ),

  contains = c("VirtualPriceData", "VirtualRODBCClient")
)



