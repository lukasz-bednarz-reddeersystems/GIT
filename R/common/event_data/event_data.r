sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/rodbc_client/rodbc_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/rodbc_client/rodbc_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# EventData Class
#
####################################


setClass(
  Class             = "VirtualEventData",
  prototype      = list(
    required_colnms = c("InstrumentID","Date","EventType")
  ),
  
  contains = c("VirtualReferenceData")
)


# setClass(
#   Class             = "EventData",
#   prototype      = list(
#     datastore_name = "event_datastore",
#     key_cols        = c("InstrumentID","DateTime"),
#     values          = c("InstrumentID","DateTime", "EventType"),
#     factorized_cols = c("EventType"),
#     factorization_keys = c("Date","InstrumentID"),
#     non_na_cols     = character(),
#     column_name_map = hash(c("DateTime","InstrumentID","EventType"), 
#                            c("Date","InstrumentID","EventType")),
#     key_values      = data.frame(InstrumentID = integer(), 
#                                  DateTime = as.Date(character()))
#     ),
#   
#   contains = c("VirtualEventData", "VirtualDataStoreClient")
# )

####################################
#
# EventData Class
#
####################################

setClass(
  Class     = "EventDataSQLQuery",
  prototype = list(
    key_cols       = c("InstrumentID", "Date"),
    key_values     = data.frame(lInstrumentID = integer(), 
                                dtDateTime = as.Date(character())),
    query_parser   = parse_instrument_date_keys,
    results_parser = convert_column_class,
    arguments    = c("@sInstrumentIDs", "@dtFrom", "@dtTo"),
    procedure    = "prEvent_GetByDateInstrumentIDs"
  ),
  contains  = c("VirtualSQLProcedureCall")
)

setClass(
  Class             = "EventData",
  prototype      = list(
    key_cols        = c("InstrumentID","Date"),
    values          = c("lInstrumentID","dtDateTime", "sEventType"),
    factorized_cols = c("EventType"),
    factorization_keys = c("Date","InstrumentID"),
    non_na_cols     = character(),
    column_name_map = hash(c("dtDateTime","lInstrumentID","sEventType"),
                           c("Date","InstrumentID","EventType")),
    key_values      = data.frame(InstrumentID = integer(),
                                 Date = as.Date(character())),
    sql_query       = new("EventDataSQLQuery")
  ),
  
  contains = c("VirtualEventData", "VirtualRODBCClient")
)
