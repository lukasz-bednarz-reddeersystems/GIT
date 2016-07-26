#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "EventDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('InstrumentID','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame"),
    urlquery     = new("URLQuery",root_url=middleware_urls@events_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('InstrumentID','DateTime'))
  ), contains = c("DataStore")
)
