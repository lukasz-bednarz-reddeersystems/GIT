#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "InstrumentHistoryDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('Instrument','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@instr_hist_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('Instrument','DateTime'))
  ), contains = c("DataStore")
)

