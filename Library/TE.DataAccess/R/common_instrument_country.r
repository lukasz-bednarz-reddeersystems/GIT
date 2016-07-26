#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "InstrumentCountryDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prInstrument_SelectCountry"),
    urlquery     = new("URLQuery",root_url=middleware_urls@inst_country_url,fields=c('id')),
    key_map      = new("KeyMap",key_generator=simple_id_kgen_fn,key_columns=c('lInstrumentID'))
  ), contains = c("DataStore")
)
