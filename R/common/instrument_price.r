sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class          = "InstrumentPriceDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID','dtDateTime'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prInstrumentHistory_Select"),
    urlquery     = new("URLQuery",root_url=middleware_urls@inst_price_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('lInstrumentID','dtDateTime'))
  ), contains = c("DataStore")
)
