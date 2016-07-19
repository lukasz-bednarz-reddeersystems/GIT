sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class          = "InstrumentDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('ID'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@instr_dts_url,fields=c('id')),
    key_map      = new("KeyMap",key_generator=simple_id_kgen_fn,key_columns=c('ID'))
  ), contains = c("DataStore")
)
