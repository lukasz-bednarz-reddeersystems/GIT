#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "InstrumentSectorDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('InstrumentID')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@instr_sect_url,fields=c('id')),
    key_map      = new("KeyMap",key_generator=simple_id_kgen_fn,key_columns=c('InstrumentID'))
  ), contains = c("DataStore.URL")
)
