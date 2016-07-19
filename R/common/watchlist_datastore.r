sourceTo("../lib/datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class          = "WatchListDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('TraderID','WatchlistID','Date')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@watchlist_url,fields=c('id','fn','date')),
    key_map      = new("KeyMap",key_generator=watchlist_keygen,key_columns=c('TraderID','WatchlistID','Date'))
  ), contains = c("DataStore")
)
