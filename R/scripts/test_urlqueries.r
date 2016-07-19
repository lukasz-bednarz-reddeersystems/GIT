sourceTo("../common/RAIDdata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

test_positions = TRUE
test_trades = TRUE

if(test_positions)
{
  psn_url_query = new("PositionHistoryURL",user_id=as.integer(11),start=as.Date("2015-02-01"),end=as.Date("2015-02-02"))
  psn_query_tester <- new("URLParser",parser_type = "XMLToFrame")
  psn_query_tester <- runURLs(psn_query_tester,c(psn_url_query@url))
}

if(test_trades)
{
  trd_url_query = new("TradeHistoryURL",user_ids=as.integer(11),start=as.Date("2015-02-01"),end=as.Date("2015-02-02"))
  trd_query_tester <- new("URLParser",parser_type = "XMLToFrame")
  trd_query_tester <- runURLs(trd_query_tester,c(trd_url_query@url))
}