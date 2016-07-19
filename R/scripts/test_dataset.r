sourceTo("../common/RAIDdata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

psn_url_query <- new("PositionHistoryURL",user_id=as.integer(11),start=as.Date("2015-02-01"),end=as.Date("2015-02-02"))
psn_query_tester <- new("TestParser",parser_type = "XMLToFrame")
psn_query_tester <- runTests(psn_query_tester,c(psn_url_query@url))

psn_dataset <- new("PositionDataSet")
psn_dataset <- setData(psn_dataset,getTestData(psn_query_tester,1))
psn_dataset <- positionTotalsByStrat(psn_dataset)

