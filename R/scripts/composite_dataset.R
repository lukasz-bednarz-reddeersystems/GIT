sourceTo("../common/RAIDdata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

psn_url_query = new("PositionHistoryURL",user_id=as.integer(11),start=as.Date("2015-02-01"),end=as.Date("2015-02-02"))
parser <- new("URLParser",parser_type = "XMLToFrame")
parser <- runURLs(parser,c(psn_url_query@url,"http://localhost:8082/trading/strategies"))

strategies <- new("StrategiesDataSet")
str_data <- getURLData(parser,2)
strat_cols <- colnames(str_data)
strat_cols[strat_cols=="ID"] <- "StrategyID"
colnames(str_data) <- strat_cols
strategies <- setData(strategies,str_data)
positions  <- new("PositionDataSet")
positions  <- setData(positions,getURLData(parser,1))

position_composite <- new("DataSet")
position_composite <- innerJoin(position_composite,strategies,NULL)
position_composite <- innerJoin(position_composite,positions,c("StrategyID"))
