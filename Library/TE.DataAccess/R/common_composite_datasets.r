#' @include dataset.r global_configs.r common_RAIDdata.r
NULL

setClass(
  Class          = "PositionComposite",
  representation = representation(
    data         = "DataSet",
    start        = "Date",
    end          = "Date",
    user_id      = "integer",
    database     = "character",
    dbuser       = "character"
  )
)

position_composite_factory <- function(user,start,end,use_db=TRUE){

  if(use_db){
    #This to be handled properly with database objects
    SQL <- paste("prPositionHistory_SelectByTraderDate @dtFrom = '",start,"', @dtTo = '",end,"', @lUserID = ",user,sep="")
    cn <- odbcConnect(database_config@database,uid=database_config@dbuser)
    pos_data <- sqlQuery(cn,SQL)
    close(cn)
    pos_data <- pos_data[c('lInstrumentID','lStrategyID','Date','dblTodayPL','dblMarketValue')]
    pos_data$lInstrumentID <- as.numeric(pos_data$lInstrumentID)
    pos_data$lStrategyID <- as.numeric(pos_data$lStrategyID)
    pos_data$Date <- as.Date(pos_data$Date)
    pos_data$dblTodayPL <- as.numeric(pos_data$dblTodayPL)
    pos_data$dblMarketValue <- as.numeric(pos_data$dblMarketValue)
    colnames(pos_data) <- c("InstrumentID","StrategyID","Date","TodayPL","MarketValue")
    urls <- c(middleware_urls@strategies_url)
  } else{
  psn_url_query = new("PositionHistoryURL",user_id=as.integer(user),start=as.Date(start),end=as.Date(end))
    urls <- c(middleware_urls@strategies_url,psn_url_query@url)
  }

  parser <- new("URLParser",parser_type = "XMLToFrame")
  parser <- runURLs(parser,urls)
  if(!use_db){
    pos_data <- getURLData(parser,2)
  }

  strategies <- new("StrategiesDataSet")
  str_data <- getURLData(parser,1)
  strat_cols <- colnames(str_data)
  strat_cols[strat_cols=="ID"] <- "StrategyID"
  colnames(str_data) <- strat_cols
  strategies <- setData(strategies,str_data)

  if (nrow(pos_data) == 0){
    stop(paste("Data query for positions in period", start, "to", stop, "returned empty dataset in position_composite_factory()."))
  }

  positions  <- new("PositionDataSet")
  positions  <- setData(positions,pos_data)

  position_composite <- new("DataSet")
  position_composite <- innerJoin(position_composite,strategies,NULL)
  position_composite <- innerJoin(position_composite,positions,c("StrategyID"))
  posn_comp_obj <- new("PositionComposite")
  posn_comp_obj@start <- start
  posn_comp_obj@end <- end
  posn_comp_obj@user_id <- user
  posn_comp_obj@data <- position_composite
  return(posn_comp_obj)
}


