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

#' Create and fill PositionComposite object
#'
#' @param user integer, trader ID.
#' @param start Date start date
#' @param end Date end date
#' @param source "character", where the data should be pulled from
#' @return \code{posn_comp_obj} object of class 'PositionComposite'.
#' @export

position_composite_factory <- function(user,start,end,source = .__DEFAULT_POSITION_HISTORY_DATA_SOURCE__.){

  if(source == "DB"){
    #This to be handled properly with reference classes

    sql_query <- new("DataAccess.SQLProcedureCall.PositionHistory_SelectByTraderDate")

    key <- data.frame(TraderID  = user,
                      DateStart = as.Date(start),
                      DateEnd   = as.Date(end) )

    pos_data <- executeSQLQuery(sql_query, key)

    colnames(pos_data) <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql_query, colnames(pos_data))
    pos_data <- pos_data[c("InstrumentID","StrategyID","Date","TodayPL","MarketValue")]

    # converting Date class
    pos_data$Date <- as.Date(pos_data$Date)


    sql_query <- new("DataAccess.SQLProcedureCall.Strategy_SelectAll")
    str_data <- executeSQLQuery(sql_query)
    colnames(str_data) <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql_query, colnames(str_data))

    str_data <- str_data[c("StrategyID","Name","Active","Trader","FundGroup","UserID","Alias","Group","Type","Direction","Description","AliasID","GroupID","TypeID","InitiatorID")]

    } else {

    psn_url_query = new("PositionHistoryURL",user_id=as.integer(user),start=as.Date(start),end=as.Date(end))
    urls <- c(middleware_urls@strategies_url,psn_url_query@url)

    parser <- new("URLParser",parser_type = "XMLToFrame")
    parser <- runURLs(parser,urls)

    pos_data <- getURLData(parser,2)
    str_data <- getURLData(parser,1)
    strat_cols <- colnames(str_data)
    strat_cols[strat_cols=="ID"] <- "StrategyID"
    colnames(str_data) <- strat_cols
  }

  strategies <- new("StrategiesDataSet")
  strategies <- setData(strategies,unique(str_data))

  if (nrow(pos_data) == 0){
    message(paste("Data query for positions in period", start, "to", end, "returned empty dataset in position_composite_factory()."))
  }

  positions  <- new("PositionDataSet")
  positions  <- setData(positions,unique(pos_data))

  position_composite <- new("DataSet", unique_rows = TRUE)
  position_composite <- innerJoin(position_composite,strategies,NULL)
  position_composite <- innerJoin(position_composite,positions,c("StrategyID"))
  posn_comp_obj <- new("PositionComposite")
  posn_comp_obj@start <- start
  posn_comp_obj@end <- end
  posn_comp_obj@user_id <- user
  posn_comp_obj@data <- position_composite
  return(posn_comp_obj)
}


