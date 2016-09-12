#' @include warehouse_client.r
NULL

####################################
#
# PositionData Class
#
####################################

#' Virtual S4 class handling Portfolio Positions data.
#'
#' Implements storage Portfolio Positions
#'
#' Inherits from "VirtualReferenceData

setClass(
  Class             = "VirtualPositionData",
  prototype      = list(
    required_colnms = c("Date", "StrategyID", "InstrumentID", "TraderID", "Direction", "MarketValue", "TodayPL",
                        "Quantity", "Age",  "PsnReturn")
  ),

  contains = c("VirtualReferenceData")
)


#' Concrtete S4 class handling Portfolio Positions data.
#'
#' Implements storage for Portfolio Positions and access
#' to Portfolio Positions data via warehouse store
#'
#' Inherits from "VirtualPositionData" and "VirtualWarehouseClient"
#' @export

setClass(
  Class             = "PositionData",
  prototype      = list(
    values = c("UserID", "Date", "StrategyID", "InstrumentID", "Active", "Trader", "FundGroup",
                "Alias", "Group", "Type", "Direction", "Description", "AliasID",
               "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
               "Quantity", "Age",  "PsnReturn"),
    column_name_map = hash(c("Date", "StrategyID", "InstrumentID", "Active", "Trader", "FundGroup",
                             "UserID", "Alias", "Group", "Type", "Direction", "Description", "AliasID",
                             "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
                             "Quantity", "Age",  "PsnReturn"),
                           c("Date", "StrategyID", "InstrumentID", "Active", "TraderName", "FundGroup",
                             "TraderID", "Alias", "Group", "Type", "Direction", "Description", "AliasID",
                             "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
                             "Quantity", "Age",  "PsnReturn")),
    key_values      = data.frame(id = integer(),
                                 start = as.Date(character()),
                                 end = as.Date(character()))
    ),

  contains = c("VirtualPositionData","VirtualWarehouseClient")
)



setMethod(".generateDataFilledWithNA",
          signature(object = "PositionData"),
          function(object, trader, start, end){

            ret_vars <- getDataSourceReturnColumnNames(object)

            diff <- setdiff(ret_vars, c("UserID", "Date"))

            ret_data <- data.frame(TraderID = trader,  Date = seq(from = start, to = end, by = 1))

            ret_data <- cbind(ret_data, data.frame(t(rep(NA,length(diff)))))

            colnames(ret_data) <- c("UserID", "Date", diff )

            return(ret_data)
          }
)

setMethod(".PostProcessResultsData",
          signature(object = "PositionData"),
          function(object){

            data <- getReferenceData(object)

            cols <- colnames(data)

            data$Quantity.old <- data$Quantity
            data$PsnReturn.old <- data$PsnReturn
            data$PsnReturn.w <- data$Quantity * data$PsnReturn

            aggr_cols <- setdiff(colnames(data), c("Quantity", "PsnReturn"))

            data_agr <- aggregate(data[aggr_cols], data[setdiff(cols, aggr_cols)], sum)

            data_agr$PsnReturn <- data$PsnReturn/data$Quantity

            data_agr <- data_agr[cols]

            object <- setReferenceData(object, data_agr)

            return(object)
          }
)
