#' TE.DataAccess: Trading Enhancement Data access, cashing and preprocessing
#'
#' The TE.DataAccess package providing classes and functions for data retrieval,
#' caching  and manipulation for Trading Enhancement R engine
#'
#' @section TE.DataAccess classes:
#' \link{DataSet-class}
#'
#' @docType package
#' @name TE.DataAccess
#'
#' @import TE.BlobStorage
#' @import TE.SQLQuery
#' @import lubridate
#' @import XML
#' @import methods
#' @importFrom stats as.formula aggregate cor sd setNames
#' @importFrom utils close.socket head make.socket object.size read.socket write.socket
#' @importFrom hashFunction cityhash.64 murmur3.32
#' @importFrom plyr rbind.fill arrange desc
#' @importFrom XML xmlTreeParse
#' @importFrom moments skewness
#' @importFrom RODBC odbcConnect sqlQuery
#' @importFrom grDevices dev.off
#' @importFrom graphics plot title
#' @importFrom R.utils copyFile
#' @importFrom digest digest
#' @importFrom hash hash values names.hash keys
#' @importClassesFrom hash hash
NULL
## NULL



.__DEFAULT_OBJECTSTORE_ODBC_DB_NAME__.   <- "RAIDLIVEDB"
.__DEFAULT_OBJECTSTORE_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_OBJECTSTORE_FILE_DB_SCHEMA__. <- "FileTableDB"

.__DEFAULT_ODBC_DB_NAME__.   <- "RAIDLIVEDB"
.__DEFAULT_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_DB_SCHEMA__.      <- "Research"

.__DEFAULT_TRADE_HISTORY_DATA_SOURCE__. <- "DB" # possible values: c("DB", "Middleware")
.__DEFAULT_POSITION_HISTORY_DATA_SOURCE__. <- .__DEFAULT_TRADE_HISTORY_DATA_SOURCE__. # possible values: c("DB", "Middleware")
.__DEFAULT_PRICE_HISTORY_DATA_SOURCE__. <- .__DEFAULT_TRADE_HISTORY_DATA_SOURCE__. # possible values: c("DB", "Middleware")
.__DEFAULT_TRADE_LEVELS_DATA_SOURCE__. <- .__DEFAULT_TRADE_HISTORY_DATA_SOURCE__. # possible values: c("DB", "Middleware")
.__DEFAULT_EXT_POSITION_DATA_SOURCE__. <- .__DEFAULT_TRADE_HISTORY_DATA_SOURCE__. # possible values: c("DB", "Middleware")
