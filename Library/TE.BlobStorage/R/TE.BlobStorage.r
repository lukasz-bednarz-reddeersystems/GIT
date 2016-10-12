#' Trading Enhancement storage of objects in Database
#'
#' Package providing classes and functions for retrieval and storage
#' of Binary Large Objects (BLOB's) into Database Tables
#'
#' @docType package
#' @name TE.BlobStorage
#'
#' @import TE.SQLQuery
#' @import lubridate
#' @import methods
#' @import XML
#' @importFrom R.utils copyFile
#' @importFrom hash hash values names.hash keys
#' @importFrom digest digest
#' @importFrom plyr arrange
#' @importFrom XML xmlTree saveXML
#' @importClassesFrom hash hash
NULL



.__DEFAULT_ODBC_DB_NAME__.   <- "RAIDLIVEDB"
.__DEFAULT_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_FILE_DB_SCHEMA__. <- "FileTableDB"
