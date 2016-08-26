#' @include TE.BlobStorage.r
NULL

#' VirtualFileTableSQLQuery class
#'
#' Implements handling querries for File Tables
#'
#' Inherits from "VirtualSQLQuery"
setClass(
  Class     = "VirtualFileTableSQLQuery",
  contains  = c("VirtualSQLQuery", "VIRTUAL")
)

#' Initialize method for "VirtualSQLQuery"
#'
#' @param .Object object of class derived from FileTableSQLQuerry
#' @param db_name "character" database name
#' @param db_schema "character" database schema
setMethod("initialize",
          signature(.Object = "VirtualFileTableSQLQuery"),
          function(.Object, db_name, db_schema) {
          .Object <- callNextMethod()

          return(.Object)
})



##########################################
#
# SQLQuery.FileTableRootPath class
#
##########################################


#' function to generate filetable rootpath query
#'
#' @param keys "data.frame" with keys
generate_filetable_rootpath_query <- function(keys) {

  SQL <- NULL

  for (row_idx in seq(nrow(keys))) {
    tb_name <- keys[row_idx,1]
    sql <- sprintf("SELECT FileTableRootPath('%s') [Path]", tb_name)
    SQL[row_idx] <- sql
  }

  return(SQL)
}

#' SQLQuery.FileTableRootPath class
#'
#' Implements handling querries for root path of file tables
#'
#' Inherits from "VirtualFileTableSQLQuerry"
#'
#' @export
setClass(
  Class     = "SQLQuery.FileTableRootPath",
  prototype = list(
    key_cols   = c("TableName"),
    key_values = data.frame(TableName = character()),
    query_parser   = generate_filetable_rootpath_query
  ),
  contains  = c("VirtualFileTableSQLQuery")
)


#' Initialize method for "SQLQuery.FileTableRootPath"
#'
#' @param .Object object of class derived from FileTableSQLQuerry
#' @param db_name "character" database name
#' @param db_schema "character" database schema
#' @param tb_name "character" table name to be querried
#'
#' @export
setMethod("initialize",
          signature(.Object = "SQLQuery.FileTableRootPath"),
          function(.Object, db_name, db_schema,tb_name) {

            .Object <- callNextMethod(.Object, db_name, db_schema )
            .Object <- prepareSQLQuery(.Object, data.frame(TableName = tb_name))

            return(.Object)
          })


##########################################
#
# SQLQuery.FileStoredInFileTable class
#
##########################################


#' function to generate filetable query for stored file
#'
#' @param keys "data.frame" with keys
generate_filetable_is_stored_query <- function(keys) {

  SQL <- NULL

  for (row_idx in seq(nrow(keys))) {
    tb_name <- keys[row_idx,1]
    filename <- keys[row_idx, 2]
    sql <- sprintf("SELECT name FROM %s WHERE name = '%s'", tb_name, filename)
    SQL[row_idx] <- sql
  }

  return(SQL)
}

#' SQLQuery.FileTableRootPath class
#'
#' Implements handling querries for root path of file tables
#'
#' Inherits from "VirtualFileTableSQLQuerry"
#'
#' @export
setClass(
  Class     = "SQLQuery.FileStoredInFileTable",
  prototype = list(
    key_cols   = c("TableName"),
    key_values = data.frame(TableName = character()),
    query_parser   = generate_filetable_is_stored_query
  ),
  contains  = c("VirtualFileTableSQLQuery")
)


#' Initialize method for "SQLQuery.FileStoredInFileTable"
#'
#' @param .Object object of class derived from FileTableSQLQuerry
#' @param db_name "character" database name
#' @param db_schema "character" database schema
#' @param tb_name "character" table name to be querried
#' @param filename "character" vector of filenames to be querried
#'
#' @export
setMethod("initialize",
          signature(.Object = "SQLQuery.FileStoredInFileTable"),
          function(.Object, db_name, db_schema,tb_name, filename) {

            .Object <- callNextMethod(.Object, db_name, db_schema )
            .Object <- prepareSQLQuery(.Object, data.frame(TableName = tb_name, FileName = filename))

            return(.Object)
          })

