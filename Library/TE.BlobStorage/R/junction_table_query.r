#' @include filetable_query.r
NULL


#' VirtualBlobStorageSQLProcedureCall class
#'
#' Implements handling querries for BLOB storage
#' procedure Calls
#'
#' Inherits from "VirtualSQLProcedureCall"
setClass(
  Class     = "BlobStorage.VirtualSQLProcedureCall",
  contains  = c("VirtualSQLProcedureCall", "VIRTUAL")
)

#' Initialize method for "BlobStorage.VirtualSQLProcedureCall"
#'
#' @param .Object object of class derived from BlobStorage.VirtualSQLProcedureCall
#' @param db_name "character" database name
#' @param db_schema "character" database schema
setMethod("initialize",
          signature(.Object = "BlobStorage.VirtualSQLProcedureCall"),
          function(.Object, db_name, db_schema) {
            .Object <- callNextMethod()

            return(.Object)
          })



#################################################################################
#
# BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName class
#'
#' Implements handling querries for filetable name that is referenced by
#' associated junction table via path_locator foreign key
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName",
  prototype = list(
    key_cols   = c("TableName"),
    key_values = data.frame(TableName = character()),
    arguments    = c("@sParentTableName"),
    procedure    = "prMultiFactorRisk_ReferencedFileTable_SelectByParentTableName"
  ),
  contains  = c("BlobStorage.VirtualSQLProcedureCall")
)


#' Initialize method for "SQLQuery.FileTableRootPath"
#'
#' @rdname initialize-ReferencedFileTable_SelectByParentTableName-method
#' @param .Object object of class derived from FileTableSQLQuerry
#' @param db_name "character" database name
#' @param db_schema "character" database schema
#' @param tb_name "character" table name to be querried
#'
#' @export
setMethod("initialize",
          signature(.Object = "BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName"),
          function(.Object, db_name, db_schema,tb_name) {

            .Object <- callNextMethod(.Object, db_name, db_schema )

            .Object <- prepareSQLQuery(.Object, data.frame(TableName = tb_name))

            return(.Object)
          })
