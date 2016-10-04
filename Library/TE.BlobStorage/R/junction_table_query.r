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
  contains  = c("VirtualSQLProcedureCall", "VIRTUAL"),
  prototype = list(
    results_parser = TE.SQLQuery:::convert_column_class
  )
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




#' VirtualBlobStorageSQLProcedureCall class
#'
#' Implements handling querries for BLOB storage
#' procedure Calls
#'
#' Inherits from "VirtualSQLProcedureCall"
setClass(
  Class     = "BlobStorage.JunctionTable.VirtualSQLProcedureCall",
  contains  = c("BlobStorage.VirtualSQLProcedureCall")
)

#' Initialize method for "BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID"
#'
#' @rdname initialize-BlobStorage.JunctionTable.VirtualSQLProcedureCall-method
#' @param .Object object of class derived from BlobStorage.JunctionTable.VirtualSQLProcedureCall
#' @param db_name "character" database name
#' @param db_schema "character" database schema
#' @param tb_name "character" table name to be querried
#' @param keys "data.frame" with columns corresponding to procedure parameters
#'
#' @export
setMethod("initialize",
          signature(.Object = "BlobStorage.JunctionTable.VirtualSQLProcedureCall"),
          function(.Object, db_name, db_schema,tb_name, keys = NULL) {

            .Object <- callNextMethod(.Object, db_name, db_schema )

            if (!is.null(keys) && is.data.frame(keys)) {

              keys <- cbind(data.frame(TableName = tb_name), keys)

              .Object <- prepareSQLQuery(.Object, keys)
            }

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


#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID class
#'
#' Implements handling querries for joint table that
#' stores keys of associated file table. Returns file name column that
#' is associated with given key
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_QueryByTbNameTraderIDStartDateEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID",
  prototype = list(
    key_cols   = c("TableName", "HashID"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@sHashID"
    ),
    column_name_map = hash(c("sJointTableName",
                             "sHashID",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName"
    ),
                          c("JointTableName",
                            "HashID",
                            "CreatedDate",
                            "CreatedByUserID",
                            "FileName")),
    procedure    = "prMultiFactorRisk_JointFileTable_QueryByHashID"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)





#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_UpdateByHashID class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_UpdateByHashID class
#'
#' Implements handling update for joint table that
#' stores keys of associated file table. Returns number of stored and
#' insertet keys
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_UpdateByTbNameTraderIDStartDateEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_UpdateByHashID",
  prototype = list(
    key_cols   = c("TableName", "HashID", "CreatedDate", "CreatedByUserID", "FileName"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@sHashID",
                   "@dtCreatedDate",
                   "@sCreatedByUserID",
                   "@sFileName"),

    column_name_map = hash(c("sJointTableName",
                             "sHashID",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName"
    ),
    c("JointTableName",
      "HashID",
      "CreatedDate",
      "CreatedByUserID",
      "FileName")),
    procedure    = "prMultiFactorRisk_JointFileTable_UpdateByHashID"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)


#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate class
#'
#' Implements handling querries for joint table that
#' stores keys of associated file table. Returns file name column that
#' is associated with given key
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_QueryByTbNameTraderIDStartDateEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate",
  prototype = list(
    key_cols   = c("TableName", "TraderID", "StartDate", "EndDate"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@lTraderID",
                   "@dtStartDate",
                   "@dtEndDate"
                   ),
    column_name_map = hash(c("sJointTableName",
                             "lTraderID",
                             "dtStartDate",
                             "dtEndDate",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName"
                             ),
                           c("JointTableName",
                             "TraderID",
                             "StartDate",
                             "EndDate",
                             "CreatedDate",
                             "CreatedByUserID",
                             "FileName")),
    procedure    = "prMultiFactorRisk_JointFileTable_QueryByTbNameTraderIDStartDateEndDate"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)


#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate class
#'
#' Implements handling update for joint table that
#' stores keys of associated file table. Returns number of stored and
#' insertet keys
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_UpdateByTbNameTraderIDStartDateEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate",
  prototype = list(
    key_cols   = c("TableName", "TraderID", "StartDate", "EndDate",
                   "CreatedDate", "CreatedByUserID", "FileName"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@lTraderID",
                   "@dtStartDate",
                   "@dtEndDate",
                   "@dtCreatedDate",
                   "@sCreatedByUserID",
                   "@sFileName"),

    column_name_map = hash(c("sJointTableName",
                             "lTraderID",
                             "dtStartDate",
                             "dtEndDate",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName"
                             ),
                           c("JointTableName",
                             "TraderID",
                             "StartDate",
                             "EndDate",
                             "CreatedDate",
                             "CreatedByUserID",
                             "FileName")),
    procedure    = "prMultiFactorRisk_JointFileTable_UpdateByTbNameTraderIDStartDateEndDate"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)

#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#'
#' Implements handling querries for joint table that
#' stores keys of associated file table. Returns file name column that
#' is associated with given key
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate",
  prototype = list(
    key_cols   = c("TableName",
                   "TraderID",
                   "InstrumentID",
                   "Direction",
                   "Strategy",
                   "LegStartDate",
                   "LegEndDate"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@lTraderID",
                   "@lInstrumentID",
                   "@sDirection",
                   "@sStrategy",
                   "@dtLegStartDate",
                   "@dtLegEndDate"),
    column_name_map = hash(c("sJointTableName",
                             "lTraderID",
                             "lInstrumentID",
                             "sDirection",
                             "sStrategy",
                             "dtLegStartDate",
                             "dtLegEndDate",
                             "sLegStatus",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName",
                             'hash',
                             'id',
                             'instrument',
                             'buysell',
                             'strategy',
                             'start',
                             'end',
                             'status'

                              ),
                           c("JointTableName",
                             "TraderID",
                             "InstrumentID",
                             "Direction",
                             "Strategy",
                             "LegStartDate",
                             "LegEndDate",
                             "LegStatus",
                             "CreatedDate",
                             "CreatedByUserID",
                             "FileName",
                             "HashID",
                             "TraderID",
                             "InstrumentID",
                             "Direction",
                             "Strategy",
                             "LegStartDate",
                             "LegEndDate",
                             "LegStatus"
                          )),
    procedure    = "prMultiFactorRisk_JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)


#################################################################################
#
# BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#
#################################################################################


#' BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#'
#' Implements handling update for joint table that
#' stores keys of associated file table. Returns number of stored and
#' insertet keys
#'
#' Inherits from "BlobStorage.VirtualSQLProcedureCall"
#'
#' @rdname JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate-class
#' @export
setClass(
  Class     = "BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate",
  prototype = list(
    key_cols   = c("TableName",
                   "HashID",
                   "TraderID",
                   "InstrumentID",
                   "Direction",
                   "Strategy",
                   "LegStartDate",
                   "LegEndDate",
                   "LegStatus",
                   "CreatedDate",
                   "CreatedByUserID",
                   "FileName"),
    key_values = data.frame(TableName = character()),
    arguments  = c("@sJointTableName",
                   "@sHashID",
                   "@lTraderID",
                   "@lInstrumentID",
                   "@sDirection",
                   "@sStrategy",
                   "@dtLegStartDate",
                   "@dtLegEndDate",
                   "@sLegStatus",
                   "@dtCreatedDate",
                   "@sCreatedByUserID",
                   "@sFileName"),
    column_name_map = hash(c("sJointTableName",
                             "sHashID",
                             "lTraderID",
                             "lInstrumentID",
                             "sDirection",
                             "sStrategy",
                             "dtLegStartDate",
                             "dtLegEndDate",
                             "sLegStatus",
                             "dtCreatedDate",
                             "sCreatedByUserID",
                             "sFileName",
                             'hash',
                             'id',
                             'instrument',
                             'buysell',
                             'strategy',
                             'start',
                             'end',
                             'status'
                            ),
                            c("JointTableName",
                              "HashID",
                              "TraderID",
                              "InstrumentID",
                              "Direction",
                              "Strategy",
                              "LegStartDate",
                              "LegEndDate",
                              "LegStatus",
                              "CreatedDate",
                              "CreatedByUserID",
                              "FileName",
                              "HashID",
                              "TraderID",
                              "InstrumentID",
                              "Direction",
                              "Strategy",
                              "LegStartDate",
                              "LegEndDate",
                              "LegStatus"
                            )),
    procedure    = "prMultiFactorRisk_JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate"
  ),
  contains  = c("BlobStorage.JunctionTable.VirtualSQLProcedureCall")
)

