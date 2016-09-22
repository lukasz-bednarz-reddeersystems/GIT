#' @include objectstore.r
NULL

.__DEFAULT_OBJECTSTORE_ODBC_DB_NAME__.   <- "RAIDSTAGEDB"
.__DEFAULT_OBJECTSTORE_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_OBJECTSTORE_FILE_DB_SCHEMA__. <- "FileTableDB"

#' helper function to generate key from objectstore name
#'
#' @param name "character" name of the objectstore
#' @return \code{key} "data.frame" with columns "id", "start", "end"
key_from_name <- function(name) {

  str_keys <- strsplit(name, "_")

  key <- data.frame(id    = str_keys[[1]][1],
                    start = str_keys[[1]][2],
                    end   = str_keys[[1]][3])
  return(key)
}



#' An S4 class implementing handling queries to objectstores derived from VirtualRemoteObjectStore.
#'
#' Inherits from "ObjectQuery"

setClass(
	Class = "RemoteObjectQuery",
	slots = c(
	  db_name = "character",
	  db_schema = "character",
	  tb_name   = "character",
	  sql_query = "BlobStorage.SQLProcedureCall",
	  sql_insert = "BlobStorage.SQLProcedureCall"
	),
	prototype = list(
	  db_name = .__DEFAULT_OBJECTSTORE_ODBC_DB_NAME__.,
	  db_schema = .__DEFAULT_OBJECTSTORE_FILE_DB_SCHEMA__.
	),
	contains = c("VirtualSQLQueryHandler",
	             "VirtualSQLInsertHandler",
	             "VIRTUAL")
)





setGeneric("getRemoteObjectQueryKeyColumnNames",
           function(object){standardGeneric("getRemoteObjectQueryKeyColumnNames")})
setMethod("getRemoteObjectQueryKeyColumnNames",
          signature(object = "ObjectQuery"),
          function(object){
            query <- getSQLQueryObject(object)

            fields <- getSQLQueryKeyColumnNames(query)

            return(fields)
          }
)

setGeneric("getRemoteObjectInsertKeyColumnNames",
           function(object){standardGeneric("getRemoteObjectInsertKeyColumnNames")})
setMethod("getRemoteObjectInsertKeyColumnNames",
          signature(object = "ObjectQuery"),
          function(object){
            insert <- getSQLInsertObject(object)

            fields <- getSQLQueryKeyColumnNames(insert)

            return(fields)
          }
)


#' Initialize method for "RemoteObjectQuery" class
#'
#' @param .Object, object of class "RemoteObjectQuery"
#' @return \code{.Object} object of class "RemoteObjectQuery"
setMethod("initialize", "RemoteObjectQuery",
          function(.Object){
            sql_query <- new("BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate",
                             .getObjectQueryDBName(.Object),
                             .getObjectQuerySchemaName(.Object),
                             .getObjectQueryTableName(.Object))
            .Object <- setSQLQueryObject(.Object, sql_query)

            sql_insert <- new("BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate",
                              .getObjectQueryDBName(.Object),
                              .getObjectQuerySchemaName(.Object),
                              .getObjectQueryTableName(.Object))
            .Object <- setSQLInsertObject(.Object, sql_insert)

            return(.Object)

          }
)

#' Set sql_query object in object slot
#'
#' Private method to set sql_query slot with "RemoteObjectQuery"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "RemoteObjectQuery"
#' @param sql_query object of class "BlobStorage.VirtualSQLProcedureCall"
#' @return \code{object} object of class "RemoteObjectQuery"

setMethod("setSQLQueryObject",
          signature(object = "RemoteObjectQuery",
                    sql_query = "BlobStorage.VirtualSQLProcedureCall"),
          function(object,sql_query){
            object <- TE.SQLQuery:::.setSQLQueryObject(object, sql_query)
            return(object)
            }
)

#' Set sql_insert object in object slot
#'
#' Private method to set sql_insert slot with "RemoteObjectQuery"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "RemoteObjectQuery"
#' @param sql_insert object of class "BlobStorage.VirtualSQLProcedureCall"
#' @return \code{object} object of class "RemoteObjectQuery"

setMethod("setSQLInsertObject",
          signature(object = "RemoteObjectQuery",
                    sql_insert = "BlobStorage.VirtualSQLProcedureCall"),
          function(object,sql_insert){
            object <- TE.SQLQuery:::.setSQLInsertObject(object, sql_insert)
            return(object)
          }
)


#' Get db name
#'
#' Returns character name of dB to be used for queries.
#'
#' @param object object of class 'RemoteObjectQuery'.
#' @return \code{db_schema} character

setGeneric(".getObjectQueryDBName", function(object){standardGeneric(".getObjectQueryDBName")})
setMethod(".getObjectQueryDBName",
          signature(object = "RemoteObjectQuery"),
          function(object){
            return(object@db_name)
          }
)

#' Get schema name
#'
#' Returns character name of dB schema to be used for queries.
#'
#' @param object object of class 'RemoteObjectQuery'.
#' @return \code{db_schema} character

setGeneric(".getObjectQuerySchemaName", function(object){standardGeneric(".getObjectQuerySchemaName")})
setMethod(".getObjectQuerySchemaName",
          signature(object = "RemoteObjectQuery"),
          function(object){
            return(object@db_schema)
          }
)

#' Get schema name
#'
#' Returns character name of dB schema to be used for queries.
#'
#' @param object object of class 'RemoteObjectQuery'.
#' @return \code{db_schema} character

setGeneric(".getObjectQueryTableName", function(object){standardGeneric(".getObjectQueryTableName")})
setMethod(".getObjectQueryTableName",
          signature(object = "RemoteObjectQuery"),
          function(object){
            return(object@tb_name)
          }
)

setGeneric("getKnownRemoteKeys",function(object,key){standardGeneric("getKnownRemoteKeys")})
setMethod("getKnownRemoteKeys","RemoteObjectQuery",
          function(object, key){

            sql_query <- getSQLQueryObject(object)

            tb_name <- .getObjectQueryTableName(object)

            key <- .generateRemoteQueryKey(object, key)
            key <- cbind(data.frame(TableName = tb_name), key)

            ret <- executeSQLQuery(sql_query, key)

            colnames(ret) <- TE.SQLQuery:::.translateSQLQueryColumnNames(sql_query,
                                                                         colnames(ret))

            return(ret)
          }
)

setGeneric(".generateRemoteQueryKey",function(object,key){standardGeneric(".generateRemoteQueryKey")})
setMethod(".generateRemoteQueryKey",
          signature(object = "RemoteObjectQuery",
                    key = "data.frame"),
          function(object,key){
            return(key)
          }
)

setGeneric("isKeyKnownInRemoteStore",function(object,key){standardGeneric("isKeyKnownInRemoteStore")})
setMethod("isKeyKnownInRemoteStore",
          signature(object = "RemoteObjectQuery",
                    key = "data.frame"),
          function(object,key){
            rval <- FALSE

            #key <- .generateRemoteQueryKey(object, key)

            test <- getKnownRemoteKeys(object, key)

            if(nrow(test)>0) {
              rval <- TRUE
            }
            return(rval)
          }
)


setGeneric("getFileTablePath",function(object,key){standardGeneric("getFileTablePath")})
setMethod("getFileTablePath","RemoteObjectQuery",
          function(object){
            db_name <- .getObjectQueryDBName(object)
            db_schema <- .getObjectQuerySchemaName(object)
            tb_name <- .getObjectQueryTableName(object)
            data_path <- get_referenced_filetable_path(tb_name, db_name, db_schema)

            return(data_path)
          }
)


setGeneric("updateKnownRemoteKeys",function(object,key){standardGeneric("updateKnownRemoteKeys")})
setMethod("updateKnownRemoteKeys","RemoteObjectQuery",
          function(object, key){
            sql_insert <- getSQLInsertObject(object)

            tb_name <- .getObjectQueryTableName(object)

            ret <- executeSQLQuery(sql_insert, key)

            message(sprintf("Result of Remote key insert into table: %s", tb_name))
            print(ret)

            if (is.null(ret$KeyAlreadyStored[1])) {
              message(sprintf("Could not update key in: %s", tb_name))
              print(ret)
              message(sprintf("Seems that file %s doesn't exist in file table for given query", key$FileName))
              print(key)

              ret <- FALSE
            } else {
              ret <- TRUE
            }

            return(ret)
          }
)


#' A VIRTUAL S4 class implementing basic functionalities of objectstore.
#'
#' Objectstore is an object that can store named datasets in separate environment
#' the datasets are stored and accessed using unique key values.
#' Objetstore also stores it's content to .Rds file and can be loaded later.
#' The API implements handling of reading and writing to the file.
#'
#' Prototype is made only once and then copied with each call to 'new'
#' for objects along the same branch in the class hierachy.
#' Must ensure new environments created since they are passed
#' by reference.
#'
#' @slot stored      "environment"
#' @slot id          "character"
#' @slot data_path   "character"

setClass(
	Class               = "VirtualRemoteObjectStore",
	representation	    = representation(
		remote_data_path  = "character"
	),
	contains = c("VirtualObjectStore", "VIRTUAL")

)

#' Initialize method for "VirtualRemoteObjectStore" class
#'
#' @param .Object, object of class "VirtualRemoteObjectStore"
#' @param id "character" name of the objectstore
#' @return \code{.Object} object of class "VirtualRemoteObjectStore"

setMethod("initialize", "VirtualRemoteObjectStore",
          function(.Object,id){
            .Object@stored <- new.env(parent = emptyenv())
            .Object@id <- id

            query <- getObjectStoreQuery(.Object)

            .Object@remote_data_path <- getFileTablePath(query)

            return(.Object)
          }
)


setGeneric("getRemotePath",function(object){standardGeneric("getRemotePath")})
setMethod("getRemotePath","VirtualObjectStore",
          function(object){
            return(object@remote_data_path)
          }
)


setGeneric("updateLocalStoreFile",function(object, key){standardGeneric("updateLocalStoreFile")})
setMethod("updateLocalStoreFile",
          signature(object = "VirtualRemoteObjectStore",
                    key    = "data.frame"),
          function(object, key){

            query <- getObjectStoreQuery(object)

            # select newest file if multiple stored remotely
            known_keys <- getKnownRemoteKeys(query, key)
            known_keys <- known_keys[order(known_keys$CreatedDate, decreasing = TRUE), ]
            newest.file <- known_keys$FileName[1]

            remote_path <- getFileTablePath(query)

            remote_file <- file.path(remote_path, newest.file)

            local_pth <- getPath(object)

            # Copy remote file to local name
            file_copied <- suppressWarnings(copyFile(remote_file,
                                                     local_pth,
                                                     overwrite = TRUE,
                                                     validate = TRUE,
                                                     verbose = FALSE))

            if (!file_copied) {
              message(sprintf("Copying file : %s,\n to location :%s,\ was not successful.",
                              remote_file, local_pth))
              stop(sprintf("Copying file : %s,\n to location :%s,\ was not successful.",
                           remote_file, local_pth))
            }

            return(object)
          }
)

setGeneric("saveObjectInRemoteStore",function(object){standardGeneric("saveObjectInRemoteStore")})
setMethod("saveObjectInRemoteStore",
          signature(object = "VirtualRemoteObjectStore"),
          function(object){

            browser()
            filename <- paste0(getID(object),
                               "_objectstore_",
                               today(),
                               "_",
                               .__DEFAULT_OBJECTSTORE_DB_USER__.,
                               ".rds")

            pth <- tempdir()

            pth <- file.path(pth, filename)
            # create local copy of the file.
            saveRDS(object@stored,pth)
            on.exit(file.remove(pth))

            # copy to remote file table
            query  <- getObjectStoreQuery(object)
            db     <- .getObjectQueryDBName(query)
            schema <- .getObjectQuerySchemaName(query)
            table  <- .getObjectQueryTableName(query)

            message(paste("Object store saving to path:",pth))
            rsp    <- store_file_in_referenced_filetable(pth, table, db, schema)

            if (rsp != 0) {
              message(sprintf("Object hasn't been saved in remote path: %s, code : %s",
                              get_referenced_filetable_path(table, db, schema),
                              rsp))
            }
            else(
              message(paste("Object store saved to remote path:",pth))
            )

            rsp <- check_file_stored_in_referenced_filetable(filename, table, db, schema)

            if (!rsp) {
              message(sprintf("Object isn't stored in remote path: %s, code",pth))
              stop(sprintf("Object isn't stored in remote path: %s, code",pth))
            }
            else {
              key <- key_from_name(getID(object))
              colnames(key) <- c("TraderID", "StartDate", "EndDate")
              key <- cbind(data.frame(TableName = table),
                           key,
                           data.frame(CreatedDate = today(),
                                      CreatedBy = .__DEFAULT_OBJECTSTORE_DB_USER__. ,
                                      FileName = filename))

              ret <- updateKnownRemoteKeys(query, key)

              if (ret) {
                message(paste("Keys updated successfully"))
              } else {
                message(sprintf("Problem with updating remote keys in object of class %s", class(object)))
              }

            }

            return(ret)

      }
)

setGeneric("removeObjectFromRemoteStore",function(object){standardGeneric("removeObjectFromRemoteStore")})
setMethod("removeObjectFromRemoteStore",
          signature(object = "VirtualRemoteObjectStore"),
          function(object){

            filename <- paste0(getID(object),
                               "_objectstore_",
                               today(),
                               "_",
                               .__DEFAULT_OBJECTSTORE_DB_USER__.,
                               ".rds")

            # copy to remote file table
            query  <- getObjectStoreQuery(object)
            db     <- .getObjectQueryDBName(query)
            schema <- .getObjectQuerySchemaName(query)
            table  <- .getObjectQueryTableName(query)

            rsp <- remove_file_from_referenced_filetable(filename, table, db, schema)

            return(rsp)

          }
)

setMethod("saveObject","VirtualRemoteObjectStore",
		  function(object){
		    pth <- getPath(object)


		  	message(paste("Object store saving to path:",pth))
		  	saveRDS(object@stored,pth)

		  	message(paste("Object store saving in remote store"))
		  	saveObjectInRemoteStore(object)

		  }
)


