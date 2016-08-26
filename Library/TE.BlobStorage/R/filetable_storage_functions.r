#' @include TE.BlobStorage.r
#' @include filetable_query.r
NULL

.__DEFAULT_ODBC_DB_NAME__.   <- "RAIDSTAGEDB"
.__DEFAULT_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_FILE_DB_SCHEMA__. <- "FileTableDB"



#' Get unc share path of file table
#'
#' @param tb_name "character" name of the filetable
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
#' @return \code{path} "character" unc path of the file table storage
#'
#' @export
get_filetable_path <- function(tb_name,
                               db = .__DEFAULT_ODBC_DB_NAME__.,
                               schema = .__DEFAULT_FILE_DB_SCHEMA__.) {


  query <- new("SQLQuery.FileTableRootPath",
               db, schema, tb_name)

  ret <- executeSQLQuery(query)

  if (nrow(ret) == 0) {
     ret <- NULL
  } else {
    ret <- ret[1, "Path"]
  }

  return(ret)
}


#' check if file exists in filetable shared unc path
#'
#' @param filename "character" filename of file to be stored in filetable
#' @param tb_name "character" name of the filetable
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
#' @return \code{file_exists} "logical" TRUE if file is stored FALSE otherwise
#'
#' @export
check_file_exists <- function(filename,
                              tb_name,
                              db = .__DEFAULT_ODBC_DB_NAME__.,
                              schema = .__DEFAULT_FILE_DB_SCHEMA__.
                              ) {

  path <- get_filetable_path(tb_name, db, schema)

  file <- paste0(path, "\\", filename)
  file_exists <- file.exists(file)

  return(file_exists)
}


#' check if file is stored in database
#'
#' @param filename "character" filename of file to be stored in filetable
#' @param tb_name "character" name of the filetable
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
#' @return \code{is_stored} "logical" TRUE if file is stored FALSE otherwise
#'
#' @export
check_file_stored <- function(filename,
                                  tb_name,
                                  db = .__DEFAULT_ODBC_DB_NAME__.,
                                  schema = .__DEFAULT_FILE_DB_SCHEMA__.
                              ) {

  query <- new("SQLQuery.FileStoredInFileTable", db, schema, tb_name, filename)

  ret <- executeSQLQuery(query)

  is_stored <- NULL

  if (nrow(ret) == 0){
    is_stored <- FALSE
  } else if (nrow(ret) == 1) {
    ret.filename <- ret[1, "name"]

    if (ret.filename == filename) {
      is_stored <- TRUE

      file_exists <- check_file_exists(filename, tb_name, db, schema)

      if (!file_exists) {
        message(sprintf("File name query for file: %s indicates that it is stored but it doesn't exist",
                        filename))
        stop(sprintf("File name query for file: %s indicates that it is stored but it doesn't exist",
                     filename))
      }

    } else {
      message(sprintf("File name returned by query: %s \n doesn't match expected name: %s.",
                      ret.filename, filename))
      stop(sprintf("File name returned by query: %s \n doesn't match expected name: %s.",
                   ret.filename, filename))
    }

  } else {
    message(sprintf("File name returned by query  for file: returned more than one row : ",
                     filename, nrow(ret)))
    message(paste(ret["name"]))
    stop(sprintf("File name returned by query  for file: returned more than one row : ",
                         filename, nrow(ret)))
  }

  return(is_stored)
}


#' Store file in file table
#'
#' @param file "character" full file path of the file to be stored in filetable
#' @param tb_name "character" name of the filetable
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
#' @param overwrite "logical" should file be overwritten if already exists?
#' @return \code{status} "integer" status of saving:\cr
#'  \code{0} - new file has been stored\cr
#'  \code{1} - file has been overwritten\cr
#'  \code{-1} - file has not been stored as it already exists and \code{overwrite} param was set to FALSE
#'
#' @export
store_file_in_filetable <- function(file,
                                    tb_name,
                                    db = .__DEFAULT_ODBC_DB_NAME__.,
                                    schema = .__DEFAULT_FILE_DB_SCHEMA__.,
                                    overwrite = FALSE) {
  if(!file.exists(file)){
    message(sprintf("File to store : %s.\n Doesn't exist or is not accessible. Please check file location.",
                    file))
    stop(sprintf("File to store : %s.\n Doesn't exist or is not accessible. Please check file location.",
                 file))
  }

  filename <- basename(file)

  is_stored <- check_file_stored(filename, tb_name, db, schema)

  if (!is_stored) {
     overwrite <- FALSE
     ret <- 0
  } else if ( !overwrite) {
    message(sprintf("File : %s \n already exists in table %s. Use overwrite = TRUE to overwrite it.",
                    filename, tb_name))

    return(-1)
  } else {
    ret <- 1
  }

  path <- get_filetable_path(tb_name, db, schema)
  file_copied <- suppressWarnings(copyFile(file,
                                           path,
                                           overwrite = overwrite,
                                           validate = TRUE,
                                           verbose = FALSE))

  if (!file_copied) {
    message(sprintf("Copying file : %s,\n to location :%s,\ was not successful.",
                    file, path))
    stop(sprintf("Copying file : %s,\n to location :%s,\ was not successful.",
                 file, path))
  }

  return(ret)
}


#' Remove file from file table
#'
#' @param filename "character" filename of  file to be removed from filetable
#' @param tb_name "character" name of the filetable
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
#' @return \code{status} "integer" status of saving:\cr
#'  \code{0} - file has been removed\cr
#'  \code{-1} - file has not been stored and couldn't be removed
#'
#' @export
remove_file_from_filetable <- function(filename,
                                       tb_name,
                                       db = .__DEFAULT_ODBC_DB_NAME__.,
                                       schema = .__DEFAULT_FILE_DB_SCHEMA__.) {

  path <- get_filetable_path(tb_name, db, schema)

  is_stored <- check_file_stored(filename, tb_name, db, schema)

  if (!is_stored) {
    message(sprintf("File : %s \n is not stored in table : %s.",
                    filename, tb_name))
    ret <- -1
  } else {

    file <- file.path(path, filename)
    file.remove(file)

    is_stored <- check_file_stored(filename, tb_name, db, schema)

    if (is_stored){
      message(sprintf("File : %s.\n Couldn't be removed from filetable %s.",
                      file, tb_name))
      stop(sprintf("File : %s.\n Couldn't be removed from filetable %s.",
                   file, tb_name))
    }

    ret <- 0
  }

  return(ret)
}
