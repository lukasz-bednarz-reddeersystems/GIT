#' @include TE.SQLQuery.r

.__DEFAULT_ODBC_DB_NAME__.   <- "RAIDSTAGEDB"
.__DEFAULT_DB_USER__.        <- Sys.info()["user"]
.__DEFAULT_FILE_DB_SCHEMA__. <- "Research"


#' passes argument to result
#'
#' @param x "ANY" pass through function
pass_thru_parser <- function(x){ return(x)}

#' Execute sql query
#'
#' @param SQL "character" query string
#' @param db "character" ODBC alias name of database
#' @param schema "character" name of DB schema
execute_sql_query <- function(SQL,
                              db = .__DEFAULT_ODBC_DB_NAME__.,
                              schema = .__DEFAULT_FILE_DB_SCHEMA__.){

  cn <- odbcConnect(db,uid=.__DEFAULT_DB_USER__., DBMSencoding = "UTF-8")
  on.exit(close(cn))

  ret_data <- tryCatch({
    sqlQuery(cn, paste("USE", schema))

    sqlQuery(cn,SQL, as.is = FALSE, stringsAsFactors  = FALSE)

  }, error = function(cond) {
    message(sprintf("Error occured when executing SQL query:\n\t%s", SQL))
    message(sprintf("Error message:\n%s", cond))
    stop(sprintf("Error occured when executing SQL query:\n\t%s", SQL))
  })

  return(ret_data)
}


#' check if data.frame has required columns
#'
#' @param data "data.frame" with data to test
#' @param required_colnms "character" vector with required column names
#' @return \code{result} "logical" TRUE if tested data.frame has required columns
#' FALSE otherwise

has_required_columns <- function(data, required_colnms) {
  columns <- colnames(data)

  if (all(required_colnms %in% columns)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
