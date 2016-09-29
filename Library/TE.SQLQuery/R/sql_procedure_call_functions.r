#' convert key values to sql procedure call
#'
#' @param proc_n "character" name of the stored procedure
#' @param args "character" vector names of the arguments
#' @param keys "data.frame"
#' @return \code{SQL} "character" vector of SQL procedure call
#' strings generated based on given parameters

generate_procedure_call_strings <- function(proc_n, args, keys){

  SQL <- NULL
  sql <- proc_n

  colnames(keys) <- args

  for (row_idx in seq(nrow(keys))) {
    sql <- proc_n
    sql <- paste(sql, args[1], sprintf(" = '%s'",keys[row_idx, args[1]]))
    for( col in args[-1]) {
      sql <- paste(sql ,",",  col , sprintf(" = '%s'", keys[row_idx, col]))
    }
    SQL[row_idx] <- sql
  }

  return(SQL)
}



parse_instrument_date_keys <- function(key_values) {
  instruments <- unique(key_values[,1])
  start <- min(key_values[,2])
  end <- max(key_values[,2])

  ret_val <- data.frame(InstrumentIDs = paste0(instruments, collapse = ","),
                        start = start,
                        end = end)

  return (ret_val)
}
