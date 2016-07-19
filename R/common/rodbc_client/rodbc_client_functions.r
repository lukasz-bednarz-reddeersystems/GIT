sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(RODBC)
library(hash)
library(lubridate)


RISK_MODEL_DB <- new("RiskModelDefaults")@store_database
RAID_DB_USER <- Sys.info()["user"]

pass_thru_parser <- function(x){ return(x)}


convert_column_class <- function(df, classes = NULL) {
  
  class_map <- list(l ="integer", dbl = "numeric", s = "character", dt = "Date")
  setAs("character", "Date", function(from){ as.Date(from)}, where = environment())
  setAs("POSIXct", "Date", function(from){ as.Date(from)}, where = environment())
  
  cols <- colnames(df)
  
  res <- sapply(paste0("^", names(class_map)), function(x){grepl(x, cols)})
  class_names <- unlist(apply(res,1,function(x){class_map[x]} ) )
  names(class_names) <- cols
  
  ret_data <- as.data.frame(lapply(seq(length(class_names)), 
                                         function(x) {as(df[,x], class_names[x])}), stringsAsFactors = FALSE)
  colnames(ret_data) <- cols
  
  return(ret_data)
}


generate_procedure_call_strings <- function(proc_n, args, parsed_keys){
  
  SQL <- NULL
  sql <- proc_n
  
  colnames(parsed_keys) <- args
  
  for (row_idx in seq(nrow(parsed_keys))) {
    sql <- proc_n
    sql <- paste(sql, args[1], sprintf(" = '%s'",parsed_keys[row_idx, args[1]]))
    for( col in args[-1]) {
      sql <- paste(sql ,",",  col , sprintf(" = '%s'", parsed_keys[row_idx, col]))  
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

parse_start_date_keys <- function(key_values) {
  start <- min(key_values[,2])
  ret_val <- data.frame(start = start)
  
  return (ret_val)
}


get_bulk_price_data_by_instrument_list <- function(instruments,start,end){
  message("Price data bulk fetch from DB...")
  instr_str <- paste0(instruments, collapse = ",")
  SQL <- sprintf("prInstrumentHistory_SelectByInstrList '%s', '%s', '%s'", instr_str, start, end)
  
  price_data <- execute_sql_query(SQL)
  
  return(price_data)
}


execute_sql_query <- function(SQL, schema = NULL){
  cn <- odbcConnect(RISK_MODEL_DB,uid=RAID_DB_USER, DBMSencoding = "UTF-8")
  
  if (!is.null(schema)) {
    sqlQuery(cn, paste("USE", schema))
  }
  
  ret_data <- sqlQuery(cn,SQL, as.is = FALSE, stringsAsFactors  = FALSE)
  close(cn)
  
  return(ret_data)
}