RISK_MODEL_DB <- function(){new("RiskModelDefaults")@store_database}
RAID_DB_USER <- function(){Sys.info()["user"]}

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


