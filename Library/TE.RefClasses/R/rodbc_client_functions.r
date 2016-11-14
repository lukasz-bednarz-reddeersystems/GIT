RISK_MODEL_DB <- function(){new("RiskModelDefaults")@store_database}
RAID_DB_USER <- function(){Sys.info()["user"]}

pass_thru_parser <- function(x){ return(x)}

parse_instrument_date_keys <- function(key_values) {
  instruments <- unique(key_values[,1])
  start <- min(key_values[,2])
  end <- max(key_values[,2])

  ret_val <- data.frame(InstrumentIDs = paste0(instruments, collapse = ","),
                        start = start,
                        end = end)

  return (ret_val)
}

parse_riskmodel_instrument_date_keys <- function(key_values) {
  risk_model <- unique(key_values[,1])
  instruments <- unique(key_values[,2])
  start <- min(key_values[,3])
  end <- max(key_values[,3])

  ret_val <- data.frame(RiskModelName = risk_model,
                        InstrumentIDs = paste0(instruments, collapse = ","),
                        start = start,
                        end = end)

  return (ret_val)
}

parse_riskmodel_date_keys <- function(key_values) {
  risk_model <- unique(key_values[,1])
  start <- min(key_values[,2])
  end <- max(key_values[,2])

  ret_val <- data.frame(RiskModelName = risk_model,
                        start = start,
                        end = end)

  return (ret_val)
}

parse_start_date_keys <- function(key_values) {
  start <- min(key_values[,2])
  ret_val <- data.frame(start = start)

  return (ret_val)
}
