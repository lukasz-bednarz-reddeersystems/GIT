sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

tryCatch(rm(dataplex_created),error=function(cond){})

instrument <- '4803'
dates <- c(as.Date('2015-02-02'),as.Date('2015-02-03'))
keys <- data.frame(lInstrumentID = instrument,dtDateTime = dates)
fct_data <- data_request("factor_datastore",keys,c("rDaysSinceLastResults"))

#instrument <- '5978'
#dates <- as.Date(unlist(Map(function(x)as.character(c(as.Date('2015-01-10')+x)),1:10)))
#keys <- data.frame(InstrumentID = instrument,DateTime = dates)
#ev_data <- data_request("event_datastore",keys,c("EventDateType"))

strat <- 'JS_LPOST'
dates <- as.Date(unlist(Map(function(x)as.character(c(as.Date('2015-01-10')+x)),1:10)))
keys <- data.frame(Strategy = strat, Date = dates)
epos <- data_request("ext_pos_datastore",keys,c("Age"))