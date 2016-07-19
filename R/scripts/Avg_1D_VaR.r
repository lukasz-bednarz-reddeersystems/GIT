sourceTo("../common/RAIDdata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#Sample average 1D VaR computation.

#PDG LN, SOP FP, TL5 SM
test_instruments <- c(4950,5583,38964)
position_sizes <- c(1070000,1130000,599000)
on_date <- as.Date('2015-09-30')
start_date <- on_date - 90

ins <- 3
data_src <- new("InstrumentHistoryURL",instrument_ids=test_instruments[ins],start=start_date)
parser <- new("URLParser",parser_type = "XMLToFrame")
parser <- runURLs(parser,c(data_src@url))
prc_data <- getURLData(parser,1)
prc_data <- prc_data[prc_data$DataElement=='dblClosePrice',c('Value')]
rtns <- diff(log(as.numeric(prc_data)))
rtns <- exp(rtns)-1
mean_rtn <- mean(rtns)
std_rtn <- sqrt(var(rtns))

one_in_twenty_loss <- -1.96*std_rtn+mean_rtn
VaR <- one_in_twenty_loss*position_sizes[ins]