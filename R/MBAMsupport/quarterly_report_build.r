library("TE.Report")

key_values <- dated_twelve_monthly_lookback(11, '2016-10-01')
colnames(key_values) <- c('TraderID','start','end')
report <- new("PMStyleReport")
report <- dataRequest(report,key_values)
report <- Process(report)