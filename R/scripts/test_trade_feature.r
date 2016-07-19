sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

instrument <- '46136'
dates <- c(as.Date('2015-02-02'),as.Date('2015-02-20'))

earnings <- new("EarningsProximity")
earnings <- updateEarningsProximity(earnings,dates,instrument)
earnings <- updateCompute(earnings)
result   <- getOutPut(earnings)



