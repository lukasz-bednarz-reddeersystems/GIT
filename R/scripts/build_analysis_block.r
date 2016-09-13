library(R.utils)
options(modifiedOnlySource=TRUE)
sourceTo("../common/analysis_client/analysis_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_client/client_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

block_client <- new("OffsidePositionsAnalysisBlockClient")
keys <- dated_twelve_monthly_lookback(11, '2016-01-01')
#colnames(keys)[colnames(keys)=='id'] <- 'TraderID'
block_client <- dataRequest(block_client,keys,force=TRUE)
