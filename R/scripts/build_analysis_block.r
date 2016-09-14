library(R.utils)
options(modifiedOnlySource=TRUE)
sourceTo("../common/analysis_client/analysis_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_client/client_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

load_only  <- FALSE
block_name <- "PositionRevisitsAnalysisBlock"
id         <- 11
date       <- '2016-07-01'
fn         <- dated_three_monthly_lookback

block_client <- new(paste(block_name,"Client",sep=""))
keys <- fn(id, date)
block_client <- dataRequest(block_client,keys,force=!load_only)
data <- block_client@analysis_block@ggplot_data