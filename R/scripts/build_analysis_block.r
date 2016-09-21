library(TE.DataAccess)
library(TE.RiskModel)
library(TE.RefClasses)
library(TE.AnalysisClasses)
library(TE.FrontendEngine)

load_only  <- FALSE
block_name <- "PortfolioVarianceDecompositionAnalysisBlock"
id         <- 11L
date       <- '2016-08-01'
fn         <- dated_three_monthly_lookback

b <- new(paste(block_name,"Client",sep=""))
keys <- fn(id, date)
block_client <- dataRequest(block_client,keys,force=!load_only)
data <- block_client@analysis_block@ggplot_data
