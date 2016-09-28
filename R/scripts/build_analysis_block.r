library(TE.DataAccess)
library(TE.RiskModel)
library(TE.RefClasses)
library(TE.AnalysisClasses)
library(TE.FrontendEngine)

load_only  <- FALSE
block_name <- "MarketStyleFactorStatisticAnalysisBlock"
id         <- 11
date       <- '2016-09-01'
fn         <- dated_twelve_monthly_lookback

block_client <- new(paste(block_name,"Client",sep=""))
keys <- fn(id, date)
block_client <- dataRequest(block_client,keys,force=!load_only)
data <- block_client@analysis_block@ggplot_data
plot <- block_client@analysis_block@ggplot
