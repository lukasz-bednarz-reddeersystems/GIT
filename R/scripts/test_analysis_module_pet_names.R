sourceTo("../analysis_modules/analysis_module_petnames_traded.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#debug(triggerVisualisationComputation)

key_func <- function(){three_monthly_lookback(70)}
pet_names_analysis <- createAnalysisModule(pet_names_analysis_module_builder,key_func)

pet_names_analysis <- updateAnalysisModel(pet_names_analysis)
pet_names_analysis <- runAnalysisModule(pet_names_analysis)

