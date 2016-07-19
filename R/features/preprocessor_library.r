#Preprocessors aggregate features over a trade warehouse and perform some computation on them
#Preprocessors return datasets summarise this information.

sourceTo("../features/preprocessor_fractile.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

