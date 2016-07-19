#Each feature derived from TradeFeature defines a datastore, data columns to fetch
#and a feature computation if applicable
#Features should return a frame of scalars for each trade indexed with trade date.
#Note that the upDateCompute method of VirtualFeature will rename computation
#(This is to ensure that feature binding works as expected).
sourceTo("../features/context_features.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/price_features.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/control_features.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)













































