sourceTo("../features/preprocessor.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
	Class = "MktCapQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'MarketCap',
		compute = quartiler,
		key_cols= c('TradeID','MarketCapQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "MktCapQuartile",
	prototype = prototype(
		computation    = new("MktCapQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileMktCap",function(object,warehouse,features){standardGeneric("quartileMktCap")})
setMethod("quartileMktCap","MktCapQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "AgeQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'Age',
		compute = quartiler,
		key_cols= c('TradeID','AgeQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "AgeQuartile",
	prototype = prototype(
		gather_method  = getPositionSummary,
		computation    = new("AgeQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileAge",function(object,warehouse,features){standardGeneric("quartileAge")})
setMethod("quartileAge","AgeQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "CompoundReturnIntoQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'CompoundReturnInto',
		compute = quartiler,
		key_cols= c('TradeID','CompoundReturnIntoQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "CompoundReturnIntoQuartile",
	prototype = prototype(
		computation    = new("CompoundReturnIntoQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileCompoundReturnInto",function(object,warehouse,features){standardGeneric("quartileCompoundReturnInto")})
setMethod("quartileCompoundReturnInto","CompoundReturnIntoQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "TradeAgeDecileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'TradeAge',
		compute = deciler,
		key_cols= c('TradeID','TradeAgeDecile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "TradeAgeDecile",
	representation = representation(
		age_limit      = "integer"
	),
	prototype = prototype(
		computation    = new("TradeAgeDecileComputation"),
		age_limit      = as.integer(40)
		),
	contains = c('Fractiler')
)

setGeneric("decileTradeAge",function(object,warehouse,features){standardGeneric("decileTradeAge")})
setMethod("decileTradeAge","TradeAgeDecile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object@computation@input <- subset(object@computation@input,object@computation@input$TradeAge<=object@age_limit)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "PsnReturnInQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'PsnReturnIn',
		compute = quartiler,
		key_cols= c('TradeID','PsnReturnInQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "PsnReturnInQuartile",
	prototype = prototype(
		computation    = new("PsnReturnInQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartilePsnReturnIn",function(object,warehouse,features){standardGeneric("quartilePsnReturnIn")})
setMethod("quartilePsnReturnIn","PsnReturnInQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "Av.MarketValueDecileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on= 'Av.MarketValue',
		compute = deciler,
		key_cols= c('TradeID','Av.MarketValueDecile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "Av.MarketValueDecile",
	prototype = prototype(
		gather_method  = getPositionSummary,
		computation    = new("Av.MarketValueDecileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("decileAvMarketValue",function(object,warehouse,features){standardGeneric("decileAvMarketValue")})
setMethod("decileAvMarketValue","Av.MarketValueDecile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)			
			return(object)
		}
)

setClass(
	Class = "Av.MarketValueQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on= 'Av.MarketValue',
		compute = quartiler,
		key_cols= c('TradeID','Av.MarketValueQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "Av.MarketValueQuartile",
	prototype = prototype(
		gather_method  = getPositionSummary,
		computation    = new("Av.MarketValueQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileAvMarketValue",function(object,warehouse,features){standardGeneric("quartileAvMarketValue")})
setMethod("quartileAvMarketValue","Av.MarketValueQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object@computation@input$Av.MarketValue <- abs(object@computation@input$Av.MarketValue)
			object <- updateCompute(object)			
			return(object)
		}
)

setClass(
	Class = "VolIntoQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'VolInto',
		compute = quartiler,
		key_cols= c('TradeID','VolIntoQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "VolIntoQuartile",
	prototype = prototype(
		computation    = new("VolIntoQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileVolInto",function(object,warehouse,features){standardGeneric("quartileVolInto")})
setMethod("quartileVolInto","VolIntoQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "SkewIntoQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'SkewInto',
		compute = quartiler,
		key_cols= c('TradeID','SkewIntoQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "SkewIntoQuartile",
	prototype = prototype(
		computation    = new("SkewIntoQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileSkewInto",function(object,warehouse,features){standardGeneric("quartileSkewInto")})
setMethod("quartileSkewInto","SkewIntoQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)

setClass(
	Class = "AvgDailyValTraded5DayQuartileComputation",
	representation = representation(
		ntile_on = 'character'
	),
	prototype = prototype(
		ntile_on = 'AvgDailyValTraded5Day',
		compute = quartiler,
		key_cols= c('TradeID','AvgDailyValTraded5DayQuartile','Strategy','Instrument')
	),
	contains = c("PreprocessorComputation")
)

setClass(
	Class = "AvgDailyValTraded5DayQuartile",
	prototype = prototype(
		gather_method  = getPositionSummary,
		computation    = new("AvgDailyValTraded5DayQuartileComputation")
		),
	contains = c('Fractiler')
)

setGeneric("quartileAvgDailyValTraded5Day",function(object,warehouse,features){standardGeneric("quartileAvgDailyValTraded5Day")})
setMethod("quartileAvgDailyValTraded5Day","AvgDailyValTraded5DayQuartile",
	function(object,warehouse,features){
			object <- gatherFeatures(object,warehouse,features)
			object <- updateCompute(object)
			return(object)
		}
)
