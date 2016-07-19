setClass(
	Class          = "ScoreCardPanelComputationDefinition",
	representation = representation(
	  up_tag       = "character",
	  down_tag     = "character"
	),
	prototype      = prototype(
	  column_names = c('STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|Psns. up 1m','PERCENT_1DP|Psns. up 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m'),
	  name         = "Position Score Card.",
	  up_tag       = "Up",
	  down_tag     = "Down"
	),
	contains       = c("VirtualPanelComputationDefinition")
)

setGeneric("setScoreCardPanelFunction",function(object){standardGeneric("setScoreCardPanelFunction")})
setMethod("setScoreCardPanelFunction","ScoreCardPanelComputationDefinition",
	      function(object){
	      	#Will need to use explicit currying here...
	      	object@panel_comp <- function(module_data_list,name){
	      		return(position_level_scorecard(module_data_list,name,object@column_names,object@context_map,object@up_tag,object@down_tag))
	      	}
	      }
)