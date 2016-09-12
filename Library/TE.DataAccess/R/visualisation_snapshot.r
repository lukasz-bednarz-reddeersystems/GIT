#' @include base_visualisation.r
#' @include visualisation_computation_functions.r
#' @include visualisation_functions.r
NULL

setClass(
	Class = "SnapShot",
	representation = representation(
		aggregate_by = 'character',
		aggregate_fn = 'list',
		format_key  = 'list',
		subset_by   = 'character',
		subset_with = 'list',
		subset_fn   = 'list',
		rotate_x_labels = 'logical',
		snapshot_col_pre= 'character',
		window  = 'numeric',
		y_label = 'character',
		x_label = 'character',
		unique_by = 'character',
		options = 'character'
	),
	prototype = prototype(
		aggregate_fn = list(primary=function(x)mean(x,na.rm=TRUE),secondary=function(x)sd(x,na.rm=TRUE),secondary=function(x)-1*sd(x,na.rm=TRUE)),
		format_key = list(primary='type="o", col="blue"',secondary='type="o", lty=2, col="grey"'),
		rotate_x_labels = FALSE,
		visuln_comp= snapshot_aggregate_by_subset,
		visuln_dspl= aggregate_timeseries_plot,
		snapshot_col_pre= 't',
		window = 10,
		unique_by = c('TradeID')
	),
	contains = c("BaseVisualisation")
)
