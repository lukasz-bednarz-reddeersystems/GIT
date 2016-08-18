#' @include base_visualisation.r
#' @include visualisation_computation_functions.r
#' @include visualisation_functions.r
NULL

setClassUnion("AggregateGroupTypes",c("character","list"))
setClassUnion("AggregateFunctionTypes",c("function","list"))
setClassUnion("SubsetByEnumerable",c("character","list"))
setClass(
	Class = "FrequencyPlot",
	representation = representation(
		aggregate_what = 'character',
		aggregate_by = 'AggregateGroupTypes',
		aggregate_fn = 'AggregateFunctionTypes',
		subset_by = 'SubsetByEnumerable',
		subset_with = 'list',
		subset_fn  = 'list',
		rotate_x_labels = 'logical',
		x_label_variable = 'character',
		y_label = 'character',
		x_label = 'character'
	),
	prototype = prototype(
		aggregate_fn = sum,
		rotate_x_labels = FALSE,
		visuln_comp= data_aggregate_and_subset,
		visuln_dspl= frequency_plot
	),
	contains = c("BaseVisualisation")
)

