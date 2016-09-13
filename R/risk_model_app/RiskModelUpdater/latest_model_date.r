latestModelDate <- function(input, output, session, values){
  
    lookback <- getRiskModelLookback(values$risk_model)
  model_prefix <- getRiskModelPrefix(values$risk_model)
  date <- today()
  values$rmstr <- get_most_recent_model_objectstore(model_prefix, date ,lookback)
  if (!is.null(values$rmstr)) {
    values$rmstr_name <- getID(values$rmstr)
    values$model_date <- getMostRecentRiskModelDate(values$rmstr,values$rmstr_name, lookback)
    
    # kick off plot rendering
    if (!is.null(values$model_date) ){
      values$render_plot <- TRUE
    }
  } else {
    name  <- paste(model_prefix,format(today()-1,'%Y-%m'),sep="_")
    values$rmstr <- risk_model_objectstore_factory(name,lookback)
    values$model_date <- ""
  }
  
  first_rmstr <- get_earliest_model_objectstore(model_prefix, lookback) 
  
  if (!is.null(first_rmstr)){
    first_rmstr_name <- getID(first_rmstr)
    values$model_start_date <- getEarliestRiskModelDate(first_rmstr,first_rmstr_name, lookback)
  }
  else {
    values$model_start_date <- ""
  }
  output$text <- renderText({ 
    sprintf("Start Model Date: %s, Latest Model Date : %s", values$model_start_date, values$model_date)
  })
  
  updateDateRangeInput(session, "date_range", start = values$model_start_date,
                       end = values$model_date)
  
  callModule(statusInfo, "status_text",  paste("Loaded Risk Model Store : ", values$rmstr_name), session = session)
  
  return(values)
}