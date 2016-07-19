latestModelDate <- function(input, output, session, values){
  lookback <- values$lookback
  model_name <- values$model_name
  date <- today()
  values$rmstr <- get_most_recent_model_objectstore(model_name, date ,lookback)
  values$rmstr_name <- getID(values$rmstr)
  values$model_date <- getMostRecentRiskModelDate(values$rmstr,values$rmstr_name, lookback)
  output$text <- renderText({ 
    paste("Last Model Date :", values$model_date)
  })
  
  callModule(statusInfo, "status_text",  paste("Loaded Risk Model Store : ", values$rmstr_name), session = session)
  
  return(values)
}