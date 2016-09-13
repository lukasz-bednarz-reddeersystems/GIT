#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(gtools)
library(ggplot2)
library(reshape2)
library(TE.DataAccess)
library(TE.RiskModel)

source("latest_model_date.r")
source("status_info.r")
source("auto_update_info.r")
source("latest_update_info.r")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # filepath <- file.path(Sys.getenv("TEMP"), "RiskModelUpdater.log")
  # 
  # log <- file(filepath, "w")
  # 
  # sink(file = log, type = "message")
  # sink(file = log, type = "output")
  # 
  values <- reactiveValues()
  values$model_date <- today() -1
  values$model_start_date <- NULL
  values$risk_model <- new("RiskModel.DevelopedEuropePrototype150")
  values$model_name <- ""
  values$rmstr_name <- ""
  values$trigger_update <- FALSE
  values$trigger_build <- FALSE
  values$trigger_load <- FALSE
  values$render_plot <- FALSE
  values$wake_up_timed_observer <- FALSE
  values$status_message <- "Welcome..."
  values$latest_update_message <- paste("No update since last launch on", now())
  values$auto_update_message <- "No auto update info"

  # event observer to trigger computation
  observe({
    
    if (!values$trigger_update) {
      return()
    } else {
      
      # update model
      model_date <- isolate(values$model_date)
      update_date <- today() -1
      while((wday(update_date) %in% c(7,1))) {
        update_date <- update_date - 1
      }
      
      if (update_date > model_date) {
        
        risk_model <- isolate(values$risk_model)
        model_builder <- new("RiskModelBuilder", risk_model = risk_model)
        
        updateRiskModelOnDate(model_builder, as.Date(update_date), FALSE, TRUE)
        
        values$latest_update_message <-  paste("Model Update last run on  ", now())
        values$trigger_load <- TRUE
        values$trigger_update <- FALSE
      }
    }
  })
  
  # event observer to trigger build
  observe({
    
    if (!values$trigger_build) {
      return()
    } else {
      # build model
      
      risk_model <- isolate(values$risk_model)
      
      model_builder <- new("RiskModelBuilder", risk_model = risk_model)
      
      dates <- isolate(input$date_range)

      buildRiskModelForDateRange  (model_builder, dates[1], dates[2])
      values$latest_update_message <-  paste("Model Update last run on  ", now())
      values$trigger_load <- TRUE
      values$trigger_update <- FALSE
      values$trigger_build <- FALSE

    }
  })
  
  
  # observer to trigger message update
  observeEvent(values$status_message, {
    callModule(statusInfo, "status_text",  
               values$status_message )
    
  }, priority = 9)
  
  # observer to trigger latest update info message update
  observeEvent(values$latest_update_message,{
    callModule(latestUpdateInfo, "latest_model_date_text",  
               values$latest_update_message)
    
  }, priority = 9)
  
  # observer to trigger auto update info message update
  observeEvent(values$auto_update_message,{
    callModule(autoUpdateInfo, "auto_update_text",  
               values$auto_update_message)
    
  }, priority = 9)
  
  # event observer to trigger load of latest model
  observe({
    if (!values$trigger_load) {
      return()
    } else {
      # update model
      date <- isolate(values$model_date)
      
      # isolate(values$progress$set(message = "Model Load triggered.. ", value =  0.1))
      
      values <- isolate(callModule(latestModelDate, "latest_model_date_text",  isolate(values), session = session))
      
      # isolate(values$progress$set(message = "Model Load finished.. ", value =  0.8))
      
      values$status_message <- paste("Latest module loaded : ", getID(isolate(values$rmstr)))
      values$trigger_load <- FALSE
      isolate(values$timed_observer$resume())
      
    }
  })
  
  
  # observer to trigger loading of model data and reading latest computeddate of the model
  # when model name or lookback dropdown is picked
  observe({
    values$risk_model <- new(input$select_model[[1]])
    values$model_name <- getRiskModelPrefix(values$risk_model)
    values$status_message <-   "Loading latest model info ..."
    values$trigger_load <- TRUE
    
  }, priority = 10)
  

  # observer for updating the plot with latest data
  observe({
    if(values$render_plot) {
      values$render_plot <- FALSE
      
      # isolate(values$progress$set(message = "Rendering plot.. ", value =  0.9))
      
      
      output$MarketStyle <- renderPlot({
        risk_model <- isolate(values$risk_model)
        lookback <- getRiskModelLookback(risk_model)
        rmstr_name <- isolate(values$rmstr_name)
        date <- isolate(values$model_date)
        market <- getRiskModelComponentOnDate(isolate(values$rmstr), rmstr_name, 'MarketStyle', date,  lookback)
        inp <- data.frame(factors = colnames(market)[-1], ir = unlist(market[1,-1]))
        # draw the market data
        plot(inp, main = "Market Style", las = 2)
        })
      
      # isolate(values$progress$set(message = "Plot Finished.. ", value =  1))
    }
    else {
      return()
    }
    
  })
  
  # observer to kick off update when the update button is pressed
  observeEvent(input$update_now_button,{
    date <- ymd(today()) -1
    
    if (date  == isolate(values$model_date)) {
      
      values$status_message <-  "Model is already up to date ..."

    } else {
      
      values$latest_update_message <- paste("Updating Model started at ", now())
      values$trigger_update <- TRUE
      
    }
  })
  
  
  # observer to kick off build when the update button is pressed
  observeEvent(input$build_now_button,{
    
    dates <- as.Date(isolate(input$date_range))
    
    if (is.null(dates[2])) {
      dates[2] <- today() -1
    }
    
    if (is.null(dates[1])) {
      # setting start date to end of previous month
      dates[1] <- dates[2]
      day(dates[1]) <- 1
      dates[1] <- dates[1] -1
    }
    
    values$latest_update_message <- paste("Building Model started at ", now())
    values$trigger_build <- TRUE

  })
  
  
  # observer to periodicaly check if latest model has computed
  values$timed_observer <- observe({
    
    
    # check every 4  hours
    model_date <- tryCatch({
      as.Date(values$model_date)
    }, error = function(cond){
      today() -1
    })
    
    update_date <- ymd(model_date) + 1
    
    while((wday(update_date) %in% c(7,1))) {
      update_date <- update_date + 1
    }
    
    # update daily after 6pm
    update_time <- as.POSIXct(as.Date(as.character(update_date)) + 1) + hours(5)
    update_time_passed <- (now() >= update_time )
    
    if (update_time_passed) {
      
      values$trigger_update <- TRUE
      values$latest_update_message <- paste("Updating Model started at ", now())
      
      values$auto_update_message <- paste("Automatic Update Triggered for model on", update_date)
      
      # trigger revalidation in case if 
      invalidateLater(500* 1000)
      
    } else {
      values$auto_update_message <- paste("Automatic Update scheduled for ", update_time)
      
      # invalidate until next update
      n_miliseconds <- as.numeric(update_time - now())* 3600 * 1000 
      invalidateLater(n_miliseconds)
    }
    

  }, suspended = TRUE, priority = -5)
  
})
