#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

latestModelDateUI <- function(id){
  ns <- NS(id)
  textOutput(ns("text"))
}

statusInfoUI <- function(id){
  ns <- NS(id)
  textOutput(ns("text"))
}

autoUpdateInfoUI <- function(id){
  ns <- NS(id)
  textOutput(ns("text"))
}

latestUpdateInfoUI <- function(id){
  ns <- NS(id)
  textOutput(ns("text"))
}



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Risk Model Update Status"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      statusInfoUI("status_text"),
      latestModelDateUI("latest_model_date_text"),
      latestUpdateInfoUI("latest_update_text"),
      autoUpdateInfoUI("auto_update_text"),
      # model selection
      selectInput("select_model", label = h3("Select Model"), 
                  choices = list("Developed Europe (Prototype 1.1)" = "RiskModel.DevelopedEuropePrototype150.1.1",
                                 "Developed Europe (Prototype 1.0)" = "RiskModel.DevelopedEuropePrototype150"
                                 ), 
                  selected = 1),
      actionButton("update_now_button", "Update Now", icon = icon("refresh")),
      
      dateRangeInput('date_range',
                     label = 'DateRange of the model: yyyy-mm-dd',
                     start = "", end = ""
      ),

      actionButton("build_now_button", "(Re)Build Now", icon = icon("gears"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("MarketStyle")
    )
  )
))
