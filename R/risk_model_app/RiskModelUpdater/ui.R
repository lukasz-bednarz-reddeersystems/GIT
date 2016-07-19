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
      selectInput("select_lookback", label = h3("Select Lookback"), 
                  choices = list("150" = 150), 
                  selected = 1),
      # model selection
      selectInput("select_model", label = h3("Select Model"), 
                  choices = list("Developed Europe (Prototype)" = "developed_europe_prototype"), 
                  selected = 1),
      actionButton("update_now_button", "Update Now", icon = icon("refresh"))
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("MarketStyle")
    )
  )
))
