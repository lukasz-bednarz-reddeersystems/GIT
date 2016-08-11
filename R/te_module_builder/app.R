library(shiny)
source("../common/te_shiny_app_funcitons.r")
# This automatically configures a small shiny app using 
# a ggplot object and its associated data as built
# by analysis modules

#NB analysis modules appear to be saving to an object store
#so the object in question could be loaded here....
#1. Write an objectstore for the analysis block
#2. Write a client for the above object store
#3. dataRequest from the client here

#The client would wrap the analysis module functions in the dataplex 
#Ineffect this would simplify/cutout the step of building
#the key function

#These are going to need the ability to take other arguments, or perhaps
#better.... an object that handles the module options. 
ui    <- shinyUIFactory(analysis_ggplot,analysis_data)
server<- shinyServerFactory(analysis_ggplot,analysis_data)
shinyApp(ui = ui, server = server)