autoUpdateInfo <- function(input, output, session, status_text){
  output$text <- renderText({status_text})
}