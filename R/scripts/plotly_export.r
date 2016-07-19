library(magrittr)
library(webshot)
library(htmltools)

plotly_html <- function(json){
  head <-'<html>
  <head>
  <meta charset="utf-8"/>
  <script src="C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts/htmlwidgets.js"></script>
  <script src="C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts/plotly-latest.min.js"></script>
  <script src="C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts/plotly.js"></script>
  </head>
  <body style="background-color:white;">
  <div id="htmlwidget_container">
  <div id="htmlwidget-5531" style="width:960px;height:500px;" class="plotly"></div>
  </div>
  <script type="application/json" data-for="htmlwidget-5531">{"x":'
  foot <- ',"evals":[]}</script>
  <script type="application/htmlwidget-sizing" data-for="htmlwidget-5531">{"viewer":{"width":450,"height":350,"padding":5,"fill":true},"browser":{"width":960,"height":500,"padding":5,"fill":true}}</script>
  </body>
  </html>'
  return(paste(head,json,foot,sep=""))
  }

plotly_export <- function(object,path,filename){
  t <- get("plotly_build", envir = asNamespace("plotly"))(object)
  json <- get("to_JSON", envir = asNamespace("plotly"))(t)
  html <- plotly_html(json)
  html_print(html) %>% 
    normalizePath(.,winslash="/") %>%
    gsub(x=.,pattern = ":/",replacement="://") %>%
    paste0("file:///",.) %>%
    webshot( file = paste(path,filename,sep=""), delay = 2)
}

plotly_export(p,figure_path,"test.png")