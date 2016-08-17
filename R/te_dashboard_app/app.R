#setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/te_dashboard_app")
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinydashboard)

position_data <- readRDS("./position_data.rds")
trade_data <- readRDS("./trade_data.rds")
strategies <- unique(position_data$Strategy)
min_date <- min(position_data$TradeDate,na.rm=TRUE)
max_date <- max(position_data$TradeDate,na.rm=TRUE)
column_group <- list('TodayPL','ActiveTodayPL','PassiveTodayPL','MarketRelPL')
feature_group <- list('CompoundReturnOutof','CompoundReturnInto','ValueUSD','VolInto','VolOutof','SkewInto','SkewOutof','RSI14','TodayPL','MarketRelPL','PnLOutof','PsnAge','Hit','StockHit','DaysOffRel','DaysOffAbs','StockLoss','StockWin')
row_groups <- list(Strategy=list(JS=strategies[grep('JS_',strategies)],
                                 BA=strategies[grep('BA_',strategies)],
                                 DK=strategies[grep('DK_',strategies)],
                                 
                                 All=strategies),
                   TradeType=list(Increase=c('Increase'),
                                  Decrease=c('Decrease'),
                                  OpenLong=c('OpenLong'),
                                  OpenShort=c('OpenShort')))

compute_pl_cumulant <- function(all_pnl,raw_pnl,pltype,strat,first,colname,offtype=NULL){
  if(length(offtype)==0){
    strat_pnl <- raw_pnl[raw_pnl$Strategy==strat&raw_pnl$PLType==pltype,]  
  } else {
    strat_pnl <- raw_pnl[raw_pnl$Strategy==strat&raw_pnl$PLType==pltype&raw_pnl$OffType==offtype,]  
  }
  strat_pnl <- strat_pnl[order(strat_pnl$TradeDate),]
  strat_pnl$CumulativePL <- cumsum(strat_pnl[colname])
  if(first){
    all_pnl <- strat_pnl
  } else {
    all_pnl <- rbind(all_pnl,strat_pnl)
  }
  return(all_pnl)
}

multi_aggregate <- function(data,aggregate_columns,over_columns,with_fn,generic_column){
  if(length(aggregate_columns)>0){
    agg <- aggregate(data[aggregate_columns],over_columns,with_fn)
    if(length(aggregate_columns)>1){
      first <- TRUE
      for(col in aggregate_columns){
        other_cols <- setdiff(colnames(agg),aggregate_columns)
        un <- agg[c(col,other_cols)]
        colnames(un) <- c(generic_column,other_cols)
        un <- cbind(PLType=col,un)
        if(first){
          unrolled <- un 
          first <- FALSE
        } else {
          unrolled <- rbind(unrolled,un)
        }
      }
    } else {
      colnames(agg)[colnames(agg)==aggregate_columns] <- generic_column
      agg <- cbind(PLType=aggregate_columns,agg)
      unrolled <- agg
    }
  } else {
    unrolled <- data[data[names(aggregate_columns)[1]]==NULL,]
  }
  return(unrolled)
}

ui <- dashboardPage(
        dashboardHeader(title="Trading Enhancement"),
        dashboardSidebar( 
                          sidebarMenu(menuItem("Position management",tabName="position_management",icon=icon("bar-chart-o")),
                                      menuItem("Trade sizing",tabName="trade_sizing",icon=icon("balance-scale")))
                        ),
        dashboardBody(
          tabItems(
            tabItem(tabName="position_management",
              fluidRow(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   box(plotOutput("timeseries"),width=12)
                   ),
              fluidRow(
                      box(
                        
                        sliderInput("dateInput", "Date", min_date, max_date, c(min_date,max_date)),
                        checkboxGroupInput("columnSelector","PnL Breakdown",column_group,inline=TRUE),
                        width = 5
                      ),
                      box(
                        title="Offside bucket",
                        numericInput("offsAbs", "Days Abs. offside", 0,min = 1, max = 100),
                        numericInput("offsRel", "Days Rel. offside", 0,min = 1, max = 100),
                        width = 5
                      ),
                      box(
                        checkboxGroupInput("groupSelector","Strategy groups",names(row_groups[['Strategy']])),
                        width=2
                      )
                     ),
              fluidRow(
                      box(
                        title="All strategies",
                        checkboxGroupInput("strategySelector","Display strategies",strategies,inline=TRUE),
                      width=12
                      )
                     )
            ),
            tabItem(tabName="trade_sizing",
              fluidRow(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   box(plotOutput("trade_histograms"),width=12)
                   ),
              fluidRow(
                      box(
                        sliderInput("tradeDateInput", "Date", min_date, max_date, c(min_date,max_date)),
                        checkboxGroupInput("tradeFeatureSelector","Trade metrics",feature_group,inline=TRUE),
                        width=5
                      ),
                      box(
                        title="Trade selector",
                        checkboxGroupInput("tradeTypeSelector","Trade category",names(row_groups[['TradeType']])),
                        numericInput("tradeVisit", "Visit to stock", 0,min = 1, max = 10),
                        width=5
                      ),
                      box(
                        checkboxGroupInput("tradeGroupSelector","Strategy groups",names(row_groups[['Strategy']])),
                        width=2
                      )
                     )
            )
          )
    )
)    
                         


server <- function(input, output) {
  
  date_filtered <- reactive({
                             position_data %>%
                             filter(TradeDate >= input$dateInput[1],
                                    TradeDate <= input$dateInput[2]
                                   )
                            })
  
  trade_date_filtered <- reactive({
    trade_data %>%
      filter(TradeDate >= input$tradeDateInput[1],
             TradeDate <= input$tradeDateInput[2]
      )
  })
  
  pnl <- reactive({
                    if(length(input$strategySelector)>0){
                      strategy_filtered <- date_filtered() %>%
                        filter(Strategy%in%input$strategySelector)
                      if(input$offsAbs==0&&input$offsRel==0){
                        multi_aggregate(strategy_filtered,input$columnSelector,list(TradeDate=strategy_filtered$TradeDate,Strategy=strategy_filtered$Strategy),function(x)sum(x,na.rm=TRUE),'PL')
                      } else {
                        off_agg  <- multi_aggregate(strategy_filtered,input$columnSelector,list(TradeDate=strategy_filtered$TradeDate,
                                                                                Strategy=strategy_filtered$Strategy,
                                                                                OffType=unlist(Map(function(x)ifelse(x,'Offside','Onside'),strategy_filtered$DaysOffAbs>input$offsAbs&strategy_filtered$DaysOffAbs>input$offsRel))),
                                                                                function(x)sum(x,na.rm=TRUE),'PL')
                        off_data <- cbind(OffType='Total',multi_aggregate(strategy_filtered,input$columnSelector,list(TradeDate=strategy_filtered$TradeDate,
                                                                                                      Strategy=strategy_filtered$Strategy),
                                                                                                      function(x)sum(x,na.rm=TRUE),'PL'))
                        rbind(off_data,off_agg) 
                      }
                    } else {
                      strategy_filtered <- date_filtered() %>%
                        filter(Strategy%in%c('returnanemptyframe'))
                    }
                  })
  
  group_strategies <- reactive({unique(unlist(row_groups[['Strategy']][input$groupSelector]))})
  group_filtered <- reactive({
                              if(length(input$groupSelector)>0){
                                date_filtered() %>%
                                filter(Strategy%in%group_strategies())
                              } else {
                                date_filtered() %>%
                                filter(Strategy%in%c('returnanemptyframe'))
                              }
                             })
  
  trade_group_strategies <- reactive({unique(unlist(row_groups[['Strategy']][input$tradeGroupSelector]))})
  trade_group_filtered <- reactive({
    if(length(input$tradeGroupSelector)>0){
      trade_date_filtered() %>%
        filter(Strategy%in%trade_group_strategies())
    } else {
      trade_date_filtered() %>%
        filter(Strategy%in%c('returnanemptyframe'))
    }
  })
  
  grouped_pnl <- reactive({if(length(input$groupSelector)>0){
                                    all_data <- group_filtered()
                                    first <- TRUE
                                    for(group in input$groupSelector){
                                      group_name <- paste(group,"Group",sep="") 
                                      group_data <- all_data[all_data[['Strategy']]%in%row_groups[['Strategy']][[group]],]
                                      group_data['Strategy'] <- group_name
                                      if(first){
                                        grouped_data <- group_data
                                        first <- FALSE
                                      } else {
                                        grouped_data <- rbind(grouped_data,group_data)
                                      }
                                    }
                                    if(input$offsAbs==0&&input$offsRel==0){
                                     multi_aggregate(grouped_data,input$columnSelector,list(TradeDate=grouped_data$TradeDate,Strategy=grouped_data$Strategy),function(x)sum(x,na.rm=TRUE),'PL')
                                    } else {
                                      off_agg  <- multi_aggregate(grouped_data,input$columnSelector,list(TradeDate=grouped_data$TradeDate,
                                                                                         Strategy=grouped_data$Strategy,
                                                                                         OffType=unlist(Map(function(x)ifelse(x,'Offside','Onside'),grouped_data$DaysOffAbs>input$offsAbs&grouped_data$DaysOffAbs>input$offsRel))),
                                                                                         function(x)sum(x,na.rm=TRUE),'PL')
                                      off_data <- cbind(OffType='Total',multi_aggregate(grouped_data,input$columnSelector,list(TradeDate=grouped_data$TradeDate,
                                                                                                                    Strategy=grouped_data$Strategy),
                                                                                                                    function(x)sum(x,na.rm=TRUE),'PL'))
                                      rbind(off_data,off_agg)   
                                    }  
                            } else {
                              date_filtered() %>%
                              filter(Strategy%in%c('returnanemptyframe'))
                            }
                          })
  
  grouped_trades <- reactive({if(length(input$tradeGroupSelector)>0){
    all_data <- trade_group_filtered()
    first <- TRUE
    for(group in input$tradeGroupSelector){
      for(type in input$tradeTypeSelector){
        group_name <- paste(group,"Group",sep="")
        type_name <- paste(type,"Type",sep="")
        group_data <- all_data[(all_data[['Strategy']]%in%row_groups[['Strategy']][[group]])&(all_data[['TradeType']]%in%row_groups[['TradeType']][[type]]),]
        group_data['Strategy'] <- group_name
        group_data['TradeType'] <- type_name
        if(first){
          grouped_data <- group_data
          first <- FALSE
        } else {
          grouped_data <- rbind(grouped_data,group_data)
        } 
      }
    }
    if(input$tradeVisit==0){
      multi_aggregate(grouped_data,input$tradeFeatureSelector,list(Strategy=grouped_data$Strategy,TradeType=grouped_data$TradeType),function(x)mean(x,na.rm=TRUE),'Metric')
    } else {
      v_agg  <- multi_aggregate(grouped_data,input$tradeFeatureSelector,list(Strategy=grouped_data$Strategy,
                                                                             TradeType=grouped_data$TradeType,
                                                                             Visit=unlist(Map(function(x)ifelse(x,'ThisVisit','OtherVisits'),grouped_data$Visit==input$tradeVisit))),
                                                                             function(x)mean(x,na.rm=TRUE),'Metric')
      v_data <- cbind(Visit='Total',multi_aggregate(grouped_data,input$tradeFeatureSelector,list(Strategy=grouped_data$Strategy,
                                                                                                 TradeType=grouped_data$TradeType),
                                                                                                 function(x)mean(x,na.rm=TRUE),'Metric'))
      rbind(v_data,v_agg)   
    }  
    
   } else {
    date_filtered() %>%
      filter(Strategy%in%c('returnanemptyframe'))
  }
  })
  
  plot_pnl <- reactive({raw_pnl <- rbind(grouped_pnl(),pnl())
                        first <- TRUE
                        all_pnl <- NULL
                        if(input$offsAbs==0&&input$offsRel==0){
                          for(strat in unique(raw_pnl$Strategy)){
                            for(pltype in input$columnSelector){
                              all_pnl <- compute_pl_cumulant(all_pnl,raw_pnl,pltype,strat,first,'PL')
                              first <- FALSE  
                            }
                          }
                        } else {
                          for(offtype in unique(raw_pnl$OffType)){
                            for(strat in unique(raw_pnl$Strategy)){
                              for(pltype in input$columnSelector){
                                all_pnl <- compute_pl_cumulant(all_pnl,raw_pnl,pltype,strat,first,'PL',offtype)
                                first <- FALSE
                              }  
                            }
                          }
                        }
                        if(exists("all_pnl")){
                          all_pnl
                        } else {
                          NA
                        }
                       })
  
  output$timeseries <- renderPlot({if(!is.na(plot_pnl())){
                                    if(length(input$columnSelector)==1){
                                      if(input$offsAbs==0&&input$offsRel==0){
                                        ggplot(plot_pnl(), aes(x=TradeDate,y=CumulativePL,group=Strategy,colour=Strategy)) +
                                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                          geom_line(size=1)
                                      } else {
                                        ggplot(plot_pnl(), aes(x=TradeDate,y=CumulativePL,group=Strategy,colour=Strategy)) +
                                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                          geom_line(size=1) +
                                          facet_grid(~OffType)
                                      }  
                                    } else if(length(input$columnSelector)>1){
                                      if(input$offsAbs==0&&input$offsRel==0){
                                        ggplot(plot_pnl(), aes(x=TradeDate,y=CumulativePL,group=Strategy,colour=Strategy)) +
                                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                          geom_line(size=1) +
                                          facet_grid(PLType~.)
                                      } else {
                                        ggplot(plot_pnl(), aes(x=TradeDate,y=CumulativePL,group=Strategy,colour=Strategy)) +
                                          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                          geom_line(size=1) +
                                          facet_grid(PLType~OffType)
                                      }
                                    }
                                  }
                       })
  
  output$trade_histograms <- renderPlot({if(!is.na(grouped_trades())){
    if(length(input$tradeFeatureSelector)==1){
      if(input$tradeVisit==0){
        ggplot(grouped_trades(), aes(x=Strategy,fill=TradeType)) +
          geom_bar(aes(weight=Metric),position="dodge") 
      } else {
        ggplot(grouped_trades(), aes(x=Strategy,fill=TradeType)) +
          geom_bar(aes(weight=Metric),position="dodge") +
          facet_grid(~Visit,scales="free_y")
      }  
    } else if(length(input$tradeFeatureSelector)>1){
      if(input$tradeVisit==0){
        ggplot(grouped_trades(), aes(x=Strategy,fill=TradeType)) +
          geom_bar(aes(weight=Metric),position="dodge") +
          facet_grid(PLType~.,scales="free_y")
      } else {
        ggplot(grouped_trades(), aes(x=Strategy,fill=TradeType)) +
          geom_bar(aes(weight=Metric),position="dodge") +
          facet_grid(PLType~Visit,scales="free_y")
      }
    }
  }
  })
}


shinyApp(ui = ui, server = server)