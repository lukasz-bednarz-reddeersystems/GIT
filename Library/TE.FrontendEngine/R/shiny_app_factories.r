#' @include TE.FrontendEngine.r
NULL

#Determine how plotted quantities should be handled by the UI.
#Simplest case is just to go on the basis of type
entity_map <- list(Date = list("Date"),
                   Interval = list("difftime"),
                   Real = list("numeric"),
                   Integer = list("integer"),
                   Categorical = list("character","factor"),
                   Boolean = list("logical")
)

#Could be good to move the control build and the
#reactive data filtration callback build into this class
setClass(
  Class = "PlotEntityTypeMapper",
  representation = representation(
    type_map = "list"
  ),
  prototype = prototype(
    type_map = entity_map
  )
)
setGeneric("getUIType",function(object,variable){standardGeneric("getUIType")})
setMethod("getUIType","PlotEntityTypeMapper",
          function(object,variable){
            cnt  <- 1
            rval <- NULL
            nmes <- names(object@type_map)
            for(r_type_list in object@type_map){
              if(class(variable)[[1]] %in% r_type_list){
                rval <- nmes[cnt]
                break
              }
              cnt <- cnt + 1
            }
            return(rval)
          }
)

setClass(
  Class = "ShinyFactory",
  representation = representation(
    entity_mapper = "PlotEntityTypeMapper",
    page_type = "function",
    title = "function",
    body_function = "function",
    control_type  = "function",
    main_panel = "function",
    main_content = "function",
    inputIds = "list",
    plot_name = "character",
    content = "ANY",
    ui = "list",
    server = "function"
  ),
  prototype = prototype(
    entity_mapper = new("PlotEntityTypeMapper"),
    page_type = fluidPage,
    title = titlePanel,
    body_function = sidebarLayout,
    control_type  = sidebarPanel,
    main_panel = mainPanel,
    main_content = plotOutput
  )
)


#' generate UI Object
#'
#' @param object object of class "ShinyFactory"
#' @param analysis_ggplot "ggplot" object
#' @param analysis_data "data.frame" ggplot data
#' @param ui_options "list" with additional UI options
#'
#' @export

setGeneric("shinyUIFactory",function(object,analysis_ggplot,analysis_data,ui_options=list(omit=c('Value','PL'))){standardGeneric("shinyUIFactory")})

#' @describeIn shinyUIFactory
#' generate UI Object
#'
#' @inheritParams shinyUIFactory
#' @return \code{object} object of class "ShinyFactory"
#'
#' @export

setMethod("shinyUIFactory","ShinyFactory",
          function(object,analysis_ggplot,analysis_data,ui_options=list(omit=c('Value','PL'))){

            plot_name <- gsub(" ","",analysis_ggplot$labels$title)

            if (length(plot_name) == 0) {
              plot_name <- analysis_ggplot$labels$y

              if (length(plot_name) == 0) {
                plot_name <- "Analysis Plot"
              }
            }
            columnar <- length(ui_options[['row_layout']])>0
            object@plot_name <- plot_name
            object <- buildContent(object,analysis_ggplot,analysis_data,ui_options)
            if(columnar){
              object@ui[[1]] <- shinyUI(object@page_type(theme = shinythemes::shinytheme("darkly"),
                                                         #object@title(object@plot_name),
                                                         object@body_function(object@main_content(object@plot_name),
                                                                              object@content)
              ))

            } else {
              object@ui[[1]] <- shinyUI(object@page_type(theme = shinythemes::shinytheme("darkly"),
                                                         #object@title(object@plot_name),
                                                         object@body_function(object@control_type(object@content),
                                                                              object@main_panel(object@main_content(object@plot_name)))
              ))
            }
            return(object)
          }
)

setGeneric("buildContent",function(object,analysis_ggplot,analysis_data,ui_options){standardGeneric("buildContent")})
setMethod("buildContent","ShinyFactory",
          function(object,analysis_ggplot,analysis_data,ui_options){
            content <- list()
            columnar <- length(ui_options[['row_layout']])>0
            if(columnar){
              #List specifying columnar control layout
              for(control_row in ui_options[['row_layout']]){
                if(length(setdiff(control_row,ui_options[['omit']]))>12){
                  stop("Cant place more than 12 controls on a single row")
                }
                width <- round(12/length(setdiff(control_row,ui_options[['omit']])))
                row_controls <- list()
                for(input in setdiff(control_row,ui_options[['omit']])){
                  object@inputIds <- c(object@inputIds,input)
                  dynamic <- parseUIOptions(object,ui_options,'group_by',input)
                  if(length(dynamic)>0){
                    row_controls[[length(row_controls)+1]] <- column(width-1,offset=1,uiOutput(input))
                  } else {
                    row_controls[[length(row_controls)+1]] <- column(width-1,offset=1,buildInputControl(object,analysis_data,input,ui_options))
                  }

                }
                content[[length(content)+1]] <- fluidRow(tagList(row_controls))
              }
            } else {
              #Automatically omit columns named 'Value' unless overidden
              for(input in setdiff(colnames(analysis_data),ui_options[['omit']])){
                object@inputIds <- c(object@inputIds,input)
                dynamic <- parseUIOptions(object,ui_options,'group_by',input)
                if(length(dynamic)>0){
                  content[[length(content)+1]] <- uiOutput(input)
                } else {
                  content[[length(content)+1]] <- buildInputControl(object,analysis_data,input,ui_options)
                }
              }
            }
            object@content <- tagList(content)
            return(object)
          }
)

setGeneric("buildInputControl",function(object,analysis_data,input,ui_options){standardGeneric("buildInputControl")})
setMethod("buildInputControl","ShinyFactory",
          function(object,analysis_data,input,ui_options){
            label <- parseUIOptions(object,ui_options,input,"label")
            if(length(label)==0){
              label <- input
            }
            type <- getUIType(object@entity_mapper,analysis_data[[input]])
            if(type=="Date"){
              an <- parseUIOptions(object,ui_options,input,'animate')
              cntrl <- sliderInput(input,label,min=min(analysis_data[[input]],na.rm=TRUE),max=max(analysis_data[[input]],na.rm=TRUE),value=c(min(analysis_data[[input]],na.rm=TRUE),max(analysis_data[[input]],na.rm=TRUE)),animate=an,width='80%')
            } else if(type == "Interval"){
              cntrl <- sliderInput(input,label,min=min(as.numeric(analysis_data[[input]]),na.rm=TRUE),max=max(as.numeric(analysis_data[[input]]),na.rm=TRUE),value=c(min(as.numeric(analysis_data[[input]]),na.rm=TRUE),max(as.numeric(analysis_data[[input]]),na.rm=TRUE)))
            } else if(type == "Real"){
              cntrl <- sliderInput(input,label,min=min(analysis_data[[input]],na.rm=TRUE),max=max(analysis_data[[input]],na.rm=TRUE),value=c(min(analysis_data[[input]],na.rm=TRUE),max(analysis_data[[input]],na.rm=TRUE)))
            } else if(type == "Integer"){
              cntrl <- numericInput(input,label,min(analysis_data[[input]],na.rm=TRUE),min=min(analysis_data[[input]],na.rm=TRUE),max=max(analysis_data[[input]],na.rm=TRUE))
            } else if(type == "Categorical"){
              choices <- unique(as.character(analysis_data[[input]]))
              il <- length(choices)>5
              cntrl <- checkboxGroupInput(input,label,choices,selected=choices,inline=il)
            } else if(type == "Boolean"){
              choices <- list(True=TRUE,False=FALSE,All=NA)
              cntrl <- radioButtons(input, label,choices,selected='All')
            }
            return(cntrl)
          }
)

#ui_options is a structure enabling additional directives/informaation
#to be inserted: named list with the columns names as names and for each
#name a named list of shiny parameters as names and values
setGeneric("parseUIOptions",function(object,ui_options,input,shiny_parameter){standardGeneric("parseUIOptions")})
setMethod("parseUIOptions","ShinyFactory",
          function(object,ui_options,input,shiny_parameter){
            if(length(ui_options[[input]])>0){
              rval <- ui_options[[input]][[shiny_parameter]]
            } else {
              rval <- NULL
            }
            return(rval)
          }
)

#Build callbacks that return the reactive conponents to build in to the server
setGeneric("dataFilter",function(object,analysis_data){standardGeneric("dataFilter")})
setMethod("dataFilter","ShinyFactory",
          function(object,analysis_data){
            #filter the data
            reactive_data_callbacks <- list()
            for(i in object@inputIds){
              type <- getUIType(object@entity_mapper,analysis_data[[i]])

              if(type=='Date' || type=='Real'){
                reactive_data_callbacks[[length(reactive_data_callbacks)+1]] <- Curry(function(input,data,i){
                  data %>%
                    filter(data[[i]] >= input[[i]][1],
                           data[[i]] <= input[[i]][2]
                    )
                },i=i)
              } else if(type == 'Interval') {
                reactive_data_callbacks[[length(reactive_data_callbacks)+1]] <- Curry(function(input,data,i){
                  data %>%
                    filter(data[[i]] >= input[[i]][1],
                           data[[i]] <= input[[i]][2]
                    )
                },i=i)
              } else if(type == 'Integer') {
                reactive_data_callbacks[[length(reactive_data_callbacks)+1]] <- Curry(function(input,data,i){
                  data %>%
                    filter(data[[i]] == input[[i]])
                },i=i)
              } else if(type == 'Categorical') {
                reactive_data_callbacks[[length(reactive_data_callbacks)+1]] <- Curry(function(input,data,i){
                  data %>%

                    filter(data[[i]] %in% input[[i]])
                },i=i)
              } else if(type == 'Boolean') {
                reactive_data_callbacks[[length(reactive_data_callbacks)+1]] <- Curry(function(input,data,i){
                  if(nchar(as.character(input[[i]]))!=0){
                    data %>%
                      filter(data[[i]] == input[[i]])
                  } else { data }},i=i)
              }

            }
            data_fn <- function(input,data,reactive_data_callbacks){
              first <- TRUE
              for(rcallback in reactive_data_callbacks){
                if(first){
                  filtered_data <- rcallback(input,data)
                  first <- FALSE
                } else {
                  filtered_data <- rcallback(input,filtered_data)
                }
              }
              return(filtered_data)
            }
            return(Curry(data_fn,reactive_data_callbacks=reactive_data_callbacks,data=analysis_data))
          }
)

#Return a callback that updates the ggplot object by replacing its data with the filtered data
setGeneric("updatePlot",function(object,input,analysis_ggplot,reactive_data){standardGeneric("updatePlot")})
setMethod("updatePlot","ShinyFactory",
          function(object,input,analysis_ggplot,reactive_data){
            return(Curry(function(ggplot,input,reactive_data){
              ggplot$data <- reactive_data(input)
              ggplot <- ggplot +
                ggthemes::theme_hc(bgcolor = "darkunica") +
                ggplot2::theme(axis.title = ggplot2::element_text(face="bold")) +
                ggplot2::theme(axis.text  = ggplot2::element_text(colour = "white")) +
                ggplot2::scale_y_continuous(labels = comma)
              return(ggplot)
            },input=input,ggplot=analysis_ggplot,reactive_data=reactive_data))
          }
)

#Return a callback that updates the dynamic inputs
setGeneric("dynamicInputs",function(object,ui_options,analysis_data){standardGeneric("dynamicInputs")})
setMethod("dynamicInputs","ShinyFactory",
          function(object,ui_options,analysis_data){
            di_callback <- function(input,output,ui_options,data){
              if(length(ui_options[['group_by']])>0){
                gi <- names(ui_options[['group_by']])
                for(grouped_input in gi){
                  grouped_on <- ui_options[['group_by']][[grouped_input]]
                  output[[grouped_input]] <- renderUI({choices <- unique(as.character(data[data[[grouped_on]]%in%input[[grouped_on]],grouped_input]))
                                                       il <- length(choices)>5
                                                       checkboxGroupInput(grouped_input,grouped_input,choices,selected=choices,inline=il)})
                }
              }
              return(output)
            }
            return(Curry(di_callback,ui_options=ui_options,data=analysis_data))
          }
)

#' generate Server Object
#'
#' @param object object of class "ShinyFactory"
#' @param analysis_ggplot "ggplot" object
#' @param analysis_data "data.frame" ggplot data
#' @param ui_options "list" with additional UI options
#'
#' @export
setGeneric("shinyServerFactory",function(object,analysis_ggplot,analysis_data,ui_options=list()){standardGeneric("shinyServerFactory")})

#' @describeIn shinyServerFactory
#' generate Server Object
#'
#' @inheritParams shinyServerFactory
#' @return \code{object} object of class "ShinyFactory"
#'
#' @export
setMethod("shinyServerFactory","ShinyFactory",
          function(object,analysis_ggplot,analysis_data,ui_options=list()){
            reactive_data <- dataFilter(object,analysis_data)
            update_plot <- Curry(updatePlot,object=object,analysis_ggplot=analysis_ggplot,reactive_data=reactive_data)
            dynamic_inputs <- dynamicInputs(object,ui_options,analysis_data)
            object@server <- Curry(function(input,output,session,updater,plotname,dynamic){
              plt_fn <- updater(input)
              output <- dynamic(input,output)
              output[[plotname]] <- renderPlot(plt_fn())
              onSessionEnded(function() {stopApp()})
            },
            updater=update_plot,plotname=object@plot_name,dynamic=dynamic_inputs)
            return(object)
          }
)


#' Get stored UI Object
#'
#' @param object object of class "ShinyFactory"
#' @param analysis_ggplot "ggplot" object
#' @param analysis_data "data.frame" with data of ggplot
#'
#' @export

setGeneric("getUI",function(object,analysis_ggplot,analysis_data){standardGeneric("getUI")})

#' @describeIn getUI
#' Get stored UI Object
#'
#' @inheritParams getUI
#' @return \code{ui} shiny UI function
#'
#' @export
setMethod("getUI","ShinyFactory",
          function(object){
            return(object@ui[[1]])
          }
)


#' Get stored Server Object
#'
#' @param object object of class "ShinyFactory"
#' @param analysis_ggplot "ggplot" object
#' @param analysis_data "data.frame" with data of ggplot
#'
#' @export
setGeneric("getServer",function(object,analysis_ggplot,analysis_data){standardGeneric("getServer")})

#' @describeIn getServer
#' Get stored UI Object
#'
#' @inheritParams getServer
#' @return \code{ui} shiny UI function
#'
#' @export
setMethod("getServer","ShinyFactory",
          function(object){
            return(object@server)
          }
)
