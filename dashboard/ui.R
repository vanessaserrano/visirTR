# Paquets a instal.lar/carregar
pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","wordcloud","tm","slam","diptest","DT","gplots",
         "googleVis","devtools","xts","dygraphs","scales",
         "formattable","treemap","Rcpp","plotly","yaml")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

if (!require("rCharts")) {
  install_github('ramnathv/rCharts', force= TRUE)
  library("rCharts")
}

source("dashboard_functiVISIR.R")
mydashboardHeader <- function(..., title = NULL, disable = FALSE,title.navbar=NULL, .list = NULL) {
  items <- c(list(...), .list)
  #lapply(items, tagAssert, type = "li", class = "dropdown")
  tags$header(class = "main-header",
              style = if (disable) "display: none;",
              span(class = "logo", title),
              tags$nav(class = "navbar navbar-static-top", role = "navigation",style = "logo{background-color:#FFF",
                       # Embed hidden icon so that we get the font-awesome dependency
                       span(shiny::icon("bars"), style = "display:none;"),
                       # Sidebar toggle button
                       #                        a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
                       #                          role="button",
                       #                          span(class="sr-only", "Toggle navigation")
                       #                        ),
                       
                       title.navbar,
                       div(class = "navbar-custom-menu",
                           tags$ul(class = "nav navbar-nav",
                                   items
                           )
                       )
              )
  )
}






theme = "bootstrap.css"
ui <- dashboardPage( 
  skin="yellow",
  mydashboardHeader(
    title="VISIR DASHBOARD"
    
  ),
  dashboardSidebar(
        sidebarMenu(
      tags$head(tags$style(HTML('
        a[href="#shiny-tab-widgets"] {
          z-index: -99999;
        }
        a[href="#"] {
          z-index: -99999;
        }
      '))),
      
      menuItem("Browse data",icon = icon("dashboard"), tabName = "browsedata"),
      menuItem("Global Results",icon = icon("eye"), startExpanded = TRUE,
              
                        menuSubItem("Circuits Distribution", tabName = "numcircu",icon = icon("area-chart")),
                        
                        menuSubItem("Circuits per Date", tabName = "overtime",icon = icon("area-chart")),
                        menuSubItem("Circuits vs Time", tabName = "circutime",icon = icon("area-chart")),
                        menuSubItem("Types of Mesure", tabName = "tyofmes",icon = icon("code-fork"))
                        
                         ,
               
             
                        menuSubItem("Time spent per Date", tabName = "timdate",icon = icon("area-chart")),
                        menuSubItem("Time/Student Distribution", tabName = "timstud",icon = icon("area-chart"))
               
      
      
      
    ),
    menuItem("Browse data",icon = icon("dashboard"), tabName = "browsedata")
    
  )),
  dashboardBody(    tags$style(HTML("
                                    
                                    
                                    .box.box-solid.box-primary>.box-header {
                                    color:#fff;
                                    background:#0073b7
                                    }
                                    
                                    .box.box-solid.box-primary{
                                    border-bottom-color:#0073b7;
                                    border-left-color:#0073b7;
                                    border-right-color:#0073b7;
                                    border-top-color:#0073b7;
                                    }
                                    
                                    ")),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                    
                    #boxes to be put in a row (or column)
                    tabItems(
                      
                      tabItem("browsedata", 
                              
                              
                              box(title = "Data Input", status = "warning", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  fileInput('logsImport', 'Log Files',accept=c('text/plain', '.txt') )
                              )
                              ,
                              
                              fluidRow(
                                column(width = 6,
                                       valueBoxOutput("numStudents"),
                                       valueBoxOutput("numActions")
                                       
                                ) )
                              
                              
                      ),
                      
                      tabItem("numcircu",
                              
                              tabBox(height=480, width=12,
                                     
                                     
                                     tabPanel( "Standard Circuits",
                                               
                                               fluidRow(
                                                 infoBoxOutput("numuniquecirc",width = 3),
                                                 infoBoxOutput("Meannumuniquecircst",width = 3),
                                                 infoBoxOutput("lowbound",width = 3),
                                                 infoBoxOutput("upbound",width = 3)
                                                 
                                               ),
                                               
                                               
                                               fluidRow(
                                                 
                                                 box(title="Number of circuits distribution",
                                                     status="primary",plotOutput("circdist"), height=480, width=12)
                                                 
                                                 
                                                 
                                               )
                                               
                                     ),
                                     tabPanel("Normalized Circuits", 
                                              
                                              fluidRow(
                                                infoBoxOutput("umuniqnormcirc",width = 3),
                                                infoBoxOutput("Meannumuniqnormcircst",width = 3),
                                                infoBoxOutput("lowboundN",width = 3),
                                                infoBoxOutput("upboundN",width = 3)
                                                
                                              ) ,
                                              
                                              
                                              fluidRow(
                                                
                                                box(title="Number of Normalized Circuits distribution",
                                                    status="primary",plotOutput("circdistN"), height=480, width=12)
                                                
                                                
                                                
                                              )  
                                              
                                              
                                              
                                              
                                     ))
                              
                              
                      ),
                      
                      tabItem("overtime", 
                              
                              fluidRow(
                                
                                box(title="Mean number of Ciruits(Standard, Closed and Normalized) per Student and per Date",
                                    status="primary",dygraphOutput("dygraph2"), height=480, width=12)
                                
                                
                                
                              )
                      ),
                      tabItem("circutime", 
                              
                              fluidRow(
                                
                                box(title="Number of Circuits vs Time on task ",
                                    status="primary",uiOutput("plotuinAct"),verbatimTextOutput("plot_pointsnAct"), height=480, width=12)
                                
                                
                                
                              )
                      ),
                      tabItem("tyofmes", 
                              
                              fluidRow(
                                
                                box(title="Number of normalized circuits per Type of mesure ",
                                    status="primary",height=480, width=12,
                                   showOutput("typmes", "Highcharts"))
                                    
     
                                
                                
                                )),
                      
                             tabItem("timdate", 
                                          
                                    fluidRow(
                                            
                                     box(title="Time(Minutes) spent per Student and per Date",
                                         status="primary",plotlyOutput("plotly"), height=480, width=12))),
                      
                      
                      
                              tabItem("timstud", 
                                      
                                      fluidRow(
                                        infoBoxOutput("totaltimespend",width = 3),
                                        infoBoxOutput("meantimespend",width = 3),
                                        infoBoxOutput("maxtimespend",width = 3),
                                        infoBoxOutput("mintimespend",width = 3)
                                        
                                      ),
            
                                      fluidRow(
                                        
                                        box(title="Time (Minutes) per Student Distribution",
                                            status="primary",plotOutput("timstu"), height=480, width=12)))
                                            
               
                                  

                               )
                    )
  )
  

