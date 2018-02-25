pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","wordcloud","tm","slam","diptest","DT","gplots",
         "googleVis","devtools","xts","dygraphs","scales",
         "formattable","treemap","Rcpp","plotly","yaml","viridis")
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

      menuItem("Data Input",icon = icon("dashboard"), tabName = "browsedata"),
      menuItem("Global Results",icon = icon("eye"), startExpanded = TRUE,
               
                        menuSubItem("Time", tabName = "timeana",icon = icon("area-chart")),
              
                        menuSubItem("Circuits", tabName = "numcircu",icon = icon("area-chart")),
                        
                        menuSubItem("Circuits vs Time", tabName = "circvstim",icon = icon("area-chart")),
                        
                        menuSubItem("Normalized Circuits vs Time", tabName = "ncircvstim",icon = icon("area-chart"))
      
      
      
    ),
    menuItem("Browse data",icon = icon("dashboard"), tabName = "browsedata")
    
  )),
  dashboardBody(    
                    tags$head(
                      tags$style(HTML('.skin-yellow .sidebar-menu>li.active>a, .skin-yellow .sidebar-menu>li:hover>a {
    color: #fff;
                                      background: #1e282c;
                                      border-left-color: #1e282c;
                                      }')),
                      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
                    
                    #boxes to be put in a row (or column)
                    tabItems(
                      
                      tabItem("browsedata", 
                              fluidRow(
                              
                              box(title = "Data Input", status = "warning", solidHeader = TRUE,width = 4,
                                  collapsible = TRUE,
                                  fileInput('logsImport', 'Log Files',accept=c('text/plain', '.txt') )
                              )
                              ,
                              
                             
                                
                                         
                                       valueBoxOutput("numStudents",width = 3),
                                       valueBoxOutput("numActions",width = 3),
                                       valueBoxOutput("mindate",width = 3),
                                       valueBoxOutput("maxdate",width = 3)
                                       
                                       
                                )
                              
                              
                      ),
                      
                      tabItem("numcircu",
                              
                              tabBox(height=480, width=12,
                                     
                                     
                                     tabPanel( "Circuits",
                                               
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
                      
                                     
                                     tabPanel( "Circuits vs Date",
                                               
                                               fluidRow(
                                                 
                                                 box(title="Total number of circuits per date",
                                                     status="primary",dygraphOutput("dygraph2"), height=480, width=12))),

                                     
                                     tabPanel( "Circuits vs User",
                                               
                                               fluidRow(
                                                 
                                                 box(title="Circuits vs User per Date",
                                                     status="primary",uiOutput("plotui"),verbatimTextOutput("plot_points"), height=480, width=12))),
                                     
                                     tabPanel( "Circuit Timeline vs User",
                                               
                                               fluidRow(
                                                 
                                                 box(title="Circuits timeline vs User per Date",
                                                     status="primary",uiOutput("plotu"),verbatimTextOutput("plot_point"), height=480, width=12))),
                                     
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
                                              
                                              
                                              
                                              
                                     )
                                     
                                     
                                     
                                     )
                              
                              
                      ),
                      
                      tabItem("timeana",
                              
                              tabBox(height=480, width=12,
                                     
                                     tabPanel("Time Distribution", 
                                              
                                              fluidRow(
                                                infoBoxOutput("totaltimespend",width = 3),
                                                infoBoxOutput("meantimespend",width = 3),
                                                infoBoxOutput("maxtimespend",width = 3),
                                                infoBoxOutput("mintimespend",width = 3)
                                                
                                              ) ,
                                              
                                              
                                              fluidRow(
                                                
                                                box(title="Time per Student Distribution",
                                                    status="primary",plotOutput("timstu"), height=480, width=12))),
                                     
                                     tabPanel( "Total Time vs Date",
                                               
                                               fluidRow(
                                                 
                                                 box(
                                                     status="primary",dygraphOutput("dygraph"), height=480, width=12)
                                                 
                                        
                                               )
                                     
                          
                                     ),
                                    
                                     tabPanel( "Time vs User",
                                               
                                               fluidRow(
                                                 box(title="Time vs User per Date",
                                                     status="primary",uiOutput("plot"),verbatimTextOutput("plot_poin"), height=480, width=12)
                                                 
                                               )
                                               
                                               
                                     )
                                     
                                                
                                                
                                                
                                              )  
                                              
                                              
                                              
                                              
                                     ),
                      
                      tabItem("circvstim",
                              
                              
                                fluidRow(
                                          
                                          box(title="Number of Circuits vs Time",
                                              status="primary",uiOutput("plotuinAct"),verbatimTextOutput("plot_pointsnAct"), height=480, width=12))),
                      
                      
                      tabItem("ncircvstim",
                              
                              
                              fluidRow(
                                
                                box(title="Number of Normalized Circuits vs Time",
                                    status="primary",uiOutput("plotuinActnorm"),verbatimTextOutput("plot_pointsnActnorm"), height=480, width=12)))
                      
                      
                              
                              
               
                                            
               
                                  

                               )
                    )
  )
  

