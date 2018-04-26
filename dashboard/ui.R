pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","wordcloud","tm","slam","diptest","DT","gplots",
         "googleVis","devtools","xts","dygraphs","scales",
         "formattable","treemap","Rcpp","plotly","yaml","viridis")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}
library(cat)
if (!require("rCharts")) {
  install_github('ramnathv/rCharts', force= TRUE)
  #library("rCharts")
} 

source("dashboard_functiVISIR.R")

theme = "bootstrap.css"
ui <- dashboardPage( 
  skin="yellow",
  
  dashboardHeader(
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

        menuItem("Data Input",icon = icon("folder"), startExpanded = TRUE,
            menuSubItem("Log Files", tabName = "browsedata")),
      menuItem("Global Results",icon = icon("folder"), startExpanded = TRUE,
        menuSubItem("Time", tabName = "timeana"),
        menuSubItem("Circuits", tabName = "numcircu"),
        menuSubItem("Circuits vs Time", tabName = "circvstim")),
      menuItem("Circuit-based Analysis",icon = icon("folder"), startExpanded  = TRUE,
        menuSubItem("Common Circuits",tabName = "common-circuits")),
      menuItem("Users-specific Results",icon = icon("folder"), startExpanded  = TRUE,
        menuSubItem("Users Results",tabName = "usresults")),
      selectInput("Help", "Help", c(Choose = "", "Dashboard Information", "layout", "docs"), selectize = TRUE)
      
      
      
  )),
  dashboardBody(tags$head(
    tags$style(HTML('.skin-yellow .sidebar-menu>li.active>a, .skin-yellow .sidebar-menu>li:hover>a {
          color: #fff;
          background: #1e282c;
          border-left-color: #1e282c;
          }
          @media (min-width: 1500px)
.col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9 {
                    float: left;
                    }          
             @media (min-width: 1500px)
.col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9 {
                    float: left;
                    }       
                    
                    
                    
                    
                    ')),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
      
    tags$head(tags$style(
      "body{
      min-width: 1300px;
      margin: auto;
      
      
      
      }")
    ),
    
    
    #boxes to be put in a row (or column)
    tabItems(
      tabItem("browsedata", 
              fluidRow(
          box(title = "Data Input", status = "warning", solidHeader = TRUE,width = 4,
              collapsible = TRUE,
              fileInput('logsImport', 'Log Files')
          ),
          box(
            title = "Information", width = 4, solidHeader = TRUE,
            "To start press button Browse and load user traces"
          )
          
          ),
          fluidRow(
          valueBoxOutput("numStudents",width = 3),
          valueBoxOutput("numActions",width = 3)),
          
          fluidRow( 
          valueBoxOutput("mindate",width = 3),
          valueBoxOutput("maxdate",width = 3)
        )
      ),

      tabItem("timeana",
        tabBox(height=480, width=12,
          tabPanel("Time Distribution", 
            fluidRow(
              valueBoxOutput("totaltimespend",width = 3),
              valueBoxOutput("meantimespend",width = 3),
              valueBoxOutput("maxtimespend",width = 3),
              valueBoxOutput("mintimespend",width = 3)
            ),
            fluidRow(
              box(status="primary",plotOutput("timstu"), height=480, width=12)
            )
          ),
          tabPanel( "Total Time vs Date",
            fluidRow(box(status="primary",dygraphOutput("dygraph"), height=480, width=12))
          ),
          tabPanel( "Time vs User & Date",
            fluidRow(
              box(status="primary",uiOutput("plot"),verbatimTextOutput("plot_poin"), height=480, width=12)
            )
          )
        )  
      ),
  
      
      tabItem("numcircu",
              tabBox(height=480, width=12,
                     tabPanel( "Circuits Distribution",
                               fluidRow(
                                 valueBoxOutput("numuniquecirc",width = 3),
                                 valueBoxOutput("Meannumuniquecircst",width = 3),
                                 valueBoxOutput("lowbound",width = 3),
                                 valueBoxOutput("upbound",width = 3)
                               ),
                               fluidRow(
                                 box(status="primary",plotOutput("circdist"), height=480, width=12)
                               )
                     ),
                     tabPanel( "Circuits vs Date",
                               fluidRow(box(status="primary",dygraphOutput("dygraph2"), height=480, width=12))),
                     tabPanel( "Circuits vs User & Date",
                               fluidRow(box(status="primary",uiOutput("plotui"),verbatimTextOutput("plot_points"), height=480, width=12))),
                     tabPanel( "Circuit Timeline vs User",
                               fluidRow(box(status="primary",
                                            checkboxInput("timeline_facet","Show facetted"),
                                            uiOutput("plotu"),
                                            verbatimTextOutput("plot_point"),
                                            height=540, width=12))),
                     tabPanel("Normalized Circuits Distribution", 
                              fluidRow(
                                valueBoxOutput("umuniqnormcirc",width = 3),
                                valueBoxOutput("Meannumuniqnormcircst",width = 3),
                                valueBoxOutput("lowboundN",width = 3),
                                valueBoxOutput("upboundN",width = 3)
                              ),
                              fluidRow(
                                box(status="primary",plotOutput("circdistN"), height=480, width=12)
                              )  
                     )
              )
      ),
      
      tabItem("circvstim",
        tabBox(height=480, width=12,
          tabPanel( "Circuits vs Time",
            fluidRow(
              box(status="primary",uiOutput("plotuinAct"),verbatimTextOutput("plot_pointsnAct"), height=480, width=12)
            )
          ),
          
          tabPanel( "Normalized Circuits vs Time",
            fluidRow(
              box(status="primary",uiOutput("plotuinActnorm"),verbatimTextOutput("plot_pointsnActnorm"), height=480, width=12)
            )
          )
        )
      ),
      

  tabItem("common-circuits",
          tabBox(height=480, width=12,
                 tabPanel("Common Circuits",
                          fluidRow(box(status="primary",
                                       dataTableOutput("cc_circuits"), 
                                       height=480, width=12))
                 ),
                 tabPanel("Circuits in Timeline",
                          fluidRow(box(status="primary",
                                       htmlOutput("ct_selectCircuit"),
                                       uiOutput("ct_plotu"),
                                       verbatimTextOutput("ct_plot_point"),
                                       height=540, width=12))
                 ),
                 
                 tabPanel("Number of Circuits vs User",
                          fluidRow(box(status="primary",
                                       htmlOutput("ntc_selectCircuit"),
                                       uiOutput("ntc_plotu"),
                                       verbatimTextOutput("ntc_plot_point"),
                                       height=540, width=12)))
                 
                 
          )
  ),
  
  tabItem("usresults",
          fluidRow(
            box( status = "primary",width = 12,
                collapsible = TRUE,
                htmlOutput("spr_selectStudent")
            
  ,
  tabBox(height=540, width=12,
         tabPanel("Summary",
                  fluidRow(
  valueBoxOutput("numStudActions",width = 3),
  valueBoxOutput("timstud",width = 3),
  valueBoxOutput("numcircuit",width = 3),
  valueBoxOutput("numnormcircuit",width = 3)),
  
  fluidRow(
    valueBoxOutput("numresist",width = 3),  
    valueBoxOutput("numcurr",width = 3),
    valueBoxOutput("numvoltag",width = 3),
    valueBoxOutput("numerror",width = 3)
    
  )

  
  
  ),
  
        tabPanel("List of Normalized Ciruits",
                 
                 fluidRow(box(status="primary",
                              dataTableOutput("lcn_circuits"), 
                              height=480, width=12))),
  
  tabPanel("History of circuits",
           
           fluidRow(box(status="primary",
                        dataTableOutput("hc_circuits"), 
                        height=480, width=12)))
                 
                 
                 
                 
  
  
  )))
  
  
  
  
  )

  

          
  

    )
  )
)  

