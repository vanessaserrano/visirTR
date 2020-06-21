options(install.packages.check.source = "no")

pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","DT","gplots",
         "xts","dygraphs","scales",
         "formattable","treemap","viridis","cat","gsubfn")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
         quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

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
        tags$head(
          tags$style(HTML('#StrucInfo{background-color:orange}'))
        ),
        tags$head(
          tags$style(HTML('#Glossary{background-color:orange}'))
        ),
       

        menuItem("Data Input",icon = icon("folder"), startExpanded = TRUE,
            menuSubItem("Logs & Work Indicators", tabName = "browsedata")),
      menuItem("Global Results",icon = icon("folder"), startExpanded = TRUE,
        menuSubItem("Time", tabName = "timeana"),
        menuSubItem("Circuits", tabName = "numcircu"),
        menuSubItem("Circuits vs Time", tabName = "circvstim")),
      menuItem("Circuit-based Analysis",icon = icon("folder"), startExpanded  = TRUE,
        menuSubItem("Common Circuits",tabName = "common-circuits")),
      menuItem("User-specific Results",icon = icon("folder"), startExpanded  = TRUE,
        menuSubItem("User Results",tabName = "usresults")),
      menuItem("Milestones",icon = icon("folder"), startExpanded  = TRUE,
               menuSubItem("Observation Items",tabName = "obsitems"),
               menuSubItem("Evaluation Milestones",tabName = "evmil")),
      menuItem("Help",icon = icon("question-circle"), startExpanded  = TRUE,
     
               actionButton("StrucInfo", "Dashboard Structure",icon = NULL, style='width:175px'),
               actionButton("Glossary", "Glossary of Terms",icon =  NULL, style='width:175px')
               )
      
      
      
  )),
  dashboardBody(tags$head(
    tags$style(HTML('.skin-yellow .sidebar-menu>li.active>a, .skin-yellow .sidebar-menu>li:hover>a {
          color: #fff;
          background: #1e282c;
          border-left-color: #1e282c;
          }
     
                    
                    
                    ')),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
      
    tags$head(tags$style(
      "body{
      
      margin: auto;
      
      
      
      }")
    ),
  
    
    #boxes to be put in a row (or column)
    tabItems(
      tabItem("browsedata", 
        fluidRow(
          box(title = "Data Input", status = "warning", solidHeader = TRUE,width = 4,
              collapsible = TRUE,
              fileInput('logsImport', 'Log Files'),
              fileInput('obsItemsImport', 'Observation Items (optional)', accept = c('text/plain','.txt')),
              fileInput('evMilestonesImport', 'Evaluation Milestones (optional)', accept = c('text/plain','.txt'))
          ),
          box(solidHeader = TRUE,
              "To start, load user traces by pressing button Browse"),
          mainPanel(
            verbatimTextOutput("milestonesData",placeholder=TRUE),
            verbatimTextOutput("evmilestonesData",placeholder=TRUE)
          )
        ),
        fluidRow(
          valueBoxOutput("numStudents",width = 3),
          valueBoxOutput("numActions",width = 3),
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
                                box(status="primary",checkboxInput("simplified_distribution","Simplified normalized circuit"),
                                    width = 12)),
                              fluidRow(
                                valueBoxOutput("umuniqnormcirc",width = 3),
                                valueBoxOutput("Meannumuniqnormcircst",width = 3),
                                valueBoxOutput("lowboundN",width = 3),
                                valueBoxOutput("upboundN",width = 3)
                              ),
                              fluidRow(
                                box(status="primary",
                                    plotOutput("circdistN"), height=390, width=12)
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
              box(status="primary",checkboxInput("simplified_time","Simplified normalized circuit"),
                  uiOutput("plotuinActnorm"),verbatimTextOutput("plot_pointsnActnorm"), height=480, width=12)
            )
          )
        )
      ),
      

  tabItem("common-circuits",
          tabBox(height=480, width=12,
                 tabPanel("Common Circuits",
                          fluidRow(box(status="primary",
                                       checkboxInput("simplified_common","Simplified normalized circuit"),
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
            box( status = "primary",
                collapsible = TRUE,
                htmlOutput("spr_selectStudent"), height=750, width=12
            
  ,
  tabBox(height=540, width=12,
         tabPanel("Summary",
                  fluidRow(
  valueBoxOutput("numStudActions",width = 3),
  valueBoxOutput("timstud",width = 3),
  valueBoxOutput("numcircuit",width = 3)),
  
  fluidRow(
    valueBoxOutput("numnormcircuit",width = 3),
    valueBoxOutput("numsimplcircuit",width = 3),
    valueBoxOutput("numresist",width = 3)),
  
  fluidRow(
    valueBoxOutput("numcurr",width = 3),
    valueBoxOutput("numvoltag",width = 3),
    valueBoxOutput("numerror",width = 3)
    
  )

  
  
  ),
  
        tabPanel("List of Normalized Ciruits",
                 
                 fluidRow(box(status="primary",
                              checkboxInput("simplified_list","Simplified normalized circuit"),
                              dataTableOutput("lcn_circuits"), 
                              height=480, width=12))),
  
  tabPanel("History of circuits",
           
           fluidRow(box(status="primary",
                        dataTableOutput("hc_circuits"), 
                        height=480, width=12)))
                 
                 
                 
                 
  
  
  )))
  
  
  
  
  ),
  tabItem("obsitems",
          tabBox(height=480, width=12,
                 tabPanel("Average Items",
                          fluidRow(box(status="primary", plotOutput("proportionbars"))
                            
                          )),
                 tabPanel("Heatmap"))),
  
  tabItem("evmil",
          tabBox(height=480, width=12,
                 tabPanel("Average Milestones",
                          fluidRow(box(status="primary")
                                   
                          )),
                 tabPanel("Heatmap")))
  

  

  

    )
  )
)  

