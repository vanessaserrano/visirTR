options(install.packages.check.source = "no",
        shiny.maxRequestSize=5*1024^3)

pckgs<-c("rmarkdown","pander",
         "shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","DT","gplots",
         "xts","dygraphs","scales",
         "formattable","treemap","viridis","cat","gsubfn","vroom",
         "parallel")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
         quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

source("functions_VISIRDB.R")


ui <- dashboardPage( 
  skin="yellow",
  dashboardHeader(
    title="VISIR-DB",
    tags$li(class = "dropdown",
            style = "padding:16px; margin:auto;",
            textOutput("inProgress", inline =T)),
    tags$li(class = "dropdown",
            style = "padding:10px;",
            actionButton("cmdReport", "Create Global Report"))
  ),
  dashboardSidebar(
    sidebarMenu(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$head(tags$style('
      body {margin: auto; min-width:400px;}
      REMOVE_TO_ENABLE_a[href="#shiny-tab-widgets"] {
        z-index: -99999;
      }
      REMOVE_TO_ENABLE_a[href="#"] {
        z-index: -99999;
      }
      #StrucInfo {background-color:#ffda5b}
      #Glossary {background-color:#ffda5b}
      .skin-yellow .sidebar-menu>li.active>a, .skin-yellow .sidebar-menu>li:hover>a
                         {color: #000fff; background: #001F3F; border-left-color: #001F3F;}
      .small-box p {font-size: 14px;}
      .small-box h3 {font-size: 30px;}
      span#loadingReady.shiny-image-output.shiny-bound-output img,
      span#actionsReady.shiny-image-output.shiny-bound-output img,
      span#timeReady.shiny-image-output.shiny-bound-output img,
      span#circuitsReady.shiny-image-output.shiny-bound-output img,
      span#studentsReady.shiny-image-output.shiny-bound-output img,
      span#workingReady.shiny-image-output.shiny-bound-output img {background-color:#0073B7;}
      span#loadingReady.shiny-image-output.shiny-bound-output.recalculating img,
      span#actionsReady.shiny-image-output.shiny-bound-output.recalculating img,
      span#timeReady.shiny-image-output.shiny-bound-output.recalculating img,
      span#circuitsReady.shiny-image-output.shiny-bound-output.recalculating img,
      span#studentsReady.shiny-image-output.shiny-bound-output.recalculating img,
      span#workingReady.shiny-image-output.shiny-bound-output.recalculating img {background-color:#ff0000;}
      
      span#inProgress.shiny-text-output.shiny-bound-output {opacity:0.0;}
      span#inProgress.shiny-text-output.shiny-bound-output.recalculating {color:#001F3F; font-weight:600;
        animation: blinker 1s infinite;}
      
      @keyframes blinker {
        from { opacity: 1.0; }
        30% { opacity: 0.3; }
        to { opacity: 1.0; }}
      
      ')),
      menuItem("Data Input",icon = icon("home"), tabName = "browsedata"),
      menuItem("Global Results",icon = icon("folder"), startExpanded = FALSE,
        menuSubItem("Time", tabName = "timeana"),
        menuSubItem("Experiments", tabName = "numcircu"),
        menuSubItem("Experiments vs Time", tabName = "circvstim")),
      menuItem("Circuit-based Analysis",icon = icon("folder"), startExpanded  = FALSE,
        menuSubItem("Common Circuits",tabName = "common-circuits")),
      menuItem("User-specific Results",icon = icon("folder"), startExpanded  = FALSE,
        menuSubItem("User Results",tabName = "usresults")),
      menuItem("Work Indicators",icon = icon("folder"), startExpanded  = FALSE,
               menuSubItem("Observation Items",tabName = "obsitems"),
               menuSubItem("Assessment Milestones",tabName = "evmil")),
      menuItem("Help",icon = icon("question-circle"), startExpanded  = FALSE,
               actionButton("StrucInfo", "Dashboard Structure",icon = NULL, style='width:175px'),
               actionButton("Glossary", "Glossary of Terms",icon =  NULL, style='width:175px'))
    )
  ),
  dashboardBody(
    #boxes to be put in a row (or column)
    tabItems(
      tabItem("browsedata", 
        fluidRow(
          box(title = "Data Input", status = "warning", solidHeader = TRUE, 
              width = 4, collapsible = FALSE,
              fileInput('logsImport', 'Log File'),
              fileInput('obsItemsImport', 'Observation Items (optional)', accept = c('text/plain','.txt')),
              fileInput('evMilestonesImport', 'Assessment Milestones (optional)', accept = c('text/plain','.txt'))
          ),
          column(width = 7,
            box(solidHeader = TRUE, width = NULL, htmlOutput("logFiles")),
            fluidRow(width = NULL,
              valueBoxOutput("numStudents",width = 6),
              valueBoxOutput("numActions",width = 6),
              valueBoxOutput("mindate",width = 6),
              valueBoxOutput("maxdate",width = 6)),
            box(solidHeader = TRUE, width = NULL, htmlOutput("milestonesData")),
            box(solidHeader = TRUE, width = NULL, htmlOutput("evmilestonesData"))
          # ),
          # box(
          #   fluidRow(width=NULL, 
          #            tags$div("Data processing is finished when all indicators below are BLUE")),
          #   fluidRow(Width=NULL, align="center",
          #     imageOutput("loadingReady", width="100px", height="100px", inline=T),
          #     imageOutput("actionsReady", width="100px", height="100px", inline=T),
          #     imageOutput("timeReady", width="100px", height="100px", inline=T),
          #     imageOutput("circuitsReady", width="100px", height="100px", inline=T),
          #     imageOutput("studentsReady", width="100px", height="100px", inline=T),
          #     imageOutput("workingReady", width="100px", height="100px", inline=T)),
          #   width=12, align="center"
          )
        )
),

      tabItem("timeana",
        tabBox(height=600, width=12,
          tabPanel("Time Distribution", 
            fluidRow(
              valueBoxOutput("mintimespend",width = 3),
              valueBoxOutput("meantimespend",width = 3),
              valueBoxOutput("maxtimespend",width = 3),
              valueBoxOutput("totaltimespend",width = 3)
            ),
            fluidRow(
              box(status="primary", 
                  # tags$img(src = "working.gif", width=80, height=80,
                  #          style = "position:absolute;top:40px;left:50%;
                  #                   margin-left:-40px;margin-top:0px;"),
                  plotOutput("timstu"),
                  tags$br(),
                  textOutput("timstu_explained"),
                  height=600, width=12)
              
            )
          ),
          tabPanel( "Time per Date",
            fluidRow(box(status="primary",uiOutput("dygraph_ui"),
                         tags$br(),
                         textOutput("dygraph_explained"),
                         height=600, width=12))
          ),
          tabPanel( "Time per User and Date",
            fluidRow(
              box(status="primary",
                  verbatimTextOutput("plot_poin"),
                  uiOutput("plot"),
                  tags$br(),
                  textOutput("timeheat_explained"),
                  height=600, width=12)
            )
          )
        )  
      ),
  
      
      tabItem("numcircu",
              tabBox(height=600, width=12,
                     tabPanel( "Experiments Distribution",
                               fluidRow(
                                 valueBoxOutput("lowbound",width = 3),
                                 valueBoxOutput("Meannumuniquecircst",width = 3),
                                 valueBoxOutput("upbound",width = 3),
                                 valueBoxOutput("numuniquecirc",width = 3)
                               ),
                               fluidRow(
                                 box(status="primary",plotOutput("circdist"),
                                     tags$br(),
                                     textOutput("circdist_explained"),
                                     height=600, width=12)
                               )
                     ),
                     tabPanel( "Experiments per Date",
                               fluidRow(box(status="primary",
                                            uiOutput("dygraph2_ui"),
                                            tags$br(),
                                            textOutput("dygraph2_explained"),
                                            height=600, width=12))),
                     tabPanel( "Experiments per User and Date",
                               fluidRow(box(status="primary",
                                            verbatimTextOutput("plot_points"),
                                            uiOutput("plotui"),
                                            tags$br(),
                                            textOutput("circsuserheat_explained"),
                                            height=600, width=12))),
                     tabPanel( "Experimental Timelines",
                               fluidRow(box(status="primary",
                                            checkboxInput("timeline_facet","Show facetted"),
                                            verbatimTextOutput("plot_point"),
                                            uiOutput("plotu"),
                                            tags$br(),
                                            textOutput("timelineus_explained"),
                                            height=600, width=12))),
                     tabPanel("Unique Circuits Distribution", 
                              fluidRow(
                                box(status="primary",checkboxInput("simplified_distribution","Check to use simplified circuits (normalized circuits when unchecked)"),
                                    width = 12)),
                              fluidRow(
                                valueBoxOutput("lowboundN",width = 3),
                                valueBoxOutput("Meannumuniqnormcircst",width = 3),
                                valueBoxOutput("upboundN",width = 3),
                                valueBoxOutput("umuniqnormcirc",width = 3)
                              ),
                              fluidRow(
                                box(status="primary",
                                    plotOutput("circdistN"),
                                    tags$br(),
                                    textOutput("circdistN_explained"),
                                    height=600, width=12)
                              )  
                     )
              )
      ),
      
      tabItem("circvstim",
        tabBox(height=600, width=12,
          tabPanel( "Experiments vs Time",
            fluidRow(
              box(status="primary",
                  verbatimTextOutput("plot_pointsnAct"),
                  uiOutput("plotuinAct"),
                  textOutput("nActionsVStoT_explained"),
                  height=600, width=12)
            )
          ),
          
          tabPanel("Unique Circuits vs Time",
            fluidRow(
              box(status="primary",checkboxInput("simplified_time","Check to use simplified circuits (normalized circuits when unchecked)"),
                  verbatimTextOutput("plot_pointsnActnorm"),
                  uiOutput("plotuinActnorm"),
                  textOutput("nActionsVStoTnorm_explained"),
                  height=600, width=12)
            )
          )
        )
      ),
      

  tabItem("common-circuits",
          fluidRow(
            box(status = "primary",
                collapsible = FALSE, width=12,
              checkboxInput("simplified_common","Check to use simplified circuits (normalized circuits when unchecked)"),
              tabBox(height=600, width=12,
                 tabPanel("Common Circuits",
                          fluidRow(box(status="primary",
                                       textOutput("cc_circuits_explained"),
                                       tags$br(),
                                       dataTableOutput("cc_circuits"), 
                                       height=600, width=12))
                 ),
                 tabPanel("Circuit in Timeline",
                          fluidRow(box(status="primary",
                                       htmlOutput("ct_selectCircuit"),
                                       verbatimTextOutput("ct_plot_point"),
                                       uiOutput("ct_plotu"),
                                       tags$br(),
                                       textOutput("ct_plotu_explained"),
                                       height=600, width=12))
                 ),
                 tabPanel("Circuit per User",
                          fluidRow(box(status="primary",
                                       htmlOutput("ntc_selectCircuit"),
                                       verbatimTextOutput("ntc_plot_point"),
                                       uiOutput("ntc_plotu"),
                                       tags$br(),
                                       textOutput("ntc_plotu_explained"),
                                       height=600, width=12)))
            )))
  ),
  
  tabItem("usresults",
          fluidRow(
            box(status = "primary",
                collapsible = FALSE, height=750, width=12,
              htmlOutput("spr_selectStudent"), 
              tabBox(height=600, width=12,
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
                    valueBoxOutput("numerror",width = 3))
                ),
                tabPanel("List of Unique Circuits",
                 fluidRow(
                   box(status="primary",
                     checkboxInput("simplified_list","Check to use simplified circuits (normalized circuits when unchecked)"),
                     textOutput("lcn_circuits_explained"),
                     tags$br(),
                     dataTableOutput("lcn_circuits"), 
                                height=600, width=12))),
    
                tabPanel("History of Experiments",
                  fluidRow(box(status="primary",
                          textOutput("hc_circuits_explained"),
                          tags$br(),
                          dataTableOutput("hc_circuits"), 
                          height=600, width=12)))
  )))
  ),

  tabItem("obsitems",
          tabBox(height=600, width=12,
                 tabPanel("Group Performance",
                          fluidRow(
                            box(status="primary",
                                plotOutput("proportionbars"),
                                tags$br(),
                                textOutput("proportionbars_explained"),
                                height=600, width=12)
                          )),
                 tabPanel("Performance per User",
                          fluidRow(
                            box(status="primary",
                                plotOutput("heatmap"),
                                tags$br(),
                                textOutput("heatmap_explained"),
                                height=600, width=12))))),
  
  tabItem("evmil",
          tabBox(height=600, width=12,
                 tabPanel("Group Performance",
                          fluidRow(
                            box(status="primary",
                                plotOutput("evproportionbars"),
                                tags$br(),
                                textOutput("evproportionbars_explained"),
                                height=600, width=12)
                          )),
                 tabPanel("Performance per User",
                          fluidRow(
                            box(status="primary",
                                plotOutput("evheatmap"),
                                tags$br(),
                                textOutput("evheatmap_explained"),
                                height=600, width=12)))))
    )
  )
)  

