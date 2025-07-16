function(input, output, session) {
  sessionID <- format(Sys.time(),"%Y%m%d%H%M%S")
  
  session$onSessionEnded(function() {
    save.image()
    files <- dir(pattern="[0-9]{14}.*[.]rda")
    file.remove(files)
    stopApp()
  })

  output$inProgress <- renderText({
    if(is.null(dfLoadLog()) | is.null(dfImport()) |
       is.null(dfActionCircuit()) | is.null(dfACxDate()) | is.null(dfACxStud()) |
       is.null(dfStudentsMilestonesEv())| T)
    "DATA BEING PROCESSED... PLEASE WAIT"
  })
  
  # status <- reactiveValues()
  # status$init <- F
  # status$loaded <- F
  # 
  # observe({
  #   if(status$init) {
  #     dfImport <- function() NULL
  #     print("init")
  #     status$loaded <- T
  #     status$init <- F
  #   }})
  # 
  # observe({
  #   if(status$loaded) {
  #     dfImport <- reactive({create_dfActions(dfLoadLog(),sessionID)})
  #     print("loaded")
  #     status$loaded <- F
  #   }})

#### CLEAN EXTERN. DATAFRAMES ####
  dfActionCircuit_ext <<- data.frame()
  dfActionsObsItems_ext <<- data.frame()
  dfActionTime_ext <<- data.frame()
  dfEvMilestones_xUser_ext <<- data.frame()
  dfImport_ext <<- data.frame()
  dfLoadLog_ext <<- data.frame()
  dfObsItems_xUser_ext <<- data.frame()
  sumxDate_ext <<- data.frame()
  sumxStud_ext <<- data.frame()
  sumxStudDate_ext <<- data.frame()
  
#### DEFINITIONS & CALCULATIONS ####
## Action & Student data ####
  dfLoadLog <- reactive({import_logFile(input$logsImport,sessionID)})
  dfImport <- reactive({create_dfActions(dfLoadLog(),sessionID)})
  dfActionTime <- reactive({create_dfActionTime(dfImport(), timeLimit = 900)})
  dfActionCircuit <- reactive({create_dfActionCircuit(dfActionTime())})
  dfACxStud <- reactive({aggreg_dfActionCircuit_xStud(dfActionTime(),dfActionCircuit())})
  dfACxStudDate <- reactive({aggreg_dfActionCircuit_xStudDate(dfActionTime(),dfActionCircuit(),sessionID)})
  dfACxDate <- reactive({aggreg_xDate(dfACxStudDate())})

  ## Normalized Circuits ####  
  tabNStudents <- reactive({
    tab <- dfACxStud()
    tab$Alumno <- as.character(tab$Alumno)
    tab <- tab[order(tab$Alumno),]
    data.frame(Student = tab$Alumno, NumExp = tab$NumExp, stringsAsFactors = FALSE) 
  }) 
  
  tabNCircuits <- reactive({
    tab <- table(na.omit(dfActionCircuit()$CircuitoNormalizado))
    tab <- tab[order(-tab)]
    # df1 <- data.frame(Circuit=dfActionCircuit()$CircuitoNormalizado)
    # df1 <- df1[!duplicated(df1$Circuit),]
    df <- data.frame(Circuit = names(tab), TimesTested = as.integer(tab),
               stringsAsFactors = FALSE)
    # df <- merge(df,df1)
    df <- df[order(df$TimesTested, decreasing=T),]
  })
  
  tabSCircuits <- reactive({
    tab <- table(na.omit(dfActionCircuit()$CircuitoSimplificado))
    tab <- tab[order(-tab)]
    # df1 <- data.frame(Circuit=dfActionCircuit()$CircuitoSimplificado)
    # df1 <- df1[!duplicated(df1$Circuit),]
    df <- data.frame(Circuit = names(tab), TimesTested = as.integer(tab),stringsAsFactors = FALSE) 
    # df <- merge(df,df1)
    df <- df[order(df$TimesTested, decreasing=T),]
})
  
  ## Work Indicators ####
  dfMilestonesDef<-reactive({
    inFile <- input$obsItemsImport
    if (is.null(inFile)) return(NULL) else {
      oiFileName_ext <<- inFile$name
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
    }
  })
  
  dfActionsMilestones <- reactive({
    inFile <- input$obsItemsImport
    if (is.null(inFile)) return(NULL) else
      generatedfActionsMilestones(dfActionCircuit(), dfMilestonesDef())
  })
  
  dfStudentsMilestones <- reactive({
    inFile <- input$obsItemsImport
    if (is.null(inFile)) return(NULL) else
      generatedfUsersMilestones(dfActionsMilestones(), dfMilestonesDef())
  })
  
  dfMilestonesEvDef<-reactive({
    inFile <- input$evMilestonesImport
    if (is.null(inFile)) return(NULL) else {
      emFileName_ext <<- inFile$name
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
    }
  })
  
  dfStudentsMilestonesEv <- reactive({
    generatedStudentsMilestonesEv(dfStudentsMilestones(),dfMilestonesEvDef())
  })
  
  dfOIColors <- reactive({
    getdfOIColors(dfMilestonesDef(),dfMilestonesEvDef())
  })
  
  # dfObsItemsLong <- reactive({
  #   dfObsLong <- dfActionsMilestones()
  #   if(is.null(dfObsLong)) return(NULL)
  #   if(input$checkbox) 
  #     dfObsLong <- rename(dfObsLong,student=user) %>% select(-filename)
  #   else
  #     dfObsLong <- rename(dfObsLong,student=filename) %>% select(-user)
  #   
  #   dfObsLong <- dfObsLong %>% 
  #     select(-application,-action,-session,-type,-param_name,-xml,
  #            -param_value,-diff_time,-xml_cum) %>% 
  #     gather("milestone","check",-(1:5)) %>% filter(check==TRUE) %>% 
  #     select(-check)
  #   dfObsLong <- dfObsLong %>% arrange(student,number)
  #   
  #   dfObsLong$numMil <- 1
  #   for(i in 2:nrow(dfObsLong)) {
  #     dfObsLong$numMil[i] <- 
  #       ifelse(dfObsLong$student[i]==dfObsLong$student[i-1],
  #              dfObsLong$numMil[i-1]+1,1)
  #   }
  #   dfObsLong
  # })
  

#### DATA INPUT ####
## Logs ####
  output$logFiles <- renderText({
    if(is.null(dfImport())) {
      "<strong>PLEASE START BY IMPORTING A LOG FILE THROUGH THE LEFT PANEL</strong>"
    } else {
      "Log file loaded, details are shown below..."
    }
  })
    
## Work Indicators ####
  output$milestonesData <- renderText({
    if(is.null(dfMilestonesDef())) {
      "<em>Observation items to be loaded... use the control on the left if needed.</em>"
    } else {
      paste("Read ",nrow(dfMilestonesDef()),
            " observation items<br />", paste(dfMilestonesDef()[,1],collapse = ", ") , sep="")
    }
  })
  output$evmilestonesData <- renderText({
    if(is.null(dfMilestonesEvDef())) {
      "<em>Assessment milestones to be loaded... use the control on the left if needed. Observation items, if available, will be used for evaluation.</em>"
    } else {
      paste("Read ",nrow(dfMilestonesEvDef()),
            " assessment milestones<br />", paste(dfMilestonesEvDef()[,1],collapse = ", "), sep="")
    }
  })

## Values ####  
  output$numStudents <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--",
             format(length(unique(isolate(dfImport()$Alumno))),format="d",big.mark="")), 
      "Users", color = "blue")
  })
  output$numActions <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--", 
             format(length(isolate(dfImport()$Alumno)),format="d",big.mark="")), 
      "Actions", color = "blue")
  })
  output$maxdate <- renderValueBox({
    valueBox(value = ifelse(is.null(dfImport()),"--",max(isolate(dfImport()$Dates))),
             width = 4, "Last logged date", color = "blue")
  })
  output$mindate <- renderValueBox({
    valueBox(value = ifelse(is.null(dfImport()),"--",min(isolate(dfImport()$Dates))),
             width = 4, "First logged date", color = "blue")
  })

### Ready ####
  # output$loadingReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfLoadLog()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  # 
  # output$actionsReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfImport()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  # 
  # output$timeReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfActionTime()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  # 
  # output$circuitsReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfActionCircuit()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  # 
  # output$studentsReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfACxDate()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  # 
  # output$workingReady <- renderImage({
  #   if(is.null(input$logsImport)) return(
  #     list(src="./www/redlight.png",
  #          width=30, height=30))
  #   if(is.null(dfStudentsMilestonesEv()) | T) Sys.sleep(1)
  #     list(src="./www/light.png",
  #          width=30, height=30)}, deleteFile=FALSE)
  
  # output$circuitsReady <- renderPlot(NULL)
  # output$usersReady  <- renderPlot(NULL)
  # output$timeReady  <- renderPlot(NULL)
  # output$indicatorsReady  <- renderPlot(NULL)
  
### Report ####
observeEvent(input$cmdReport, {
  if(is.null(dfActionTime()) |
     is.null(dfACxDate()) | is.null(dfACxStud()) |
     is.null(dfACxStudDate()) |
     is.null(dfStudentsMilestonesEv())) {
    showModal(modalDialog(
      title = "Report not available!",
      "Log file and work indicators (observation items and optionally 
      assessment milestones) data have to be loaded so a report 
      can be produced.",
      easyClose = TRUE
    ))
  } else {
    showReport()
  }
})
  
#### GLOBAL RESULTS ####
### >> TIME ####
## Distribution of time on task ####
# Total time spend
  output$totaltimespend <-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                   InfovalueBoxTT(dfACxStud())), 
      subtitle="Total time, in h",
      color = "navy")
  })
  
# Mean time spend
  output$meantimespend <-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                   InfovalueBoxMeT(dfACxStud())), 
      subtitle="Mean, among users, in h",
      color = "blue")
  })
  
# Max time spend
  output$maxtimespend <-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                   InfovalueBoxMaxT(dfACxStud())), 
      subtitle = "Maximum, among users, in h",
      color = "blue")
  })
  
# Min time spend
  output$mintimespend<-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                   InfovalueBoxMinT(dfACxStud())), 
      subtitle = "Mininum, among users, in h",
      color = "blue")
  })
  
  output$timstu <- renderPlot({
    if(is.null(dfACxStud())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      plotDistribution(dfACxStud()$TotalTime, xlabel="Time per User, in h")
    }
  })
  
  output$timstu_explained <- renderText({
    if(is.null(dfACxStud())) return(NULL)
    
    paste0("This chart shows the distribution of time of work of the VISIR users 
    (idle times larger than 15 minutes are discounted). It allows estimating the
    usual completion of the VISIR activities. Average time of work (",
    InfovalueBoxMeT(dfACxStud()),
    " h) is 
    presented as a dashed vertical line. For the pool of users, time of work goes
    from ",
    InfovalueBoxMinT(dfACxStud()),
    " h to ",
    InfovalueBoxMaxT(dfACxStud()),
    " h.")
  })
    
## Total Time vs Date (Dygraph) ####
  output$dygraph_ui <- renderUI({
    if(is.null(dfACxDate()))  {
      plotOutput("dygraph_gg")
    } else {
      if(nrow(dfACxDate())==1)  {
        plotOutput("dygraph_gg")
      } else {
        dygraphOutput("dygraph")
      }
    }
  })
  
  output$dygraph <-renderDygraph({
        createPlot_TimexDate(dfACxDate())
  })

  output$dygraph_gg <-renderPlot({
    if(is.null(dfACxDate()))  {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T,
                 size=5)+theme_void()
    } else {
      if(nrow(dfACxDate())==1)  {
        ggplot()+
          coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
          annotate("text", x=-1, y=1,
                   vjust=1, hjust=0, label='bold("THIS CHART IS ONLY AVAILABLE IF DATA EXPAND OVER MORE THAN ONE DAY")', parse=T, 
                   size=5)+theme_void()
      }
    }
  })
  
  output$dygraph_explained <- renderText({
    if(is.null(dfACxDate())) return(NULL)
    if(nrow(dfACxDate())==1) return(NULL)
    
    paste0("This chart shows total time of work per date. Time frame can be adjusted by
            using the slider at the bottom of the chart.")
    
  })
  
  
## Time on task vs User per Date -- HeatMap ####
  output$timeheat <-renderPlot({
    if(is.null(dfACxStudDate()))  {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      dfACxSD <- expand.grid(Dates=as.character(
        seq(min(as.Date(dfACxStudDate()$Dates)),max(as.Date(dfACxStudDate()$Dates)),by=1)),
        Alumno = levels(dfACxStudDate()$Alumno))
      dfACxSD <- merge(dfACxSD,dfACxStudDate(),all=T)
      
      dfACxSD$Alumno <- factor(dfACxSD$Alumno,
                               levels(dfACxStudDate()$Alumno),
                               ordered=T)
      
      g <- dfACxSD %>%  
        mutate (TimeInDate = round(TimeInDate/60, digits=2)) %>% 
        ggplot(aes(x = Alumno, y = Dates)) + theme_bw() +
        geom_tile(aes(fill=TimeInDate),width=0.9) + 
        labs(x="User", y="Date") +
        theme(axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              legend.title = element_text(size=14),
              legend.text= element_text(size=11),
              axis.text.x=element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
        scale_fill_viridis(name="Time, in h",direction=-1,
                           na.value=rgb(1,1,1,0))
  
      if(length(unique(dfACxStudDate()$Alumno)) > 50) g <- g +
        theme(axis.text.x = element_blank())
      if(length(unique(dfACxStudDate()$Dates)) > 30) g <- g +
        theme(axis.text.y = element_blank())
      g
    }
  })
  
  output$plot <- renderUI({
    if(is.null(dfACxStudDate())) {
      plotOutput("timeheat")
    } else {
      plotOutput("timeheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T))
    }
  })
  
  output$plot_poin <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfACxStudDate()$TimeInDate/60)
    dat$toT <- dfACxStudDate()$Alumno
    dat$nAc <- dfACxStudDate()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      threshold = 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.factor(res$nAc))
    responset <- unique(as.factor(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see user, date and time dedicated.") 
    else
      return (paste("User Id:",responset[1],"\t",
                   "Date:",responsec[1],"\t",
                   "Time, in h:",round(as.numeric(response[1]),2)))
    
  })
  
  output$timeheat_explained <- renderText({
    if(is.null(dfACxStudDate())) return(NULL)
    
    maxDateStud <- dfACxStudDate() %>% .[which.max(.$TimeInDate),]
    
    paste0("This chart shows the time of work per user and date. Darker colors 
    indicate larger amounts of working time. The highest peak of work intensity can be seen on ",
    maxDateStud$Dates, 
    " where user ",
    maxDateStud$Alumno,
    " spent ",
    round(maxDateStud$TimeInDate,0),
    " min using VISIR.")
  })
  
  
### >> EXPERIMENTS ####
## Experiments distribution ####
# Lower bound
  output$lowbound<-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                          InfovalueBoxMinExp(dfACxStud())),width = 4, 
      "Minimum, among users",color = "blue")
  })
  
# Mean number of circuits per user  
  output$Meannumuniquecircst <- renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                          InfovalueBoxMeanExp(dfACxStud())),width = 4, 
      "Mean, among users",color = "blue")
  })

# Upper bound 
  output$upbound<-  renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                          InfovalueBoxMaxExp(dfACxStud())),width = 4, 
      "Maximum, among users",color = "blue")
  })

# Total number of experiments
  output$numuniquecirc <- renderValueBox({
    valueBox(
      value=ifelse(is.null(dfACxStud()),"--",
                          InfovalueBoxTNumExp(dfACxStud())),width = 4, 
      "Total number of experiments", color = "navy")
  })
  
# Plot  
  output$circdist <- renderPlot({
    if(is.null(dfImport())) {
      ggplot()+
      coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
      annotate("text", x=-1, y=1,
               vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
               size=5)+theme_void()
    } else {
      plotDistribution(dfACxStud()$NumCircu, xlabel="Experiments per User")
    }
  })

  output$circdist_explained <- renderText({
    if(is.null(dfACxStud())) return(NULL)
    
    paste0("This chart shows the distribution of the number of experiments per user.
           Average number of experiments (",
           InfovalueBoxMeanExp(dfACxStud()),
           ") is 
    presented as a dashed vertical line. For the pool of users, the number of experiments goes
    from ",
           InfovalueBoxMinExp(dfACxStud()),
           " to ",
           InfovalueBoxMaxExp(dfACxStud()),
           ".")
  })
  
  
## Circuits per date (Dygraph) ####
  output$dygraph2_ui <- renderUI({
    if(is.null(dfACxDate()))  {
      plotOutput("dygraph2_gg")
    } else {
      if(nrow(dfACxDate())==1)  {
        plotOutput("dygraph2_gg")
      } else {
        dygraphOutput("dygraph2")
      }
    }
  })
  
  output$dygraph2 <-renderDygraph({
    createPlot_ExpxDate(dfACxDate())
  })
  
  output$dygraph2_gg <-renderPlot({
    if(is.null(dfACxDate()))  {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T,
                 size=5)+theme_void()
    } else {
      if(nrow(dfACxDate())==1)  {
        ggplot()+
          coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
          annotate("text", x=-1, y=1,
                   vjust=1, hjust=0, label='bold("THIS CHART IS ONLY AVAILABLE IF DATA EXPAND OVER MORE THAN ONE DAY")', parse=T, 
                   size=5)+theme_void()
      }
    }
  })
  
  output$dygraph2_explained <- renderText({
    if(is.null(dfACxDate())) return(NULL)
    if(nrow(dfACxDate())==1) return(NULL)
    
    paste0("This chart shows the number of experiments per date. Time frame can be adjusted by
            using the slider at the bottom of the chart.")
    
  })  
  
    
## Experiments per User and Date -- Heat map ####
  output$circsuserheat <-renderPlot({
    if(is.null(dfACxStudDate())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      dfACxSD <- expand.grid(Dates=as.character(
        seq(min(as.Date(dfACxStudDate()$Dates)),max(as.Date(dfACxStudDate()$Dates)),by=1)),
        Alumno = levels(dfACxStudDate()$Alumno))
      dfACxSD <- merge(dfACxSD,dfACxStudDate(),all=T)
      
      dfACxSD$Alumno <- factor(dfACxSD$Alumno,
                               levels(dfACxStudDate()$Alumno),
                               ordered=T)
    
      # dfACxSD[is.na(dfACxSD)] <- 0
      
      g <- ggplot(data = dfACxSD, aes(x = Alumno, y = Dates)) + theme_bw() +
        geom_tile(aes(fill=NumCircu), width=0.9) +
        labs(x ="User", y= "Date") +
        theme(axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              legend.title = element_text(size=14),
              legend.text= element_text(size=11),
              axis.text.x=element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_viridis(name="Experiments",direction=-1,
                           na.value=rgb(1,1,1,0))
      
      if(length(unique(dfACxStudDate()$Alumno)) > 50) g <- g +
        theme(axis.text.x = element_blank())
      if(length(unique(dfACxStudDate()$Dates)) > 30) g <- g +
        theme(axis.text.y = element_blank())
      g
    }
  })
  
  output$plotui <- renderUI({
    if(is.null(dfImport())) {
      plotOutput("circsuserheat")
    } else {
      plotOutput("circsuserheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
      )
    }
  })
  
  output$plot_points <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfACxStudDate()$NumExp)
    dat$toT <- dfACxStudDate()$Alumno
    dat$nAc <- dfACxStudDate()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      threshold = 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.factor(res$nAc))
    responset <- unique(as.factor(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see user, date and number of experiments.") 
    else
      return (paste("User Id:",responset[1],"\t",
                    "Date:",responsec[1],"\t",
                    "Experiments:",response[1]))
  })
  
  output$circsuserheat_explained <- renderText({
    if(is.null(dfACxStudDate())) return(NULL)
    
    maxDateStud <- dfACxStudDate() %>% .[which.max(.$NumExp),]
    
    paste0("This chart shows the number of experiments per user and date. Darker colors 
    indicate more experiments performed. The highest peak of performance can be seen on ",
           maxDateStud$Dates, 
           " where user ",
           maxDateStud$Alumno,
           " performed ",
           maxDateStud$NumExp,
           " VISIR experiments.")
  })
  
## Experimental Timelines ####
  output$timelineus <-renderPlot({
    if(is.null(dfActionCircuit())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      gra=c("red","green","blue","black")
      
      g <- dfActionCircuit() %>% mutate (Time = round(Time/60, digits=2)) %>% 
        ggplot(aes(x = Alumno, y = Time, 
                   color = Measure, shape= Measure)) + 
        geom_line(aes(x=Alumno, y=TotalTime, color=NULL, shape=NULL, group=1),
                  data=dfACxStud(), color="grey") + 
        geom_point(size = 4, alpha = 0.5) +
        labs(y="Time, in h", x = "User") +
        scale_color_manual(values = gra) +
        scale_shape_manual(values= c(95,95,95,13)) +
        theme(panel.background = element_rect(fill=NA), 
              panel.border = element_rect(colour="black",fill=NA),
              panel.grid.major.x = element_line(linetype=0),
              panel.grid.major.y = element_line(colour="lightgray",linetype="solid"),
              legend.key=element_blank(),
              legend.title = element_text(size=14),
              legend.text= element_text(size=11),
              axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              axis.text.x=element_text(angle = 90, vjust = 0.5))+
        guides(color = guide_legend(override.aes = list(size=9)))  
               
      if(input$timeline_facet) {
        g <- g + facet_wrap(~Measure,nrow=2) +
          theme(axis.text.x = element_blank(),
                strip.text= element_text(size=14),
                legend.position = "none")
      }
      
      if(nrow(dfACxStud())>50) g <- g + theme(axis.text.x = element_blank())
      
      g
    }
  })
  
  output$plotu <- renderUI({
    if(is.null(dfImport())) {
      plotOutput("timelineus")
    } else {
      plotOutput("timelineus", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
      )
    }
  })
  
  output$plot_point <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time/60
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      threshold = 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("In the non-facetted chart, place your mouse over a data point to identify the user.") 
    else
      return (paste("User:",response[1]))
  })
  
  output$timelineus_explained <- renderText({
    if(is.null(dfActionCircuit())) return(NULL)
    
    paste0("This chart shows the experiments performed by each user within their timeline.
            The experiment are represented by different markers according to the measurement taken.
            The line chart indicates the total time of work per user. The facetted version
            (checkbox at the top-left) presents each type of measurement in a different plot.
            Users are ordered by ascending total time of work.")
  }) 
  
## Unique Circuits ####
  #Low bound 
  output$lowboundN<-  renderValueBox({
    if(is.null(dfACxStud())) vVB <- "---"
    if(!is.null(dfACxStud())) {
      vVB <- ifelse(input$simplified_distribution,
                    InfovalueBoxMinSimp(dfACxStud()),
                    InfovalueBoxMinNorm(dfACxStud()))
    }
    valueBox(
      value=vVB, width = 4, 
      "Minimum, among users",color = "blue")
  })
  
#Mean 
  output$Meannumuniqnormcircst<-  renderValueBox({
    if(is.null(dfACxStud())) vVB <- "---"
    if(!is.null(dfACxStud())) {
      vVB <- ifelse(input$simplified_distribution,
                    InfovalueBoxMeanSimp(dfACxStud()),
                    InfovalueBoxMeanNorm(dfACxStud()))
    }
    valueBox(
      value=vVB, width = 4, 
      "Mean, among users",color = "blue")
  })

  #Upper bound 
  output$upboundN<-  renderValueBox({
    if(is.null(dfACxStud())) vVB <- "---"
    if(!is.null(dfACxStud())) {
      vVB <- ifelse(input$simplified_distribution,
                    InfovalueBoxMaxSimp(dfACxStud()),
                    InfovalueBoxMaxNorm(dfACxStud()))
    }
    valueBox(
      value=vVB, width = 4, 
      "Maximum, among users",color = "blue")
  })
  
# Total
  output$umuniqnormcirc<-  renderValueBox({
    if(is.null(dfActionCircuit())) vVB <- "---"
    if(!is.null(dfActionCircuit())) {
      vVB <- ifelse(input$simplified_distribution,
                    InfovalueBoxGNumSimp(dfActionCircuit()),
                    InfovalueBoxGNumNorm(dfActionCircuit()))
    }
    valueBox(
      value=vVB, width = 4, 
      "Number of unique circuits, in global",color = "navy")
  }) 
    
# Plot  
  output$circdistN <- renderPlot({
    if(is.null(dfImport())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      g <- plotDistribution(dfACxStud()$NumNCircu, xlabel="Normalized Circuits per User")
      
      if(input$simplified_distribution) {
        g <- plotDistribution(dfACxStud()$NumSCircu, xlabel="Simplified Circuits per User")
      }
      g
    }
  })
  
  output$circdistN_explained <- renderText({
    if(is.null(dfACxStud())) return(NULL)
    
    paste0("This chart shows the distribution of the number of unique circuits per user.
           Average number of ",
           ifelse(input$simplified_distribution,
                  "simplified","normalized"),
           " circuits (",
           ifelse(input$simplified_distribution,
                  InfovalueBoxMeanSimp(dfACxStud()),
                  InfovalueBoxMeanNorm(dfACxStud())),
           ") is presented as a dashed vertical line. For the pool of users, the number of ",
           ifelse(input$simplified_distribution,
                  "simplified","normalized"),
            " circuits goes from ",
           ifelse(input$simplified_distribution,
                  InfovalueBoxMinSimp(dfACxStud()),
                  InfovalueBoxMinNorm(dfACxStud())),
           " to ",
           ifelse(input$simplified_distribution,
                  InfovalueBoxMaxSimp(dfACxStud()),
                  InfovalueBoxMaxNorm(dfACxStud())),
           ".")
  })
  

### >> NUMBER OF CIRCUITS VS TIME ON TASK ####
## Number of experiments vs time ####
    output$nActionsVStoT <- renderPlot({
    if(is.null(dfImport())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      meanTT <- mean(dfACxStud()$TotalTime)
      meanNC <- mean(dfACxStud()$NumCircu)
      maxTT <- max(dfACxStud()$TotalTime)
      maxNC <- max(dfACxStud()$NumCircu)
      
      ggplot(dfACxStud(), aes(x=TotalTime, y=NumCircu))  +  
        geom_point(size = 3, alpha = 0.4) + 
        labs(x = "Time, in h", y = "Number of Experiments") +
        geom_hline(yintercept = mean(dfACxStud()$NumCircu), color="#fbada7")+
        geom_vline(xintercept = mean(dfACxStud()$TotalTime), color="#66d9dc") +
        geom_text(aes(x = (meanTT + maxTT)/2, y= meanNC, label=round(meanNC,digits = 2))) +
        geom_text(aes(x = meanTT, y= (meanNC + maxNC)/2, label=round(meanTT,digits = 2))) + 
        theme_bw() + 
        theme(axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    }
  })

  output$plotuinAct <- renderUI({
    if(is.null(dfImport())) {
      plotOutput("nActionsVStoT")
    } else {
      plotOutput("nActionsVStoT", height=350,
                 hover = hoverOpts(
                   id = "plot_hovernAct",
                   delay = 100,
                   nullOutside = T)
      )
    }
  })
  
  output$plot_pointsnAct <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfACxStud()$Alumno)
    dat$toT <- dfACxStud()$TotalTime
    dat$nAc <- dfACxStud()$NumCircu
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      threshold = 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify user, time and number of experiments.\n\n") 
    else
      return (paste("User Id:",response[1],"\t",
                    "Number of Experiments:",responsec[1],"\t",
                    "Time, in h:",responset[1]))
  })
  
  output$nActionsVStoT_explained <- renderText({
    if(is.null(dfACxStud())) return(NULL)
    
    paste0("This chart shows the relationship between the number of experiments
           and the total time or work. Each marker corresponds to one user.
           Average number of experiments (",
           round(mean(dfACxStud()$NumCircu),2),
           ") is presented as a red horizontal line. 
           Average total time of work per user (",
           round(mean(dfACxStud()$TotalTime),2),
           " h) is presented as a blue vertical line.")
  })
  
## Number of unique circuits vs time ####
  output$nActionsVStoTnorm <- renderPlot({
    if(is.null(dfACxStud())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      meanTT <- mean(dfACxStud()$TotalTime)
      maxTT <- max(dfACxStud()$TotalTime)
      
      meanNC <- mean(dfACxStud()$NumNCircu)
      maxNC <- max(dfACxStud()$NumNCircu)
      
      g <- ggplot(dfACxStud(), aes(x=TotalTime, y=NumNCircu)) +  geom_point(size = 3, alpha = 0.4) + 
        labs(x = "Time, in h", y = "Number of Normalized Circuits") + theme_bw() +
        geom_hline(yintercept = meanNC, color="#fbada7") +
        geom_vline(xintercept = meanTT, color="#66d9dc") + 
        geom_text(aes(x = (maxTT + meanTT)/2, y= meanNC, label=round(meanNC,digits = 2))) +
        geom_text(aes(x = meanTT, y= (maxNC + meanNC)/2, label=round(meanTT, digits = 2))) +
        theme(axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
    
      if(input$simplified_time) {
        meanNC <- mean(dfACxStud()$NumSCircu)
        maxNC <- max(dfACxStud()$NumSCircu)
        
        g <- ggplot(dfACxStud(), aes(x=TotalTime, y=NumSCircu)) +  geom_point(size = 3, alpha = 0.4) + 
          labs(x = "Time, in h", y = "Number of Simplified Circuits") + theme_bw() +
          geom_hline(yintercept = meanNC, color="#fbada7") +
          geom_vline(xintercept = meanTT, color="#66d9dc") + 
          geom_text(aes(x = (maxTT + meanTT)/2, y= meanNC, label=round(meanNC,digits = 2))) +
          geom_text(aes(x = meanTT, y= (maxNC + meanNC)/2, label=round(meanTT, digits = 2))) +
          theme(axis.title = element_text(size=14),
                axis.text= element_text(size=11),
                axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
      }
      
      g
    }
  })
  

  output$plotuinActnorm <- renderUI({
    if(is.null(dfImport())) {
      plotOutput("nActionsVStoTnorm")
    } else {
      plotOutput("nActionsVStoTnorm", height=350,
                 hover = hoverOpts(
                   id = "plot_hovernAct",
                   delay = 100,
                   nullOutside = T)
      )
    }
  })
  
  output$plot_pointsnActnorm <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfACxStud()$Alumno)
    dat$toT <- dfACxStud()$TotalTime
    dat$nAc <- dfACxStud()$NumNCircu
    dat <- as.data.frame(dat)
    g <- "Number of Normalized Circuits:"
    
    if(input$simplified_time) {
      dat <- data.frame(ids=dfACxStud()$Alumno)
      dat$toT <- dfACxStud()$TotalTime
      dat$nAc <- dfACxStud()$NumSCircu
      dat <- as.data.frame(dat)
      g <- "Number of Simplified Circuits:"
    }
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      threshold = 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the user.\n\n") 
    else
      return (paste("User Id:",response[1],"\t",
                    g,responsec[1],"\t",
                    "Time (in h):",responset[1]))
  })

  output$nActionsVStoTnorm_explained <- renderText({
    if(is.null(dfACxStud())) return(NULL)
    
    paste0("This chart shows the relationship between the number of unique circuits
           and the total time or work. Each marker corresponds to one user.
           Average number of ",
           ifelse(input$simplified_time,
                  "simplified","normalized"),
           " circuits (",
           ifelse(input$simplified_time,
                  InfovalueBoxMeanSimp(dfACxStud()),
                  InfovalueBoxMeanNorm(dfACxStud())),
           ") is presented as a red horizontal line. 
           Average total time of work per user (",
           round(mean(dfACxStud()$TotalTime),2),
           " h) is presented as a blue vertical line.")
  })
  
  
#### CIRCUIT-BASED RESULTS ####
### >> COMMON CIRCUITS ####
## Common circuits ####
  output$cc_circuits <- renderDataTable({
    if (is.null(dfImport())) {
      datatable(data.frame(character(0)), 
              class="display",
              colnames = '')
    } else { 
      #g <- datatable(tabNCircuits())
      g <- tabNCircuits()
      names(g)[names(g) == 'Circuit'] <- 'Normalized Circuits'
      
      if(input$simplified_common) {g <- tabSCircuits()
      names(g)[names(g) == 'Circuit'] <- 'Simplified Circuits'
      }
      g
    }
  })

  output$cc_circuits_explained <- renderText({
    if(is.null(dfImport())) return(NULL)
    
    paste0("This table lists the ",
           ifelse(input$simplified_common,
                  "simplified","normalized"),
           " circuits sorted by decreasing frequency in the log data. ",
           ifelse(input$simplified_common,
                  paste0("An empty simplified circuit means a circuit with",
                  " errors in the connections to the multimeter."),
                  "An empty normalized circuit means that no components have been added to the breadboard."))
  })
  
## Circuit in timeline ####
  output$ct_selectCircuit <- renderUI({
    if(is.null(dfImport())) return(NULL)
    
    if(is.null(tabNCircuits())) 
      return(selectInput("ct_circuit", "Select a circuit...", c("No data available")))
    else {
      if(input$simplified_common) {
        return(selectInput("ct_circuit", "Select a circuit...", 
                           if("" %in% tabSCircuits()$Circuit) {
                             c("- Empty circuit"="",
                               tabSCircuits()$Circuit)
                           } else {
                             tabSCircuits()$Circuit
                           }))
      } else {
        return(selectInput("ct_circuit", "Select a circuit...", 
                           if("" %in% tabNCircuits()$Circuit) {
                             c("- Empty circuit"="",
                               tabNCircuits()$Circuit)
                           } else {
                             tabNCircuits()$Circuit
                           }))
      }
    }})
  
  output$ct_plotu_chart <- renderPlot({
    if(is.null(dfImport())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      gra <- c("grey","red")
      
      grafdata <- dfActionCircuit() %>% mutate (Time = round(Time/60, digits=2))
      
      if(is.null(input$ct_circuit)) {
        grafdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
      } else {
        if(input$simplified_common) {
          grafdata$sel <- dfActionCircuit()$CircuitoSimplificado == input$ct_circuit
        } else {
          grafdata$sel <- dfActionCircuit()$CircuitoNormalizado == input$ct_circuit
        }
      }
      
      g <- ggplot(grafdata, aes(x = Alumno, y = Time,
                                         color = sel, shape= sel)) + 
        geom_line(aes(x=Alumno, y=TotalTime, color=NULL, shape=NULL, group=1),
                  data=dfACxStud(), color="grey") + 
        geom_point(size = 4, alpha = 0.5) +  
        labs(y="Time, in h") +
        scale_color_manual(values = gra) +
        scale_shape_manual(values= c(95,1)) + labs(x = "User")  +
        theme(panel.background = element_rect(fill=NA), 
              panel.border = element_rect(colour="black",fill=NA),
              panel.grid.major.x = element_line(linetype=0),
              panel.grid.major.y = element_line(colour="lightgray",linetype="solid"),
              legend.position="none",
              axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              axis.text.x=element_text(angle = 90, vjust = 0.5)) 
  
      if(nrow(dfACxStud())>50) g <- g + theme(axis.text.x = element_blank())
      
      g
    }
  })
  
  output$ct_plotu <- renderUI({
    if(is.null(dfImport())) {
      plotOutput("ct_plotu_chart", height=400)
    } else {
      plotOutput("ct_plotu_chart", height=400,
                 hover = hoverOpts(
                   id = "ct_plotu_hover",
                   delay = 100,
                   nullOutside = T))
    }
  })
  
  output$ct_plot_point <- renderText({
    if(is.null(dfImport())) return(NULL)
    
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time/60
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$ct_plotu_hover, 
                      xvar = "toT", yvar = "nAc",
                      threshold= 50,
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the user.") 
    else
      return (paste("User:",response[1]))
  })
  
  output$ct_plotu_explained <- renderText({
    if(is.null(dfActionCircuit())) return(NULL)
    
    paste0("This chart shows the experiments performed by each user within their timeline. ",
           "Experiments corresponding the selected ",
           ifelse(input$simplified_common,
                  "simplified","normalized"),
           " circuit are circled in red color. ",
           "By default, the empty circuit is selected. ",
           "The line chart indicates the total time of work per user. Users are ordered by
           ascending total time of work. ")
  }) 
  

  ## Circuit per User ####
  output$ntc_selectCircuit <- renderUI({
    if(is.null(dfImport())) return(NULL)
    
    if(is.null(tabSCircuits())) 
      return(selectInput("ncu_circuit", "Select a circuit...", c("No data available")))
    else {
      if(input$simplified_common) {
        return(selectInput("ncu_circuit", "Select a circuit...", 
                           if("" %in% tabSCircuits()$Circuit) {
                             c("- Empty circuit"="",
                               tabSCircuits()$Circuit)
                           } else {
                             tabSCircuits()$Circuit
                           }))
      } else {
        return(selectInput("ncu_circuit", "Select a circuit...",
                           if("" %in% tabNCircuits()$Circuit) {
                             c("- Empty circuit"="",
                               tabNCircuits()$Circuit)
                           } else {
                             tabNCircuits()$Circuit
                           }))
      }
    }})
  
  output$ntc_plotu_chart <- renderPlot({
    if(is.null(dfImport())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE WHEN LOG DATA ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      grafntcdata <- dfActionCircuit()
   
      if(is.null(input$ncu_circuit)) {
        grafntcdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
      } else {
        if(input$simplified_common) {
          grafntcdata$sel <- dfActionCircuit()$CircuitoSimplificado == input$ncu_circuit
        } else {
          grafntcdata$sel <- dfActionCircuit()$CircuitoNormalizado == input$ncu_circuit
        }
      }
      
      grafntcdata <- grafntcdata %>% group_by(Alumno) %>%
        summarise(Len=sum(sel,na.rm=TRUE),.groups="drop")
  
      grafntcdata <- merge(sumxStud_ext, grafntcdata, all=T)
      grafntcdata[is.na(grafntcdata)] <- 0
      
  
      g <- ggplot(grafntcdata,aes(x=Alumno, y=Len)) + theme_bw() +
        geom_bar(stat="identity",width = 0.7,fill="steelblue") +
        theme(axis.title = element_text(size=14),
              axis.text= element_text(size=11),
              axis.text.x=element_text(angle = 90, vjust = 0.5),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        labs(x = "User", y= "Times Tested")
      
      if(nrow(dfACxStud())>50) g <- g + theme(axis.text.x = element_blank())
      
      g
    }
  })
  
  output$ntc_plotu <- renderUI({
    plotOutput("ntc_plotu_chart", height=400)
  })
  
  
  output$ntc_plotu_explained <- renderText({
    if(is.null(dfActionCircuit())) return(NULL)
    
    paste0("This chart shows the number of experiments performed by each user that ",
           "correspond to the selected ",
           ifelse(input$simplified_common,
                  "simplified","normalized"),
           " circuit. By default, the empty circuit is selected. ",
           "Users are ordered by ascending total time of work, as in the previous chart. ")
  }) 
  
  
  
  #### USER-SPECIFIC RESULTS ####
  ### >> USER RESULTS ####
  
  output$spr_selectStudent <- renderUI({
    if(is.null(dfImport())) return(NULL)
    
    if(is.null(tabNStudents())) 
      return(selectInput("ns_student", "Select a user...", c("No data available")))
    else {
      return(selectInput("ns_student", "Select a user...", tabNStudents()$Student))
    }})
  
  

  output$numStudActions <- renderValueBox({
    if(is.null(dfImport())) return( valueBox(
      "--", 
      "Actions",color = "blue"))
    else {  
    
    df1 <-dfImport() %>%  select(Alumno)
    
    df1$sol <- "YES"
    df1$sol[df1$Alumno != input$ns_student] <- NA
    
    df1<- df1 %>% select(Alumno,sol)%>% filter(complete.cases(.)) %>%
      summarise(Act=length(sol),.groups="drop")
    
    valueBox(
        value = df1$Act, 
        "Actions",color = "blue")
   
    }
  }
  )
  
  
  output$timstud <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Total Time, in h",color = "blue"))
    else { 
      df <-dfACxStud()
      valueBox(
        value = df$TotalTime[df$Alumno == input$ns_student],
        "Total Time, in h",color = "blue")
    }
  }) 
  
  output$numcircuit <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Experiments",color = "blue"))
    else { 
      df <-dfACxStud()
      valueBox(
        value = df$NumExp[df$Alumno == input$ns_student], 
        "Experiments",color = "blue")
    }
  })  
  

  output$numnormcircuit <- renderValueBox({
    g <- "Normalized Circuits"
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      g,color = "blue"))
    else { 
      df4 <-dfACxStud()
      
      valueBox(
        value = df4$NumNCircu[df4$Alumno == input$ns_student], 
        g,color = "blue")
    }
  })

  
  output$numsimplcircuit <- renderValueBox({
    g <- "Simplified Circuits"
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      g,color = "blue"))
    else { 
      df4 <- dfACxStud()
      
      valueBox(
        value = df4$NumSCircu[df4$Alumno == input$ns_student], 
        g,color = "blue")
    }
  })
  
  output$numresist <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Resistance Measurements",color = "blue"))
    
    df <- dfACxStud()
    valueBox(
      value = df$NumMesR[df$Alumno==input$ns_student], 
        "Resistance Measurements",color = "blue")
  })    
  
  output$numcurr <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Current Measurements",color = "blue"))
    
    df <- dfACxStud()
    valueBox(
      value = df$NumMesC[df$Alumno==input$ns_student], 
      "Current Measurements",color = "blue")
  })  

  
  output$numvoltag <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Voltage Measurements",color = "blue"))
    
    df <- dfACxStud()
    valueBox(
      value = df$NumMesV[df$Alumno==input$ns_student], 
      "Voltage Measurements", color = "blue")
  })  
  
  output$numerror <- renderValueBox({
    if(is.null(dfACxStud())) return( valueBox(
      "--", 
      "Errors in Measurement",color = "blue"))
    
    df <- dfACxStud()
    valueBox(
      value = df$NumMesE[df$Alumno==input$ns_student], 
      "Errors in measurement",color = "blue")
  })  
  
  output$lcn_circuits <- renderDataTable({
    if(is.null(dfActionCircuit())) return(NULL)
    
    df5 <- dfActionCircuit()
    df5 <- df5[df5$Alumno == input$ns_student,]
    
    df5<- df5 %>%
      group_by(Alumno,CircuitoNormalizado) %>%
      summarise(TimTes=length(CircuitoNormalizado),.groups="drop") %>%
      arrange(desc(TimTes)) %>%
      select(CircuitoNormalizado,TimTes)
    
    names(df5)[names(df5) == 'CircuitoNormalizado'] <- 'Normalized Circuits'
    names(df5)[names(df5) == 'TimTes'] <- 'Times Tested'
    
    datatable(df5)
  })

  
  
## List of unique circuits ####  
  output$lcn_circuits <- renderDataTable({
    if(is.null(dfImport())) {
      datatable(data.frame(character(0)), 
                class="display",
                colnames = '')
    } else { 
      
      df5 <-dfActionCircuit()[dfActionCircuit()$Alumno == input$ns_student,]
      
      if(input$simplified_list) {
        df5 <- df5 %>% group_by(Alumno,CircuitoSimplificado) %>% 
          summarise(TimTes=length(CircuitoSimplificado),.groups="drop") %>% 
          arrange(desc(TimTes)) %>%
          select(CircuitoSimplificado,TimTes)
        
        names(df5)[names(df5) == 'CircuitoSimplificado'] <- 'Simplified Circuit'
        names(df5)[names(df5) == 'TimTes'] <- 'Times Tested'
      } else {
        df5 <- df5 %>% group_by(Alumno,CircuitoNormalizado) %>%
          summarise(TimTes=length(CircuitoNormalizado),.groups="drop") %>%
          arrange(desc(TimTes)) %>%
          select(CircuitoNormalizado,TimTes)
        
        names(df5)[names(df5) == 'CircuitoNormalizado'] <- 'Normalized Circuit'
        names(df5)[names(df5) == 'TimTes'] <- 'Times Tested'
      }
     
      datatable(df5)
    }
  })

  output$lcn_circuits_explained <- renderText({
    if(is.null(dfImport())) return(NULL)
    
    paste0("This table lists the ",
           ifelse(input$simplified_list,
                  "simplified","normalized"),
           " circuits built by the selected user, sorted by decreasing frequency in the log data. ",
           "Users are sorted alfabetically and the first one is selected by default. ",
           ifelse(input$simplified_list,
                  paste0("An empty simplified circuit means a circuit with",
                         " errors in the connections to the multimeter."),
                  "An empty normalized circuit means that no components have been added to the breadboard."))
  })
  
  
## History of circuits ####    
  output$hc_circuits <- renderDataTable({
    if(is.null(dfImport())) {
      datatable(data.frame(character(0)), 
                class="display",
                colnames = '')
    } else { 
      df6 <-dfActionCircuit()[dfActionCircuit()$Alumno == input$ns_student,] %>%
        mutate(Time=round(Time,digits = 2),
               `Sent Date`=FechaHoraEnvio,
               `Coded Circuit`= Circuito) %>% 
        select(Time,`Sent Date`,`Coded Circuit`,Measure)
      
      rownames(df6) <- NULL
      datatable(df6, rownames = T)
    }
  })

  output$hc_circuits_explained <- renderText({
    if(is.null(dfImport())) return(NULL)
    
    paste0("This table lists all the experiments performed by the selected user, sorted ",
           "chronologically. ",
           "Users are sorted alfabetically and the first one is selected by default. ",
           "The table includes the measurement done in the experiment. An error there means that ",
           "the measurement was wrongly set up.")
  })
  
#### WORK INDICATORS ####  
### >> OBSERVATION ITEMS ####
  output$proportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestones())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                        vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE ONLY IF LOG DATA AND OBSERVATION ITEMS ARE LOADED")', parse=T, 
                        size=5)+theme_void()
    } else {
      visirtrMilestonesDifficulty(dfStudentsMilestones(), evaluation=F)
    }
  })
  
  output$proportionbars_explained <- renderText({
    if(is.null(dfStudentsMilestones())) return(NULL)
    
    paste0("This bar chart shows the percentage of users that performed each observation item ",
           "anytime in their resolution process.")
  })
  
  #Heatmap
  output$heatmap <- renderPlot({
    if(is.null(dfStudentsMilestones())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE ONLY IF LOG DATA AND OBSERVATION ITEMS ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      visirtrHeatMapAchievedMilestonePerId(dfStudentsMilestones(),labels=NULL, evaluation = F)
    }
  })
  
  output$heatmap_explained <- renderText({
    if(is.null(dfStudentsMilestones())) return(NULL)
    
    paste0("The heatmap above indicates whether each user performed each observation item, ",
           "anytime in the resolution process.")
  })
  
  
### >> ASSESSMENT MILESTONES ####
  output$evproportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestonesEv())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE ONLY IF LOG DATA AND OBSERVATION ITEMS ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      visirtrMilestonesDifficulty(dfStudentsMilestonesEv()[,-ncol(dfStudentsMilestonesEv())],
                                  evaluation = !is.null(input$evMilestonesImport))
    }
  })
  
  output$evproportionbars_explained <- renderText({
    if(is.null(dfStudentsMilestones())) return(NULL)
    
    paste0("This bar chart shows the percentage of users that achieved each assessment milestone ",
           "anytime in their resolution process. ",
           ifelse(is.null(input$evMilestonesImport), 
              "As assessment milestones have not been loaded, observation items are used in their place.", ""))
  })
  
  #Heatmap
  output$evheatmap <- renderPlot({
    if(is.null(dfStudentsMilestonesEv())) {
      ggplot()+
        coord_cartesian(xlim=c(-1,1),ylim=c(-1,1))+
        annotate("text", x=-1, y=1,
                 vjust=1, hjust=0, label='bold("THIS CHART WILL BE AVAILABLE ONLY IF LOG DATA AND OBSERVATION ITEMS ARE LOADED")', parse=T, 
                 size=5)+theme_void()
    } else {
      visirtrHeatMapAchievedMilestonePerId(dfStudentsMilestonesEv()[,1:(ncol(dfStudentsMilestonesEv())-1)],labels=NULL,
                                           evaluation = !is.null(input$evMilestonesImport))
    }
  })

  output$evheatmap_explained <- renderText({
    if(is.null(dfStudentsMilestones())) return(NULL)
    
    paste0("The heatmap above indicates whether each user achieved each assessment milestone, ",
           "anytime in the resolution process. ",
           ifelse(is.null(input$evMilestonesImport), 
              "As assessment milestones have not been loaded, observation items are used in their place.", ""))
  })
  
#### DATA DOWNLOAD ####
  output$StudentsData <- downloadHandler(
    filename = "StuData.xlsx",
    content = function(file) {
        if(nrow(dfObsItems_xUser_ext)==0) { 
          dfStu <- sumxStud_ext
        } else {
          dfStu <- merge(sumxStud_ext, dfObsItems_xUser_ext, all=TRUE)
        }
      write_xlsx(dfStu, file)
      }
  )
  
  observeEvent(input$"StudentsData", {
    dfStu <- merge(sumxStud_ext,dfObsItems_xUser_ext)
    
  })
  
#### HELP ####  
  observeEvent(input$"StrucInfo", {
    showModal(modalDialog(title = "Dashboard Structure",
                          HTML("This dashboard is divided into the following folders: <br/>
                          <b> Data Input </b> <br/> Data loading and main information <br/>
                          <b> Global Results </b> <br/> Global information about the time spent and the circuits performed by users <br/>
                          <b> Circuit-based Analysis </b> <br/> Detailed information about each of the circuits performed <br/>
                          <b> User-specific Results </b> <br/> Detailed information about the actions performed by each of the users <br/>
                          <b> Work Indicators </b> <br/> Performance analyses of the group and per user <br/> 
                          <b> Data Download </b> <br/> Download of students' performance data <br/> 
                                     "),
                          footer = tagList(actionButton("closeDS", "OK"))
    ))
    observeEvent(input$closeDS, {
      removeModal()
      
    })
  })
  
  observeEvent(input$"Glossary", {
    showModal(modalDialog(title = "Glossary of Terms",
                          HTML("<b> User:</b> A person who uses VISIR, can be a student or not <br/>
                                <b> Date:</b> Day of the month or year in which the user interact with the client <br/>
                                <b> Action:</b> User interactions with the client that generate a message on the server <br/>
                                <br/>
                                <b> Observation Item: </b> A priori steps in the resolution process and/or potential errors<br/> 
                                <b> Assessment Milestone: </b> Logical combinations of observation items to grade students' performance <br/>  
                                <br/>
                                <b> Time:</b> Span of time from the user first action to the current interaction with VISIR;
                                large gaps in time between consecutive actions are discounted <br/>
                                <b> Timeline:</b> Sequence of actions distributed over time <br/>
                                <br/>
                                <b> Experiment: </b> A circuit and its instrumental settings, as performed when interacting with VISIR <br/>
                                <b> Circuit:</b> Set of electrical components as placed and connected in the breadboard <br/>
                                <b> Normalized Circuit:</b> Standardization of a circuit to achieve identical representations for 
                                circuits with the same components connected in the same manner <br/>
                                <b> Simplified Circuit:</b> Standardization of a circuit to achieve identical representations for 
                                circuits with the same components connected in the same manner in the fragment being measured <br/>
                                <b> Measure:</b> Each of the posible magnitudes that can be determined in a circuit <br/>
      
                               <br/>Additional information can be found at 
                               <a href='http://asistembe2.iqs.edu/visirtr/index.htm' target='_blank'>
                               http://asistembe2.iqs.edu/visirtr/index.htm</a>."),
                          footer = tagList(actionButton("closeG", "OK"))
    ))
    observeEvent(input$closeG, {
      removeModal()
    })
  }) 
}

