shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  
#### DEFINITIONS & CALCULATIONS ####
## Action & Student data ####
  dfImport<-reactive({import_dfActions(input$logsImport)})
  dfActionTime <- reactive({create_dfActionTime(dfImport(), timeLimit = 900)})
  dfActionCircuit <- reactive({create_dfActionCircuit(dfActionTime())})
  dfACxStud <- reactive({aggreg_dfActionCircuit_xStud(dfActionTime(),dfActionCircuit())})
  dfACxStudDate <- reactive({aggreg_dfActionCircuit_xStudDate(dfActionTime(),dfActionCircuit())})
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
             format(length(unique(dfImport()$Alumno)),format="d",big.mark="")), 
      "Users", color = "blue")
  })
  output$numActions <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--",
             format(length(dfImport()$Alumno),format="d",big.mark="")), 
      "Actions", color = "blue")
  })
  output$maxdate <- renderValueBox({
    valueBox(value = ifelse(is.null(dfImport()),"--",max(dfImport()$Dates)), width = 4, 
             "Last logged date", color = "blue")
  })
  output$mindate <- renderValueBox({
    valueBox(value = ifelse(is.null(dfImport()),"--",min(dfImport()$Dates)),width = 4, 
             "First logged date", color = "blue")
  })
  
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
    if(is.null(dfACxStud())) return(NULL)
    plotDistribution(dfACxStud()$TotalTime, xlabel="Time per User, in h")
  })

    
## Total Time vs Date (Dygraph) ####
  output$dygraph <-renderDygraph({
    if(is.null(dfACxDate())) return(NULL)
    if(nrow(dfACxDate())==1) return(NULL)
    
    createPlot_TimexDate(dfACxDate())
  })


## Time on task vs User per Date -- HeatMap ####
  output$timeheat <-renderPlot({
    if(is.null(dfACxStudDate())) return(NULL)

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
  })
  
  output$plot <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("timeheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
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
      return (paste("User Id:",responset[1],"\n",
                   "Date:",responsec[1],"\n",
                   "Time, in h:",response[1]))
    
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
    if(is.null(dfImport())) return(NULL)
    plotDistribution(dfACxStud()$NumCircu, xlabel="Experiments per User")
  })

## Circuits per date ####
  output$dygraph2 <-renderDygraph({
    if(is.null(dfACxDate())) return(NULL)
    if(nrow(dfACxDate())==1) return(NULL)
    
    createPlot_ExpxDate(dfACxDate())
  })
  
## Experiments per User and Date -- Heat map ####
  output$circsuserheat <-renderPlot({
    if(is.null(dfACxStudDate())) return(NULL)
    
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
  })
  
  output$plotui <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("circsuserheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
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
      return (paste("User Id:",responset[1],"\n",
                    "Date:",responsec[1],"\n",
                    "Experiments:",response[1]))
  })
  

## Experimental Timelines ####
  output$timelineus <-renderPlot({
    if(is.null(dfActionCircuit())) return(NULL)
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
  })
  
  output$plotu <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("timelineus", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_point <- renderText({
    if(is.null(dfImport())) return(NULL)
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time
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
    if(is.null(dfImport())) return(NULL)
    g <- plotDistribution(dfACxStud()$NumNCircu, xlabel="Normalized Circuits per User")
    
    if(input$simplified_distribution) {
      g <- plotDistribution(dfACxStud()$NumSCircu, xlabel="Simplified Circuits per User")
    }
    g
  })
  

### >> NUMBER OF CIRCUITS VS TIME ON TASK ####
## Number of experiments vs time ####
    output$nActionsVStoT <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    
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

  })

  output$plotuinAct <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("nActionsVStoT", height=350,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
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
      return (paste("User Id:",response[1],"\n",
                    "Number of Experiments:",responsec[1],"\n",
                    "Time, in h:",responset[1]))
  })
  
## Number of unique circuits vs time ####
  output$nActionsVStoTnorm <- renderPlot({
    if(is.null(dfACxStud())) return(NULL)
    
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
  })
  

  output$plotuinActnorm <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("nActionsVStoTnorm", height=350,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
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
      return (paste("User Id:",response[1],"\n",
                    g,responsec[1],"\n",
                    "Time (in h):",responset[1]))
  })

#### CIRCUIT-BASED RESULTS ####
### >> COMMON CIRCUITS ####
## Common circuits ####
  output$cc_circuits <- renderDataTable({
    if(is.null(dfImport())) return(NULL)
    
    #g <- datatable(tabNCircuits())
    g <- tabNCircuits()
    names(g)[names(g) == 'Circuit'] <- 'Normalized Circuits'
    
    if(input$simplified_common) {g <- tabSCircuits()
    names(g)[names(g) == 'Circuit'] <- 'Simplified Circuits'
    }
    g
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
    if(is.null(dfImport())) return(NULL)
    
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
  })
  
  output$ct_plotu <- renderUI({
    if(is.null(dfImport())) return(NULL)
    
    plotOutput("ct_plotu_chart", height=400,
               hover = hoverOpts(
                 id = "ct_plotu_hover",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$ct_plot_point <- renderText({
    if(is.null(dfImport())) return(NULL)
    
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time
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
    if(is.null(dfImport())) return(NULL)
    
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
  })
  
  output$ntc_plotu <- renderUI({
    if(is.null(dfImport())) return(NULL)
    
    plotOutput("ntc_plotu_chart", height=400)
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
    if(is.null(dfImport())) return(NULL)
    
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
  })

## History of circuits ####    
  output$hc_circuits <- renderDataTable({
    if(is.null(dfImport())) return(NULL)
    
    df6 <-dfActionCircuit()[dfActionCircuit()$Alumno == input$ns_student,] %>%
      mutate(Time=round(Time,digits = 2),
             `Sent Date`=FechaHoraEnvio,
             `Coded Circuit`= Circuito) %>% 
      select(Time,`Sent Date`,`Coded Circuit`,Measure)
   
    datatable(df6)
  })

#### WORK INDICATORS ####  
### >> OBSERVATION ITEMS ####
  output$proportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestones())) return(NULL)
    visirtrMilestonesDifficulty(dfStudentsMilestones())
  })
  
  #Heatmap
  output$heatmap <- renderPlot({
    if(is.null(dfStudentsMilestones())) return(NULL)
    visirtrHeatMapAchievedMilestonePerId(dfStudentsMilestones(),labels=NULL)
  })
  
### >> ASSESSMENT MILESTONES ####
  output$evproportionbars <- renderPlot ({
    if(is.null(dfStudentsMilestonesEv())) return(NULL)
    visirtrMilestonesDifficulty(
      dfStudentsMilestonesEv()[,-ncol(dfStudentsMilestonesEv())])
  })
  
  #Heatmap
  output$evheatmap <- renderPlot({
    if(is.null(dfStudentsMilestonesEv())) return(NULL)
    visirtrHeatMapAchievedMilestonePerId(dfStudentsMilestonesEv()[,1:(ncol(dfStudentsMilestonesEv())-1)],labels=NULL)
  })
  

#### HELP ####  
  observeEvent(input$"StrucInfo", {
    showModal(modalDialog(title = "Dashboard Structure",
                          HTML("This dashboard is divided in 4 folders: <br/>
                          <b> Data Input </b> <br/> Data loading and main information <br/>
                          <b> Global Results </b> <br/> Global information about the time spent and the circuits performed by users <br/>
                          <b> Circuit-based Analysis </b> <br/> Detailed information about each of the circuits performed <br/>
                          <b> User-specific Results </b> <br/> Detailed information about the actions performed by each of the users <br/>
                          <b> Work Indicators </b> <br/> Performance analyses of the group and per user <br/> 
                                     "),
                          footer = tagList(actionButton("closeDS", "OK"))
    ))
    observeEvent(input$closeDS, {
      removeModal()
      
    })
  })
  
  observeEvent(input$"Glossary", {
    showModal(modalDialog(title = "Glossary of Terms",
                          HTML("<b> Action:</b> User interactions with the client that generate a message on the server <br/>
                                <b> Circuit:</b> Set of electrical components as placed and connected in the breadboard <br/>
                                <b> Date:</b> Day of the month or year in which the user interact with the client <br/>
                                <b> Assessment Milestone: </b> logical combinations of observation items to grade students' performance <br/>  
                                <b> Experiment: </b> A circuit and its instrumental settings <br/>
                                <b> Measure:</b> Each of the posible magnitudes that can be determined in a circuit <br/>
                                <b> Observation Item: </b> A priori steps in the resolution process and/or potential errors<br/> 
                                <b> Normalized Circuit:</b> Standardization of the circuits to achieve a representation independent of the position in the breadboard <br/>
                                <b> Time:</b> The indefinite continued progress of the user in each of his interactions with VISIR, time events with large gaps of time have been neglected  <br/>
                                <b> Timeline:</b> Sequence of actions distributed over time <br/>
                                <b> User:</b> A person who uses VISIR, can be a student or not <br/>
                               
                               
                               
                               "),
                          footer = tagList(actionButton("closeG", "OK"))
    ))
    observeEvent(input$closeG, {
      removeModal()
    })
  }) 
})

