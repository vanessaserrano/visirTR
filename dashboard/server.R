options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output, session) {

  session$onSessionEnded(function() {
    stopApp()
  })
  
#### DEFINITIONS & CALCULATIONS ####
  dfImport<-reactive({
    inFile <- input$logsImport
    if (is.null(inFile)) return(NULL)
    
    df <- read.table(inFile$datapath, header=F, sep=",", quote="\"",
                     stringsAsFactors = FALSE)
    colnames(df) <- c("Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
        "DatosEnviadosXML","DatosRecibidosXML","Tarea")
    df$Dates <- format(as.Date(df$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")
   
    return(df)
  })

  dfActionCircuit<- reactive({FunctionActionCircuit(dfImport())})
  dfOrderTime <- reactive({FunctionOrderTime(dfImport())})
  dfSSA <- reactive({FunctionSSA(dfImport())})
  dfStudTime <- reactive({FunctionTimeStud(dfImport(),dfActionCircuit())})
  dfMTS <- reactive({FunctionMTS(dfImport())})
  dfcircuser <- reactive({fciruserdate(dfActionCircuit())})
  dftimeuserdate <- reactive({fplotlyfunc3(dfMTS())})
  dfStudTimeNorm <- reactive({FunctionTimeStudNorm(dfImport(),dfActionCircuit())})

#### DATA INPUT ####
  output$numStudents <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--",
             format(length(unique(dfImport()$Alumno)),format="d",big.mark="")), 
      "Students", icon = icon("group"), color = "blue")
  })
  output$numActions <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--",
             format(length(dfImport()$Alumno),format="d",big.mark="")), 
      "Actions", icon = icon("cubes"), color = "blue")
  })
  output$maxdate <- renderValueBox({
      valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",max(dfImport()$Dates)),
                              style = "font-size: 90%;"), width = 4, 
        "Final Date", icon = icon("calendar-o"), color = "blue")
  })
  output$mindate <- renderValueBox({
    valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",min(dfImport()$Dates)),
                            style = "font-size: 90%;"),width = 4, 
      "Start Date", icon = icon("calendar-o"), color = "blue")
  })

  
#### GLOBAL RESULTS ####
### >> TIME ####
## Total Time vs Date (Dygraph) ####
  output$dygraph <-renderDygraph({
    Dygraphfunc(dfMTS())
  })

## Distribution of time on task ####
  # Total time spend
  output$totaltimespend <-  renderInfoBox({
    infoBox(
      title= NULL, value =InfovalueBoxTT(dfOrderTime()) ,subtitle ="Total Time (Hours)", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
    )})
  
  # Mean time spend
  output$meantimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value =InfovalueBoxMeT(dfOrderTime()),subtitle = "Mean Time/Student (Hours)" , icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
  # Max time spend
  output$maxtimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMaxT(dfOrderTime()),subtitle = "Max Time (Hours)",icon = icon("arrow-up"),
      color = "teal",width = 3
      
    )})
  
  # Min time spend
  output$mintimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMinT(dfOrderTime()),subtitle = "Min Time (Hours)", icon = icon("arrow-down"),
      color = "teal",width = 3
      
    )})
  
  output$timstu <- renderPlot({
    if(is.null(dfOrderTime())) return(NULL)
    plotDistribution(dfOrderTime()$TotalTime, xlabel="Time(Hours) per Student")
  })
  
## Time on task vs User per Date HeatMap ####
  output$timeheat <-renderPlot({
    ggplot(data = dftimeuserdate(), aes(x = Student, y = Dates)) +
      geom_tile(aes(fill=Time)) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))+
      scale_fill_viridis(direction=-1)
  })
  
  output$plot <- renderUI({
    plotOutput("timeheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_poin <- renderText({
    dat <- data.frame(ids=dftimeuserdate()$Time)
    dat$toT <- dftimeuserdate()$Student
    dat$nAc <- dftimeuserdate()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see the time dedicated.") 
    else
      return (paste("Time (Minutes):",response[1]))
  })
  
### >> CIRCUITS ####
## Circuits vs User heat map ####
  output$circsuserheat <-renderPlot({
    ggplot(data = dfcircuser(), aes(x = Student, y = Dates)) +
      geom_tile(aes(fill=Circuits)) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))+
      scale_fill_viridis(direction=-1)
  })
  
  output$plotui <- renderUI({
    plotOutput("circsuserheat", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_points <- renderText({
    dat <- data.frame(ids=dfcircuser()$Circuits)
    dat$toT <- dfcircuser()$Student
    dat$nAc <- dfcircuser()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see the number of circuits.") 
    else
      return (paste("Circuits:",response[1]))
  })
  
## Circuits Timeline vs user ####
  output$timelineus <-renderPlot({
    gra=c("red","green","blue","yellow")
    ggplot(dfActionCircuit(), aes(x = Alumno, y = Time,
                                  color = Mesure,shape= Mesure)) + 
      geom_point(size = 4, alpha = 0.5) +
      scale_color_manual(values = gra) +
      scale_shape_manual(values= c(95,95,95,1)) + 
      theme_few() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.text = element_text(size=8),
            legend.key.size=unit(5,"points")) +
      guides(color = guide_legend(override.aes = list(size=8)))
  })
  
  output$plotu <- renderUI({
    plotOutput("timelineus", height=400,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_point <- renderText({
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student:",response[1]))
  }) 
  
## Circuits distribution ####
  # Total number of circuits
  output$numuniquecirc <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxNC(dfActionCircuit()) ,subtitle =  "Total Number of Circuits", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
      
    )})
      
  # Mean number of circuits by student  
  output$Meannumuniquecircst <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxMNC(dfActionCircuit()) ,subtitle = "Mean Number Circuits per Student", icon = icon("users"),
      color = "teal",width = 3
    )})
  
  # Lower bound
  output$lowbound <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxlowbound(dfActionCircuit()),subtitle = "Minimum circuits performed",icon = icon("arrow-down"),
      color = "teal",width = 3
    )})
  
  # Upper bound 
  output$upbound <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxupbound(dfActionCircuit()) ,subtitle = "Maximum circuits performed", icon = icon("arrow-up"),
      color = "teal",width = 3
    )})
  
  output$circdist <- renderPlot({
    if(is.null(dfActionCircuit())) return(NULL)
    plotDistribution(distnumcirc(dfActionCircuit())$Circuito, xlabel="Standard Circuits per Student Distribution")
  })

## Circuitos normalizados únicos ####
  # Total number of normalized circuits
  output$umuniqnormcirc <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxNNC(dfActionCircuit()) ,subtitle = "Total Number of Circuits", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
    )})
  
  #Mean number of circuits by student  
  output$Meannumuniqnormcircst <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxMNNC(dfActionCircuit()) ,subtitle = "Mean Number Circuits per Student", icon = icon("users"),
      color = "teal",width = 3
    )})
  
  #Low bound 
  output$lowboundN <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxlowboundN(dfActionCircuit()),subtitle ="Minimum circuits performed", icon = icon("arrow-down"),
      color = "teal",width = 3
    )})
  
  #Upper bound 
  output$upboundN <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxupboundN(dfActionCircuit()),subtitle = "Maximum circuits performed", icon = icon("arrow-up"),
      color = "teal",width = 3
    )})
  
  output$circdistN <- renderPlot({
    if(is.null(dfActionCircuit())) return(NULL)
    plotDistribution(distnumcircN(dfActionCircuit())$CircuitoNormalizado, xlabel="Normalized Circuits per Student Distribution")
  })
  
## Circuits per date ####
  output$dygraph2 <-renderDygraph({
    Dygraphfunc2(dfActionCircuit())
  })
  
### >> NUMBER OF CIRCUITS VS TIME ON TASK ####
  output$nActionsVStoT <- renderPlot({
    ggplot(dfStudTime(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) +
      labs(x = "Time on Task", y = "Number of Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTime()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTime()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTime()$TotalTime), y= mean(dfStudTime()$NumCircu), label=round(mean(dfStudTime()$NumCircu),digits = 2),hjust = -1.5))+
      geom_text(aes(x = mean(dfStudTime()$TotalTime), y= mean(dfStudTime()$NumCircu), label=round(mean(dfStudTime()$TotalTime),digits = 2),hjust = -3.5,angle=90))
  })

  output$plotuinAct <- renderUI({
    plotOutput("nActionsVStoT", height=350,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_pointsnAct <- renderText({
    dat <- data.frame(ids=dfStudTime()$Alumno)
    dat$toT <- dfStudTime()$TotalTime
    dat$nAc <- dfStudTime()$NumCircu
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student Id:",response[1],"Number of circuts:",responsec[1],"Time (Minutes):",responset[1]))
  })
  
### >> NUMBER OF NORMALIZED CIRCUITS VS TIME ON TASK ####
  output$nActionsVStoTnorm <- renderPlot({
    ggplot(dfStudTimeNorm(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) +
      labs(x = "Time on Task", y = "Number of Normalized Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTimeNorm()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTimeNorm()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$NumCircu),digits = 2),hjust = -1.5))+
      geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$TotalTime),digits = 2),hjust = -3.5,angle=90))
  })

  output$plotuinActnorm <- renderUI({
    plotOutput("nActionsVStoTnorm", height=350,
               hover = hoverOpts(
                 id = "plot_hovernAct",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$plot_pointsnActnorm <- renderText({
    dat <- data.frame(ids=dfStudTimeNorm()$Alumno)
    dat$toT <- dfStudTimeNorm()$TotalTime
    dat$nAc <- dfStudTimeNorm()$NumCircu
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.numeric(res$nAc))
    responset <- unique(as.numeric(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to identify the student.") 
    else
      return (paste("Student Id:",response[1],"Number of Normalized Circuits:",responsec[1],"Time (Minutes):",responset[1]))
  })
})
