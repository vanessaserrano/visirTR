options(shiny.maxRequestSize=500*1024^2)
shinyServer(function(input, output) {

  #Data selection
  dfImport <- NULL

  
  #Data import
  dfImport<-reactive({
    inFile <- input$logsImport
    if (is.null(inFile)) return(NULL) else
      df <- read.table(inFile$datapath, header=F, sep=",", quote="\"",
                       stringsAsFactors = FALSE)
      colnames(df) <- c("Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
            "DatosEnviadosXML","DatosRecibidosXML","Tarea")
      df$Dates <- format(as.Date(df$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")
     
      
      return(df)
  })
  
  # 1 TAB ValueBox
  output$numStudents <- renderValueBox({
    valueBox(
      format(length(unique(dfImport()$Alumno)),format="d",big.mark=""), 
      "Students", icon = icon("group"), color = "blue")
  })
  output$numActions <- renderValueBox({
    valueBox(
      format(length(dfImport()$Alumno),format="d",big.mark=""), 
      "Actions", icon = icon("cubes"), color = "blue")
  })
  
  output$maxdate <- renderValueBox({

    valueBox(
      value = tags$p(max(dfImport()$Dates),style = "font-size: 90%;"), width = 4, 
      "Final Date", icon = icon("calendar-o"), color = "blue")
  })

  output$mindate <- renderValueBox({
    
    valueBox(
      value = tags$p(min(dfImport()$Dates),style = "font-size: 90%;"),width = 4, 
      "Start Date", icon = icon("calendar-o"), color = "blue")
  })
    
  

  dfActionCircuit<- reactive({FunctionActionCircuit(dfImport())})
  dfOrderTime <- reactive({FunctionOrderTime(dfImport())})
  dfSSA <- reactive({FunctionSSA(dfImport())})
  dfStudTime <- reactive({FunctionTimeStud(dfImport(),dfActionCircuit())})
  dfMTS <- reactive({FunctionMTS(dfImport())})
  dfcircuser <- reactive({fciruserdate(dfActionCircuit())})
  dftimeuserdate <- reactive({fplotlyfunc3(dfMTS())})
  dfStudTimeNorm <- reactive({FunctionTimeStudNorm(dfImport(),dfActionCircuit())})
  
 ##### GLOBAL RESULTS
  
  
  #### TIME ANALYSIS
  
  ### Total Time vs Date (Dygraph)
  
  output$dygraph <-renderDygraph({
    
    
    Dygraphfunc(dfMTS())
    
  })
  
  
  ## Time on taskl vs User per Date HeatMap
  
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
  
  
  
  
  #### CIRCUITS ANALYSIS 
  
  ## Circuits vs User heat map 2
  
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
  
  
  
  ###Circuits Time line vs user
  
  
  output$timelineus <-renderPlot({
    
    gra=c("red","black","blue","green")
    ggplot(dfActionCircuit(), aes(x = Alumno, y = Time,color=Mesure)) + 
      geom_point(size = 3, alpha = 1,shape=95) +
      theme_few()+
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+scale_fill_manual(values=gra)
    
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
      return ("Place your mouse over a data point to see the number of circuits.") 
    else
      return (paste("Student:",response[1]))
  }) 
  
  
  
  
  #### 1 SECCION CIRCUIT DISTRIBUTION BOXES
  
    ### 1.1 STANDARD CIRCUITS
  
      ## TAB PANEL CIRCUITOS NORMALES UNICOS
  
  
        # Total number of circuits
  output$numuniquecirc <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxNC(dfActionCircuit()) ,subtitle =  "Total Number of Circuits", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
      
    )})
      
      #Mean number of circuits by student  
  
  output$Meannumuniquecircst <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxMNC(dfActionCircuit()) ,subtitle = "Mean Number Circuits per Student", icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
    #LOWBOUNDBOUND 
    
  output$lowbound <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxlowbound(dfActionCircuit()),subtitle = "Minimum circuits performed",icon = icon("arrow-down"),
      color = "teal",width = 3
      
    )})
  
   #UPBOUNDBOUND 
  
  output$upbound <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxupbound(dfActionCircuit()) ,subtitle = "Maximum circuits performed", icon = icon("arrow-up"),
      color = "teal",width = 3
      
    )})
  

  
  
  # GRAFICO 1
  
  output$circdist <- renderPlot({
    if(is.null(dfActionCircuit())) return(NULL)
    
    
    
    plotDistribution(distnumcirc(dfActionCircuit())$Circuito, xlabel="Standard Circuits per Student Distribution")
  })
  
  
      ## TAB PANEL CIRCUITOS NORMALIZADOS UNICOS
  
  
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
  
  #LOWBOUNDBOUND 
  
  output$lowboundN <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxlowboundN(dfActionCircuit()),subtitle ="Minimum circuits performed", icon = icon("arrow-down"),
      color = "teal",width = 3
      
    )})
  
  #UPBOUNDBOUND 
  
  output$upboundN <-  renderInfoBox({
    infoBox(
      title= NULL,value = InfovalueBoxupboundN(dfActionCircuit()),subtitle = "Maximum circuits performed", icon = icon("arrow-up"),
      color = "teal",width = 3
      
    )})
  
  
  
  # GRAFICO 2
  
  output$circdistN <- renderPlot({
    if(is.null(dfActionCircuit())) return(NULL)
    
    
    
    plotDistribution(distnumcircN(dfActionCircuit())$CircuitoNormalizado, xlabel="Normalized Circuits per Student Distribution")
  })
  
  ### 2 NUMBER OF CIRCUITS ALONG TIME
  
  
    ## DRYGRAPH 2
  
  output$dygraph2 <-renderDygraph({
    
    
    Dygraphfunc2(dfActionCircuit())
    
  })
  
  ### 3 NUMBER OF CIRCUITS VS TIME ON TASK zz

  
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
  
  
  
  
  ### 3.1  NUMBER OF  NORMALIZED CIRCUITS VS TIME ON TASK zz
  
  
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
  
  
  
  
  
  ### 4 TYPES OF MESURE
  
  
  output$typmes <- renderChart2({
    
    functymes(dfActionCircuit())
   
  })
  
  
  
  #Values type of mesure 
  
  output$numvolts <- renderInfoBox({
    infoBox(
      "Mesure Voltage",ValueBoxnumvolts(dfActionCircuit()) , icon = icon("cogs"),
      color = "teal"
      
    ) 
  })
  
  output$numresis <- renderInfoBox({
    infoBox(
      "Mesure Resistance",ValueBoxnumresis(dfActionCircuit()) , icon = icon("cogs"),
      color = "purple"
      
    ) 
  })
  
  output$numna <- renderInfoBox({
    infoBox(
      "N.A.",ValueBoxnumna(dfActionCircuit()) , icon = icon("cogs"),
      color = "red"
      
    ) 
  })
  
  output$numint <- renderInfoBox({
    infoBox(
      "Mesure Intensity",ValueBoxnumint(dfActionCircuit()) , icon = icon("cogs"),
      color = "yellow"
      
    ) 
  })
  
  
  
  ### TIEMPO ON TASK ####
  
  ## DYGRAPH 3 Tiempo medio dedicado por alumno y por fecha
  

  
  ## TIME PER STUDENT DISTRIBUTION
  
  #INFOBOXES

  
  
  
  # Total time spend
  output$totaltimespend <-  renderInfoBox({
    infoBox(
      title= NULL, value =InfovalueBoxTT(dfOrderTime()) ,subtitle ="Total Time (Hours)", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
      
    )})
  
  #Mean time spend
  
  output$meantimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value =InfovalueBoxMeT(dfOrderTime()),subtitle = "Mean Time/Student (Hours)" , icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
  #Max time spend
  
  output$maxtimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMaxT(dfOrderTime()),subtitle = "Max Time (Hours)",icon = icon("arrow-up"),
      color = "teal",width = 3
      
    )})
  
  #Min time spend
  
  output$mintimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMinT(dfOrderTime()),subtitle = "Min Time (Hours)", icon = icon("arrow-down"),
      color = "teal",width = 3
      
    )})
  
  
  
  #GRAFICO DISTRIBUTION TIME PER STUDENT
  
  output$timstu <- renderPlot({
    if(is.null(dfOrderTime())) return(NULL)
    plotDistribution(dfOrderTime()$TotalTime, xlabel="Time(Hours) per Student")
  })
  
  
  
  ##### PARTE ANTIGUA ####
  
  
  
     # 2.1 First Row
  

  
  output$numuniquecloseirc <-  renderInfoBox({
    infoBox(
      "Total Number of Closed Circuits",InfovalueBoxNCC(dfActionCircuit()) , icon = icon("cog", lib = "glyphicon"),
      color = "green"
      
    )})
  
  output$numuniqnormcirc <-  renderInfoBox({
    infoBox(
      "Total Number of Normalized Circuits",InfovalueBoxNNC(dfActionCircuit()) , icon = icon("code-fork"),
      color = "yellow"
      
    )})
    
  # 2.2 Second Row
  
  output$Meannumuniquecirc <-  renderInfoBox({
    infoBox(
      "Mean Number of Circuits per Student",InfovalueBoxMNC(dfActionCircuit()) , icon = icon("users"),
      color = "teal"
      
    )})
  
  
  output$Meannumuniquecloseircst <-  renderInfoBox({
    infoBox(
      "Mean Number of Closed Circuits per Student",InfovalueBoxMNCC(dfActionCircuit()) , icon = icon("users"),
      color = "green"
      
    )})
  
  output$Meannumuniqnormcircstxxx <-  renderInfoBox({
    infoBox(
      "Mean Number of Normalized Circuits per Student",InfovalueBoxMNNC(dfActionCircuit()) , icon = icon("users"),
      color = "yellow"
      
    )})
  
  
  
  
  
  #Data output.
  output$studentData <- renderText({
    if(is.null(dfImport())) {
      "Please select some logs files to start the analysiss!"
    } else {
      paste("Loaded log data:",length(unique(dfImport()$Alumno)),
            "Students,",nrow(dfImport()),"actions.")
    }
  })

  output$nActions <-  renderPlot({
    
    Numcirc(dfActionCircuit())
    
  })
  
  
    #GRAPHS
  
  # DRYGRAPH 1
  


  
  #TABLA 20 circuitos normalizados mas frecuentes
  
  output$mostNormCircuits <- DT::renderDataTable(DT::datatable({ 
    #wss_temp <- updateWcvValue()
   
    Acircu <- dfActionCircuit() %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
    
    MCNCircuits <- Acircu %>%  group_by(CircuitoNormalizado) %>% summarise(MCCirc=length(CircuitoNormalizado)) %>%
      arrange(desc(MCCirc)) %>% slice(1:20)
    
    MCNCircuits
    
  }, 
  rownames = FALSE,
  #caption = 'These values are the Within-Cluster variance (WCV) corresponding to the number of Clusters (K)',
  class = 'cell-border stripe',
  extensions = c('Buttons','ColReorder','Scroller'),
  options = list(
    searching = F,
    #dom = 'RC<"clear">lfrtip',
    dom = 't',
    colVis = list(activate = 'mouseover'),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}"), pageLength = 20))
  )
  
  
  
  
  
  #Global Results
  #Time on task distribution

  # 1? SECCI?N
  output$toT_dist <- renderPlot({
    if(is.null(dfOrderTime())) return(NULL)
    plotDistribution(dfOrderTime()$TiempoAcumuladoCorregidoMax, xlabel="Time on Task (in minutes)")
  })
  
  output$toT_text1 <- renderPrint({
    if(is.null(dfOrderTime())) return(NULL)
    str1 <- paste("Distribution of the time on task spent by student. 
                  A histogram and an estimate of the density curve are presented.")
    str2 <- paste("The mean of this distribution is", 
                  round(mean(dfOrderTime()$TiempoAcumuladoCorregidoMax), 2),
                  "minutes.")
    lowbound <- floor(mean(dfOrderTime()$TiempoAcumuladoCorregidoMax) - sd(dfOrderTime()$TiempoAcumuladoCorregidoMax))
    upbound <- ceiling(mean(dfOrderTime()$TiempoAcumuladoCorregidoMax) + sd(dfOrderTime()$TiempoAcumuladoCorregidoMax))
    prop <- mean(dfOrderTime()$TiempoAcumuladoCorregidoMax >= lowbound &
                   dfOrderTime()$TiempoAcumuladoCorregidoMax <= upbound) * 100
    str3 <- paste(round(prop,0), "% of students have worked between", lowbound, 
                  "and", upbound, "minutes.")
    bimod <- diptest::dip.test(dfOrderTime()$TiempoAcumuladoCorregidoMax)
    str4 <- if(bimod$p.value < 0.05) {
      paste("There is clearly more than one mode for the time on task.")
    } else if(bimod$p.value < 0.10) {
      paste("There is some sign of presence of more than one mode for time on task.")
    } else(paste("There is no sign of more than one mode for time on task."))
    HTML(paste(str1, str2, str3, str4, sep = '<br />'))
  })

  # 2? SECCI?N
  

  

  
  
  
  
  
  
  
  
  
  # 3 SECCION
  

  


})