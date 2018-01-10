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
      format(nrow(dfImport()),format="d",big.mark=""), 
      "Actions", icon = icon("cubes"), color = "blue")
  })
  
  output$numSesions <- renderValueBox({
    valueBox(
      format(length(unique(dfImport()$Sesion)),format="d",big.mark=""), 
      "Sesions", icon = icon("database"), color = "blue")
  })

  dfActionCircuit<- reactive({FunctionActionCircuit(dfImport())})
  dfOrderTime <- reactive({FunctionOrderTime(dfImport())})
  dfSSA <- reactive({FunctionSSA(dfImport())})
  dfStudTime <- reactive({FunctionTimeStud(dfImport(),dfActionCircuit())})
  dfMTS <- reactive({FunctionMTS(dfImport())})
  
 ##### GLOBAL RESULTS
  
  
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
  
  ### 3 NUMBER OF CIRCUITS VS TIME ON TASK

  
  output$nActionsVStoT <- renderPlot({
   
    ggplot(dfStudTime(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) +
      labs(x = "Time on Task", y = "Number of Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTime()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTime()$TotalTime), color="#66d9dc")
    
  })
  
  
  
  output$plotuinAct <- renderUI({
    
    plotOutput("nActionsVStoT", height=300,
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
  
  output$plotly <-renderPlotly({
    
    
    plotlyfunc3(dfMTS())
    
  })
  
  ## TIME PER STUDENT DISTRIBUTION
  
  #INFOBOXES

  
  
  
  # Total time spend
  output$totaltimespend <-  renderInfoBox({
    infoBox(
      title= NULL, value =InfovalueBoxTT(dfOrderTime()) ,subtitle ="Total Time(Hours) spend", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "teal",width = 3
      
    )})
  
  #Mean time spend
  
  output$meantimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value =InfovalueBoxMeT(dfOrderTime()),subtitle = "Mean Time/Student(Hours) spent" , icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
  #Max time spend
  
  output$maxtimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMaxT(dfOrderTime()),subtitle = "Max Time(Hours) spent",icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
  #Min time spend
  
  output$mintimespend <-  renderInfoBox({
    infoBox(
      title= NULL,value=InfovalueBoxMinT(dfOrderTime()),subtitle = "Min Time(Minutes) spent", icon = icon("users"),
      color = "teal",width = 3
      
    )})
  
  
  
  #GRAFICO DISTRIBUTION TIME PER STUDENT
  
  output$timstu <- renderPlot({
    if(is.null(dfOrderTime())) return(NULL)
    plotDistribution(dfOrderTime()$TotalTime, xlabel="Time per Student (in minutes)")
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
  
  output$dygraph <-renderDygraph({
  
    
    Dygraphfunc(dfSSA())
    
  })
  

  
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