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

  dfActionCircuit<- reactive({funActionCircuit(dfImport())})
  dfOrderTime <- reactive({funOrderTime(dfActionCircuit())})
  dfSSA <- reactive({FunctionSSA(dfImport())})
  dfStudTime <- reactive({FunctionTimeStud(dfImport(),dfActionCircuit())})
  dfMTS <- reactive({FunctionMTS(dfImport())})
  dfcircuser <- reactive({fciruserdate(dfActionCircuit())})
  dftimeuserdate <- reactive({fplotlyfunc3(dfMTS())})
  dfStudTimeNorm <- reactive({FunctionTimeStudNorm(dfImport(),dfActionCircuit())})
  tabNCircuits <- reactive({
    tab <- table(na.omit(dfActionCircuit()$CircuitoNormalizado))
    tab <- tab[order(-tab)]
    data.frame(Circuit = names(tab), TimesTested = as.integer(tab),stringsAsFactors = FALSE) 
  })
  
  tabNStudents <- reactive({
    tab <- table(na.omit(dfActionCircuit()$Alumno))
    tab <- tab[order(tab)]
    data.frame(Student = names(tab), TimesTested = as.integer(tab), stringsAsFactors = FALSE) 
  }) 
  

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
    plotDistribution(dfOrderTime()$TotalTime/60, xlabel="Time(Hours) per Student")
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
    g <- ggplot(dfActionCircuit(), aes(x = Alumno, y = Time,
                                  color = Measure,shape= Measure)) + 
      geom_point(size = 4, alpha = 0.5) +
      scale_color_manual(values = gra) +
      scale_shape_manual(values= c(95,95,95,1)) + 
      theme_few() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.text = element_text(size=8),
            legend.key.size=unit(5,"points")) +
      guides(color = guide_legend(override.aes = list(size=8)))
    if(input$timeline_facet) g <- g + facet_wrap(~Measure,nrow=2)
    g
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
      return ("In the non-facetted chart, place your mouse over a data point to identify the student.") 
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
    plotDistribution(distnumcirc(dfActionCircuit())$Circuito, xlabel="Circuits per User")
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
    plotDistribution(distnumcircN(dfActionCircuit())$CircuitoNormalizado, xlabel="Normalized Circuits per User")
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
      return ("Place your mouse over a data point to identify the student.\n\n") 
    else
      return (paste("Student Id:",response[1],"\n",
                    "Number of circuts:",responsec[1],"\n",
                    "Time (Minutes):",responset[1]))
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
      return ("Place your mouse over a data point to identify the student.\n\n") 
    else
      return (paste("Student Id:",response[1],"\n",
                    "Number of Normalized Circuits:",responsec[1],"\n",
                    "Time (Minutes):",responset[1]))
  })

#### CIRCUIT-BASED RESULTS ####
### >> COMMON CIRCUITS ####
## Common circuits ####
  output$cc_circuits <- renderDataTable({
    datatable(tabNCircuits())
  })

## Circuit in timeline
  output$ct_selectCircuit <- renderUI({
    if(is.null(tabNCircuits())) 
      return(selectInput("ct_circuit", "Select a circuit..", c("No data available")))
    else {
      return(selectInput("ct_circuit", "Select a circuit..", tabNCircuits()$Circuit))
    }})
  
  output$ct_plotu_chart <- renderPlot({
    gra <- c("grey","red")
    grafdata <- dfActionCircuit() %>% select(Alumno,Time)
    
    if(is.null(input$ct_circuit)) {
      grafdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
    } else {
      grafdata$sel <- dfActionCircuit()$CircuitoNormalizado == input$ct_circuit
    }
    g <- ggplot(grafdata, aes(x = Alumno, y = Time,
                                       color = sel,shape= sel)) + 
      geom_point(size = 4, alpha = 0.5) +
      scale_color_manual(values = gra) +
      scale_shape_manual(values= c(95,1)) + 
      theme_few() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.position="none")
    g
  })
  
  output$ct_plotu <- renderUI({
    plotOutput("ct_plotu_chart", height=400,
               hover = hoverOpts(
                 id = "ct_plotu_hover",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$ct_plot_point <- renderText({
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$ct_plotu_hover, 
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
  
  ## Number of Circuits vs User
  
  
  
  output$ntc_selectCircuit <- renderUI({
    if(is.null(tabNCircuits())) 
      return(selectInput("ct_circuit", "Select a circuit..", c("No data available")))
    else {
      return(selectInput("ct_circuit", "Select a circuit..", tabNCircuits()$Circuit))
    }})
  
  output$ntc_plotu_chart <- renderPlot({
    
        grafntcdata <- dfActionCircuit() %>% select(Alumno)
    
    if(is.null(input$ct_circuit)) {
      grafntcdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
    } else {
      
      grafntcdata$sol <- factor(ifelse(dfActionCircuit()$CircuitoNormalizado == input$ct_circuit,"YES",
                                      NA))
    
      grafntcdata <- grafntcdata %>% select(Alumno,sol)%>% filter(complete.cases(.))
      
      
      grafntcdata <- grafntcdata %>% group_by(Alumno,sol) %>% summarise(Len=length(sol))
      
    }
        
    g <- ggplot(grafntcdata,aes(x=Alumno, y=Len)) + 
      geom_bar(stat="identity") 
    g
  })
  
  output$ntc_plotu <- renderUI({
    plotOutput("ntc_plotu_chart", height=400,
               hover = hoverOpts(
                 id = "ntc_plotu_hover",
                 delay = 100,
                 nullOutside = T)
    )
  })
  
  output$ntc_plot_point <- renderText({
    dat <- data.frame(ids=dfActionCircuit()$Alumno)
    dat$toT <- dfActionCircuit()$Alumno
    dat$nAc <- dfActionCircuit()$Time
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$ntc_plotu_hover, 
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
  
  #### Student-specific Results ####
  ### >> User results ####
  
  output$spr_selectStudent <- renderUI({
    if(is.null(tabNCircuits())) 
      return(selectInput("ns_student", "Select a student..", c("No data available")))
    else {
      return(selectInput("ns_student", "Select a student..", tabNStudents()$Student))
    }})
  
  
  output$numStudActions <- renderValueBox({
     df1 <-dfActionCircuit() %>%  select(Alumno)
     df1$stu <-dfActionCircuit()$Alumno == input$ns_student
     
     
     df1$sol <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                                      NA))
     
     df1<- df1 %>% select(Alumno,sol)%>% filter(complete.cases(.))
     
     
     df1 <- df1 %>% summarise(Act=length(sol))
     
     
     
    valueBox(
      
      ifelse(is.null(df1),"--",
             format(df1$Act,format="d",big.mark="")), 
      "Actions", icon = icon("cubes"), color = "red")

  })
  
  output$timstud <- renderValueBox({
    
    df2 <-dfStudTime() %>%  select(Alumno,TotalTime)
    df2$st <-dfStudTime()$Alumno == input$ns_student
    
    
    df2$co <- factor(ifelse(dfStudTime()$Alumno == input$ns_student,"YES",
                             NA))
    
    df2<- df2 %>% select(Alumno,TotalTime,co)%>% filter(complete.cases(.))
    
    
    valueBox(
      
      ifelse(is.null(df2),"--",
             format(df2$TotalTime,format="d",big.mark="")), 
      "Total Time (Minutes)", icon = icon("hourglass-half"), color = "green")
    
  })
  
  output$numcircuit <- renderValueBox({
    
    df3 <-dfStudTime() %>%  select(Alumno,NumCircu)
    df3$st <-dfStudTime()$Alumno == input$ns_student
    
    
    df3$co <- factor(ifelse(dfStudTime()$Alumno == input$ns_student,"YES",
                            NA))
    
    df3<- df3 %>% select(Alumno,NumCircu,co)%>% filter(complete.cases(.))
    
    
    valueBox(
      
      ifelse(is.null(df3),"--",
             format(df3$NumCircu,format="d",big.mark="")), 
      "Circuits", icon = icon("wrench"), color = "orange")
    
  })  
  
  output$numnormcircuit <- renderValueBox({
    
    df4 <-dfStudTimeNorm() %>%  select(Alumno,NumCircu)
    df4$st <-dfStudTimeNorm()$Alumno == input$ns_student
    
    
    df4$co <- factor(ifelse(dfStudTimeNorm()$Alumno == input$ns_student,"YES",
                            NA))
    
    df4<- df4 %>% select(Alumno,NumCircu,co)%>% filter(complete.cases(.))
    
    
    valueBox(
      
      ifelse(is.null(df4),"--",
             format(df4$NumCircu,format="d",big.mark="")), 
      "Normalized Circuits", icon = icon("wrench"), color = "teal")
    
  })
  
  output$lcn_circuits <- renderDataTable({
    
    df5 <-dfActionCircuit() %>%  select(Alumno,CircuitoNormalizado)
    
    df5$scl <-dfActionCircuit()$Alumno == input$ns_student
    
    df5$co <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                            NA))
    
    df5<- df5 %>% select(Alumno,CircuitoNormalizado,co)%>% filter(complete.cases(.))
    
    df5 <- df5 %>% group_by(Alumno,CircuitoNormalizado) %>% summarise(TimTes=length(CircuitoNormalizado)) 
    
    df5 <- df5 %>%arrange(desc(TimTes)) %>%
      ungroup(Alumno,CircuitoNormalizado) %>%select(CircuitoNormalizado,TimTes)
    
    names(df5)[names(df5) == 'CircuitoNormalizado'] <- 'Normalized Circuits'
    names(df5)[names(df5) == 'TimTes'] <- 'Times Tested'
   
    datatable(df5)
  })
  
  output$hc_circuits <- renderDataTable({
    
    df6 <-dfActionCircuit() %>%  select(Alumno,Sesion,FechaHoraEnvio,Circuito)
    
    df6$hc <-dfActionCircuit()$Alumno == input$ns_student
    
    df6$hco <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                            NA))
   
    df6<- df6 %>% filter(complete.cases(.)) %>% select(Sesion,FechaHoraEnvio,Circuito)
   
 
    datatable(df6)
  })
  
  
  
  
  
})
