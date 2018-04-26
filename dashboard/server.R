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
  
  tabN1Circuits <- reactive({
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
      "Students", color = "blue")
  })
  output$numActions <- renderValueBox({
    valueBox(
      ifelse(is.null(dfImport()),"--",
             format(length(dfImport()$Alumno),format="d",big.mark="")), 
      "Actions", color = "blue")
  })
  output$maxdate <- renderValueBox({
      valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",max(dfImport()$Dates)),
                              style = "font-size: 90%;"), width = 4, 
        "Last logged day", color = "blue")
  })
  output$mindate <- renderValueBox({
    valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",min(dfImport()$Dates)),
                            style = "font-size: 90%;"),width = 4, 
      "First logged day", color = "blue")
  })

  
#### GLOBAL RESULTS ####
### >> TIME ####
## Total Time vs Date (Dygraph) ####
  output$dygraph <-renderDygraph({
    if(is.null(dfImport())) return(NULL)
    Dygraphfunc(dfMTS())
  })


  
  
  
  
## Distribution of time on task ####
  # Total time spend
  output$totaltimespend <-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
             InfovalueBoxTT(dfOrderTime())),style = "font-size: 90%;"),width = 4, 
      "Total Time (in h)",color = "blue")
    })
      
  
  # Mean time spend
  
  output$meantimespend <-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMeT(dfOrderTime())),style = "font-size: 90%;"),width = 4, 
      "Mean Time/Student (in h)",color = "blue")
  })
  

  # Max time spend
  output$maxtimespend <-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMaxT(dfOrderTime())),style = "font-size: 90%;"),width = 4, 
      "Max Time (in h)",color = "blue")
  })
  

  # Min time spend
  
  output$mintimespend<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMinT(dfOrderTime())),style = "font-size: 90%;"),width = 4, 
      "Min Time (in h)",color = "blue")
  })
  

  output$timstu <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    plotDistribution(dfOrderTime()$TotalTime/60, xlabel="Time(in h) per Student")
  })
  
## Time on task vs User per Date HeatMap ####
  output$timeheat <-renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(data = dftimeuserdate(), aes(x = Student, y = Dates)) + theme_bw() +
      geom_tile(aes(fill=Time)) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5),axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
      scale_fill_viridis(direction=-1)
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
    dat <- data.frame(ids=dftimeuserdate()$Time)
    dat$toT <- dftimeuserdate()$Student
    dat$nAc <- dftimeuserdate()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.factor(res$nAc))
    responset <- unique(as.factor(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see the time dedicated.") 
    else
      return (paste("Student Id:",responset[1],"\n",
                   "Date:",responsec[1],"\n",
                   "Time (in h):",response[1]))
    
  })
  
  
  
  
  
  
### >> CIRCUITS ####
## Circuits vs User heat map ####
  output$circsuserheat <-renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(data = dfcircuser(), aes(x = Student, y = Dates)) + theme_bw() +
      geom_tile(aes(fill=Circuits)) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5),axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
      scale_fill_viridis(direction=-1)
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
    dat <- data.frame(ids=dfcircuser()$Circuits)
    dat$toT <- dfcircuser()$Student
    dat$nAc <- dfcircuser()$Dates
    dat <- as.data.frame(dat)
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
                      maxpoints = 1,
                      addDist = TRUE)
    
    response <- unique(as.character(res$ids))
    responsec <- unique(as.factor(res$nAc))
    responset <- unique(as.factor(res$toT))
    if(length(response)==0)
      return ("Place your mouse over a data point to see the number of circuits.") 
    else
      return (paste("Student Id:",responset[1],"\n",
                    "Date:",responsec[1],"\n",
                    "Circuits:",response[1]))
  })
  

  
## Circuits Timeline vs user ####
  output$timelineus <-renderPlot({
    if(is.null(dfImport())) return(NULL)
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
  
  output$numuniquecirc<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxNC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Total Number of Circuits",color = "blue")
  })
  

  # Mean number of circuits by student  
  
  output$Meannumuniquecircst<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMNC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Mean Number Circuits per Student",color = "blue")
  })
  

  # Lower bound
  
  output$lowbound<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxlowbound(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Minimum circuits performed",color = "blue")
  })

  # Upper bound 
  
  output$upbound<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxupbound(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Maximum circuits performed",color = "blue")
  })
  

  output$circdist <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    plotDistribution(distnumcirc(dfActionCircuit())$Circuito, xlabel="Circuits per User")
  })

## Circuitos normalizados únicos ####
  # Total number of normalized circuits
  
  
  output$umuniqnormcirc<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxNNC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Total Number of Circuits",color = "blue")
  })
  

  #Mean number of circuits by student  
  output$Meannumuniqnormcircst<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMNNC(dfActionCircuit())),style = "font-size: 70%;"),width = 4, 
      "Mean Number Circuits per Student",color = "blue")
  })

  #Low bound 
  
  output$lowboundN<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxlowboundN(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Minimum circuits performed",color = "blue")
  })
  
  #Upper bound 
  
  output$upboundN<-  renderValueBox({
    valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxupboundN(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Maximum circuits performed",color = "blue")
  })

  
  output$circdistN <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    plotDistribution(distnumcircN(dfActionCircuit())$CircuitoNormalizado, xlabel="Normalized Circuits per User")
  })
  
## Circuits per date ####
  output$dygraph2 <-renderDygraph({
    if(is.null(dfImport())) return(NULL)
    Dygraphfunc2(dfActionCircuit())
  })
  
### >> NUMBER OF CIRCUITS VS TIME ON TASK ####
  output$nActionsVStoT <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(dfStudTime(), aes(x=TotalTime, y=NumCircu))  +  geom_point(size = I(3), alpha = I(0.4)) + 
      labs(x = "Time (in h)", y = "Number of Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTime()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTime()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTime()$TotalTime), y= mean(dfStudTime()$NumCircu), label=round(mean(dfStudTime()$NumCircu),digits = 2),hjust = -25.5))+
      geom_text(aes(x = mean(dfStudTime()$TotalTime), y= mean(dfStudTime()$NumCircu), label=round(mean(dfStudTime()$TotalTime),digits = 2),vjust = -7.5)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
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
                    "Time (in h):",responset[1]))
  })
  
### >> NUMBER OF NORMALIZED CIRCUITS VS TIME ON TASK ####
  output$nActionsVStoTnorm <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(dfStudTimeNorm(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) + 
      labs(x = "Time (in h)", y = "Number of Normalized Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTimeNorm()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTimeNorm()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$NumCircu),digits = 2),hjust = -25.5))+
      geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$TotalTime),digits = 2),vjust = -3.5)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
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
                    "Time (in h):",responset[1]))
  })

#### CIRCUIT-BASED RESULTS ####
### >> COMMON CIRCUITS ####
## Common circuits ####
  output$cc_circuits <- renderDataTable({
    if(is.null(dfImport())) return(NULL)
    datatable(tabNCircuits())
  })

## Circuit in timeline
  output$ct_selectCircuit <- renderUI({
    if(is.null(dfImport())) return(NULL)
    if(is.null(tabNCircuits())) 
      return(selectInput("ct_circuit", "Select a circuit...", c("No data available")))
    else {
      return(selectInput("ct_circuit", "Select a circuit...", tabNCircuits()$Circuit))
    }})
  
  output$ct_plotu_chart <- renderPlot({
    if(is.null(dfImport())) return(NULL)
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
    if(is.null(dfImport())) return(NULL)
    if(is.null(tabN1Circuits())) 
      return(selectInput("ct_circuit", "Select a circuit...", c("No data available")))
    else {
      return(selectInput("ct_circuit", "Select a circuit...", tabN1Circuits()$Circuit))
    }})
  
  output$ntc_plotu_chart <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    
        grafntcdata <- dfActionCircuit() %>% select(Alumno)
    
    if(is.null(input$ct_circuit)) {
      grafntcdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
    } else {
      
      grafntcdata$sol <- as.numeric((ifelse(dfActionCircuit()$CircuitoNormalizado == input$ct_circuit,"1",
                                      "0")))
    
      grafntcdata <- grafntcdata %>% select(Alumno,sol)
      
      
      grafntcdata <- grafntcdata %>% group_by(Alumno) %>% summarise(Len=sum(sol))
      

      
    }
        
    g <- ggplot(grafntcdata,aes(x=Alumno, y=Len)) + theme_bw() +
      geom_bar(stat="identity",width = 0.7,fill="steelblue") +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5),axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + labs(x = "Student", y= "Number of Circuits")
    g
  })
  
  output$ntc_plotu <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("ntc_plotu_chart", height=400
    )
  })
  
 
  
  
  
  #### Student-specific Results ####
  ### >> User results ####
  
  output$spr_selectStudent <- renderUI({
    if(is.null(dfImport())) return(NULL)
    if(is.null(tabNStudents())) 
      return(selectInput("ns_student", "Select a student...", c("No data available")))
    else {
      return(selectInput("ns_student", "Select a student...", tabNStudents()$Student))
    }})
  
  

  output$numStudActions <- renderValueBox({
    df1 <-dfImport() %>%  select(Alumno)
    df1$stu <-dfImport()$Alumno == input$ns_student
    
    
    df1$sol <- factor(ifelse(dfImport()$Alumno == input$ns_student,"YES",
                             NA))
    
    df1<- df1 %>% select(Alumno,sol)%>% filter(complete.cases(.))
    
    
    df1 <- df1 %>% summarise(Act=length(sol))
    
    
    
    valueBox(
      
      ifelse(is.null(df1),"--",
             format(df1$Act,format="d",big.mark="")), 
      "Actions",color = "blue")
    
  })
  
  

  output$timstud <- renderValueBox({
    
    df2 <-dfStudTime() %>%  select(Alumno,TotalTime)
    df2$st <-dfStudTime()$Alumno == input$ns_student
    
    
    df2$co <- factor(ifelse(dfStudTime()$Alumno == input$ns_student,"YES",
                             NA))
    
    df2<- df2 %>% select(Alumno,TotalTime,co)%>% filter(complete.cases(.))
    

  valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",df2$TotalTime),
                          style = "font-size: 90%;"),width = 4, 
           "Total Time (in h)", color = "blue")
}) 

  
  
  
  output$numcircuit <- renderValueBox({
    
    df3 <-dfStudTime() %>%  select(Alumno,NumCircu)
    df3$st <-dfStudTime()$Alumno == input$ns_student
    
    
    df3$co <- factor(ifelse(dfStudTime()$Alumno == input$ns_student,"YES",
                            NA))
    
    df3<- df3 %>% select(Alumno,NumCircu,co)%>% filter(complete.cases(.))
    
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(df3$NumCircu,format="d",big.mark="")), 
      "Circuits", color = "blue")
    
  })  
  
  output$numnormcircuit <- renderValueBox({
    
    df4 <-dfStudTimeNorm() %>%  select(Alumno,NumCircu)
    df4$st <-dfStudTimeNorm()$Alumno == input$ns_student
    
    
    df4$co <- factor(ifelse(dfStudTimeNorm()$Alumno == input$ns_student,"YES",
                            NA))
    
    df4<- df4 %>% select(Alumno,NumCircu,co)%>% filter(complete.cases(.))
    
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(df4$NumCircu,format="d",big.mark="")), 
      "Normalized Circuits",color = "blue")
    
  })
  
  output$numresist <- renderValueBox({
    
    df7 <-dfActionCircuit() %>%  select(Alumno,Measure)
    df7$st <-dfActionCircuit()$Alumno == input$ns_student
    
    df7$sest <- as.numeric((ifelse(dfActionCircuit()$Measure =="Resistance","1",
                                   "0")))
    
    df7$res <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                             NA))
      
   
 
    df7<- df7 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.))


    df7 <- df7 %>% group_by(Alumno,res) %>% summarise(resis=sum(sest))
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(df7$resis,format="d",big.mark="")), 
      "Resistance Measurement",color = "blue")
    
  })  
  
  output$numcurr <- renderValueBox({
    
    df8 <-dfActionCircuit() %>%  select(Alumno,Measure)
    df8$st <-dfActionCircuit()$Alumno == input$ns_student
    
    df8$sest <- as.numeric((ifelse(dfActionCircuit()$Measure =="Current","1",
                                   "0")))
    
    df8$res <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                             NA))
    
    
    
    df8<- df8 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.))
    
    
    df8 <- df8 %>% group_by(Alumno,res) %>% summarise(curr=sum(sest))
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(df8$curr,format="d",big.mark="")), 
      "Current Measurement",color = "blue")
    
  })  
  
  output$numvoltag <- renderValueBox({
    
    df9 <-dfActionCircuit() %>%  select(Alumno,Measure)
    df9$st <-dfActionCircuit()$Alumno == input$ns_student
    
    df9$sest <- as.numeric((ifelse(dfActionCircuit()$Measure =="Voltage","1",
                                   "0")))
    
    df9$res <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                             NA))
    
    
    
    df9<- df9 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.))
    
    
    df9 <- df9 %>% group_by(Alumno,res) %>% summarise(voltag=sum(sest))
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(df9$voltag,format="d",big.mark="")), 
      "Voltage Measurement",color = "blue")
    
  })  
  
  output$numerror <- renderValueBox({
    
    dfx <-dfActionCircuit() %>%  select(Alumno,Measure)
    dfx$st <-dfActionCircuit()$Alumno == input$ns_student
    
    dfx$sest <- as.numeric((ifelse(dfActionCircuit()$Measure =="Error","1",
                                   "0")))
    
    dfx$res <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                             NA))
    
    
    
    dfx<- dfx %>% select(Alumno,res,sest) %>%  filter(complete.cases(.))
    
    
    dfx <- dfx %>% group_by(Alumno,res) %>% summarise(error=sum(sest))
    
    valueBox(
      
      ifelse(is.null(dfImport()),"--",
             format(dfx$error,format="d",big.mark="")), 
      "Measures not evaluated",color = "blue")
    
  })  
  
  
  
  
  output$lcn_circuits <- renderDataTable({
    if(is.null(dfImport())) return(NULL)
    
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
    if(is.null(dfImport())) return(NULL)
    
    df6 <-dfActionCircuit() %>%  select(Alumno,Time,FechaHoraEnvio,Circuito)
    
    df6$hc <-dfActionCircuit()$Alumno == input$ns_student
    
    df6$hco <- factor(ifelse(dfActionCircuit()$Alumno == input$ns_student,"YES",
                            NA))
   
    df6<- df6 %>% filter(complete.cases(.)) %>% select(Time,FechaHoraEnvio,Circuito) %>% mutate(Time=round(Time,digits = 2))
   
 
    datatable(df6)
  })
  
  tasks <-  reactive({
    b <- input$bonus
    list(
      code = list(id = "Dashboard Information", value = 15 + b, color = "aqua", text = "Information about the Dashboard",
                  description = paste("Before start using the Dashboard read this information: this
                                      dashboard has been created to evaluate and obtain a global visualization
                                      of the traces obtained by the 
                                      users in the laboratory remote VISIR")),
                                      
      
      layout = list(id = "layout", value = 40 + b, color = "green", text = "Design new layout",
                    description = paste(rep("Design new layout", 30), collapse = "!! ")),
      
      docs = list(id = "docs", value = 37 + b, color = "red", text = "Write documentation",
                  description = paste(rep("Write documentation", 30), collapse = "!! "))
    )
  })
  
  observeEvent(req(input$Help), {
    # match input$todo to the right task from the reactive tasks() (by id)
    # (I'm sure there's a simpler way of doing this)
    task <- lapply(tasks(), function(el) {
      if (el$id == input$Help) el
      else NULL
    }) %>% shiny:::dropNulls() %>% `[[`(1)
    
    # show a custom-made modal for the task (input$todo) that was just clicked
    showModal(modalDialog(title = task$text,
                          task$description,
                          easyClose = TRUE, footer = NULL
    ))
  })

  
  
})
