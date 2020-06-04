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
  dfStudTimeSimpl <- reactive({FunctionTimeStudSimpl(dfImport(),dfActionCircuit())})
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
  
  tabSCircuits <- reactive({
    tab <- table(na.omit(dfActionCircuit()$CircuitoSimplificado))
    tab <- tab[order(-tab)]
    data.frame(Circuit = names(tab), TimesTested = as.integer(tab),stringsAsFactors = FALSE) 
  })

#### DATA INPUT ####
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
      valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",max(dfImport()$Dates)),
                              style = "font-size: 90%;"), width = 4, 
        "Last logged date", color = "blue")
  })
  output$mindate <- renderValueBox({
    valueBox(value = tags$p(ifelse(is.null(dfImport()),"--",min(dfImport()$Dates)),
                            style = "font-size: 90%;"),width = 4, 
      "First logged date", color = "blue")
  })

  
  
  #Yes Milestones
  dfMilestonesDef<-reactive({
    inFile <- input$obsMilestonesImport
    if (is.null(inFile)) return(NULL) else
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
  })
  
  dfActionsMilestones <- reactive({
    inFile <- input$obsMilestonesImport
    if (is.null(inFile)) return(NULL) else
      generatedfActionsMilestones(dfActions(), dfMilestonesDef())
  })
  
  dfStudentsMilestones <- reactive({
    inFile <- input$obsMilestonesImport
    if (is.null(inFile)) return(NULL) else
      if(input$checkbox) {
        generatedfUsersMilestones(dfActionsMilestones(), dfMilestonesDef())
      } else {
        generatedfFilesMilestones(dfActionsMilestones(), dfMilestonesDef())
      }
  })
  
  dfMilestonesEvDef<-reactive({
    inFile <- input$evMilestonesImport
    if (is.null(inFile)) return(NULL) else
      return(read.table(inFile$datapath, header=TRUE, sep="\t", quote=""))
  })
  
  dfStudentsMilestonesEv <- reactive({
    generatedStudentsMilestonesEv(dfStudentsMilestones(),dfMilestonesEvDef())
  })
  
  dfOIColors <- reactive({
    getdfOIColors(dfMilestonesDef(),dfMilestonesEvDef())
  })
  
  dfObsItemsLong <- reactive({
    dfObsLong <- dfActionsMilestones()
    if(is.null(dfObsLong)) return(NULL)
    
    if(input$checkbox) 
      dfObsLong <- rename(dfObsLong,student=user) %>% select(-filename)
    else
      dfObsLong <- rename(dfObsLong,student=filename) %>% select(-user)
    
    dfObsLong <- dfObsLong %>% 
      select(-application,-action,-session,-type,-param_name,-xml,
             -param_value,-diff_time,-xml_cum) %>% 
      gather("milestone","check",-(1:5)) %>% filter(check==TRUE) %>% 
      select(-check)
    dfObsLong <- dfObsLong %>% arrange(student,number)
    
    dfObsLong$numMil <- 1
    for(i in 2:nrow(dfObsLong)) {
      dfObsLong$numMil[i] <- 
        ifelse(dfObsLong$student[i]==dfObsLong$student[i-1],
               dfObsLong$numMil[i-1]+1,1)
    }
    dfObsLong
  })
  
  
## Data input milestones ##
  output$milestonesData <- renderText({
    if(is.null(dfMilestonesDef())) {
      "Observation milestones definition not yet loaded!"
    } else {
      paste("Read observation milestones: ",nrow(dfMilestonesDef()),
            " milestones (", paste(dfMilestonesDef()[,1],collapse = ", ") ,")", sep="")
    }
  })
  output$evmilestonesData <- renderText({
    if(is.null(dfMilestonesEvDef())) {
      "Evaluation milestones not loaded. Observation milestones will be used for evaluation."
    } else {
      paste("Read evaluation milestones: ",nrow(dfMilestonesEvDef()),
            " milestones (", paste(dfMilestonesEvDef()[,1],collapse = ", ") ,")", sep="")
    }
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
      "Mean Time/User (in h)",color = "blue")
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
    plotDistribution(dfOrderTime()$TotalTime/60, xlabel="Time (in h) per User")
  })
  
## Time on task vs User per Date HeatMap ####
  output$timeheat <-renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(data = dftimeuserdate(), aes(x = User, y = Dates)) + theme_bw() +
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
    dat$toT <- dftimeuserdate()$User
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
      return (paste("User Id:",responset[1],"\n",
                   "Date:",responsec[1],"\n",
                   "Time (in h):",response[1]))
    
  })
  
  
  
  
  
  
### >> CIRCUITS ####
## Circuits vs User heat map ####
  output$circsuserheat <-renderPlot({
    if(is.null(dfImport())) return(NULL)
    ggplot(data = dfcircuser(), aes(x = User, y = Dates)) + theme_bw() +
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
    dat$toT <- dfcircuser()$User
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
      return (paste("User Id:",responset[1],"\n",
                    "Date:",responsec[1],"\n",
                    "Circuits:",response[1]))
  })
  

## Circuits Timeline vs user ####
  output$timelineus <-renderPlot({
    if(is.null(dfImport())) return(NULL)
    gra=c("red","green","blue","yellow")
    g <- ggplot(dfActionCircuit(), aes(x = Alumno, y = Time,
                                  color = Measure, shape= Measure)) + 
      geom_point(size = 4, alpha = 0.5) +  
      scale_color_manual(values = gra) +
      scale_shape_manual(values= c(95,95,95,1)) + labs(x = "User")  +
      theme(panel.background = element_rect(fill=NA), 
            panel.border = element_rect(colour="black",fill=NA),
            panel.grid.major.x = element_line(linetype=0),
            panel.grid.major.y = element_line(colour="lightgray",linetype="solid"),
            legend.key=element_blank(),
            axis.text.x=element_text(angle = 90, vjust = 0.5))+
      guides(color = guide_legend(override.aes = list(size=7)))  
             
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
      return ("In the non-facetted chart, place your mouse over a data point to identify the user.") 
    else
      return (paste("User:",response[1]))
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
      "Mean Number Circuits per User",color = "blue")
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
    g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxNNC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Total Number of Circuits",color = "blue")
    
    if(input$simplified_distribution) g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxSC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Total Number of Circuits",color = "blue")
    g

  })
  

  #Mean number of circuits by student  
  output$Meannumuniqnormcircst<-  renderValueBox({
    g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMNNC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Mean Number Circuits per User",color = "blue")
    
    if(input$simplified_distribution) g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxMSC(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Mean Number Circuits per User",color = "blue")
    g
  })

  #Low bound 
  
  output$lowboundN<-  renderValueBox({
    g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxlowboundN(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Minimum circuits performed",color = "blue")
    
    if(input$simplified_distribution) g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxlowboundS(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Minimum circuits performed",color = "blue")
    g
  })
  
  #Upper bound 
  
  output$upboundN<-  renderValueBox({
    g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxupboundN(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Maximum circuits performed",color = "blue")
    
    if(input$simplified_distribution) g <- valueBox(
      value=tags$p(ifelse(is.null(dfImport()),"--",
                          InfovalueBoxupboundS(dfActionCircuit())),style = "font-size: 90%;"),width = 4, 
      "Maximum circuits performed",color = "blue")
    g
  })

  
  output$circdistN <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    g <- plotDistribution(distnumcircN(dfActionCircuit())$CircuitoNormalizado, xlabel="Normalized Circuits per User")
    
    if(input$simplified_distribution) g <- plotDistribution(distnumcircS(dfActionCircuit())$CircuitoSimplificado, 
                                                            xlabel="Simplified Normalized Circuits per User")
    g
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
      geom_vline(xintercept = mean(dfStudTime()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTime()$TotalTime), y= mean(dfStudTime()$NumCircu), label=round(mean(dfStudTime()$NumCircu),digits = 2),hjust = -15.5))+
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
      return ("Place your mouse over a data point to identify the user.\n\n") 
    else
      return (paste("User Id:",response[1],"\n",
                    "Number of circuts:",responsec[1],"\n",
                    "Time (in h):",responset[1]))
  })
  
### >> NUMBER OF NORMALIZED CIRCUITS VS TIME ON TASK ####
  output$nActionsVStoTnorm <- renderPlot({
    if(is.null(dfImport())) return(NULL)
    g <- ggplot(dfStudTimeNorm(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) + 
      labs(x = "Time (in h)", y = "Number of Normalized Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTimeNorm()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTimeNorm()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$NumCircu),digits = 2),hjust = -15.5))+
      geom_text(aes(x = mean(dfStudTimeNorm()$TotalTime), y= mean(dfStudTimeNorm()$NumCircu), label=round(mean(dfStudTimeNorm()$TotalTime),digits = 2),vjust = -3.5)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  
    if(input$simplified_time) g <- ggplot(dfStudTimeSimpl(), aes(x=TotalTime, y=NumCircu)) +  geom_point(size = I(3), alpha = I(0.4)) + 
      labs(x = "Time (in h)", y = "Number of Simplified Normalized Circuits") + theme_bw()+geom_hline(yintercept = mean(dfStudTimeNorm()$NumCircu), color="#fbada7")+
      geom_vline(xintercept = mean(dfStudTimeSimpl()$TotalTime), color="#66d9dc") + geom_text(aes(x = mean(dfStudTimeSimpl()$TotalTime), y= mean(dfStudTimeSimpl()$NumCircu), label=round(mean(dfStudTimeSimpl()$NumCircu),digits = 2),hjust = -15.5))+
      geom_text(aes(x = mean(dfStudTimeSimpl()$TotalTime), y= mean(dfStudTimeSimpl()$NumCircu), label=round(mean(dfStudTimeSimpl()$TotalTime),digits = 2),vjust = -3.5)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
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
    dat <- data.frame(ids=dfStudTimeNorm()$Alumno)
    dat$toT <- dfStudTimeNorm()$TotalTime
    dat$nAc <- dfStudTimeNorm()$NumCircu
    dat <- as.data.frame(dat)
    g <- "Number of Normalized Circuits:"
    
    if(input$simplified_time) {dat <- data.frame(ids=dfStudTimeSimpl()$Alumno)
    dat$toT <- dfStudTimeSimpl()$TotalTime
    dat$nAc <- dfStudTimeSimpl()$NumCircu
    dat <- as.data.frame(dat)
    g <- "Number of Simplified Normalized Circuits:"
    }
    
    res <- nearPoints(dat, input$plot_hovernAct, 
                      xvar = "toT", yvar = "nAc",
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
    g <- datatable(tabNCircuits())
    
    if(input$simplified_common) g <- datatable(tabSCircuits())
    g
  })

## Circuit in timeline ####
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
      scale_shape_manual(values= c(95,1)) + labs(x = "User")  +
      theme(panel.background = element_rect(fill=NA), 
            panel.border = element_rect(colour="black",fill=NA),
            panel.grid.major.x = element_line(linetype=0),
            panel.grid.major.y = element_line(colour="lightgray",linetype="solid"),
            legend.position="none",
            axis.text.x=element_text(angle = 90, vjust = 0.5)) 
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
      return ("Place your mouse over a data point to identify the user.") 
    else
      return (paste("User:",response[1]))
  })
  
  ## Number of Circuits vs User ####
  output$ntc_selectCircuit <- renderUI({
    if(is.null(dfImport())) return(NULL)
    if(is.null(tabN1Circuits())) 
      return(selectInput("ncu_circuit", "Select a circuit...", c("No data available")))
    else {
      return(selectInput("ncu_circuit", "Select a circuit...", tabN1Circuits()$Circuit))
    }})
  
  output$ntc_plotu_chart <- renderPlot({
    if(is.null(dfImport())) return(NULL)
        grafntcdata <- dfActionCircuit() %>% select(Alumno)
 
    if(is.null(input$ct_circuit)) {
      grafntcdata$sel <- rep(FALSE,nrow(dfActionCircuit()))
    } else {
      grafntcdata$sel <- dfActionCircuit()$CircuitoNormalizado == input$ncu_circuit
    
      grafntcdata <- grafntcdata %>% select(Alumno,sel)
      grafntcdata <- grafntcdata %>% group_by(Alumno) %>% summarise(Len=sum(sel,na.rm=TRUE))
    }
        
    g <- ggplot(grafntcdata,aes(x=Alumno, y=Len)) + theme_bw() +
      geom_bar(stat="identity",width = 0.7,fill="steelblue") +
      theme(axis.text.x=element_text(angle = 90, vjust = 0.5),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      labs(x = "User", y= "Number of Circuits")
    g
  })
  
  output$ntc_plotu <- renderUI({
    if(is.null(dfImport())) return(NULL)
    plotOutput("ntc_plotu_chart", height=400
    )
  })
  
 
  
  
  
  #### STUDENT-SPECIFIC RESULTS ####
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
      summarise(Act=length(sol))
    
    valueBox(
        value = df1$Act, 
        "Actions",color = "blue")
   
    }
  }
  )
  
  
  output$timstud <- renderValueBox({
    if(is.null(dfImport())) return( valueBox(
      "--", 
      "Total Time (in h)",color = "blue"))
    else { 
      df2 <-dfStudTime() %>%  select(Alumno,TotalTime)
      
      df2$co <- "YES"
      df2$co[df2$Alumno != input$ns_student] <- NA
      
      df2 <- df2 %>% select(Alumno,TotalTime,co) %>% filter(complete.cases(.))
      
      valueBox(
        value = df2$TotalTime, 
        "Total Time (in h)",color = "blue")
    }
  }) 
  
  output$numcircuit <- renderValueBox({
    if(is.null(dfImport())) return( valueBox(
      
      "--", 
      "Circuits",color = "blue"))
    else { 
      df3 <-dfStudTime() %>%  select(Alumno,NumCircu)
      
      df3$co <- "YES"
      df3$co[df3$Alumno != input$ns_student] <- NA
      
      df3<- df3 %>% select(Alumno,NumCircu,co)%>% filter(complete.cases(.))
      
      valueBox(
        value = df3$NumCircu, 
        "Circuits",color = "blue")
    }
  })  
  

  output$numnormcircuit <- renderValueBox({
    g <- "Normalized Circuits"
    if(is.null(dfImport())) return( valueBox(
      "--", 
      g,color = "blue"))
    else { 
      df4 <-dfStudTimeNorm() %>%  select(Alumno,NumCircu)
      if(input$simplified_summary) {df4 <- dfStudTimeSimpl() %>%  select(Alumno,NumCircu)
      g <- "Simplified Normalized Circuits"
      }
      df4$co <- "YES"  
      df4$co[df4$Alumno != input$ns_student] <- NA
      
      df4<- df4 %>% select(Alumno,NumCircu,co) %>% filter(complete.cases(.))
      
      valueBox(
        value = df4$NumCircu, 
        g,color = "blue")
    }
  })

  
  
  output$numresist <- renderValueBox({
    if(is.null(dfImport())) return( valueBox(
      
      "--", 
      "Resistance Measurement",color = "blue"))
    else { 
      df7 <-dfActionCircuit() %>%  select(Alumno,Measure)
  
      df7$sest <- as.numeric((ifelse(df7$Measure =="Resistance","1",
                                     "0")))
      
      df7$res <- "YES"
      df7$res[df7$Alumno != input$ns_student] <-NA
      
      df7<- df7 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.)) %>%
        group_by(Alumno,res) %>% summarise(resis=sum(sest))
      
      valueBox(
        value = df7$resis, 
        "Resistance Measurement",color = "blue")
    }
    
  })    
  
  output$numcurr <- renderValueBox({
    
    if(is.null(dfImport())) return( valueBox(
      
      "--", 
      "Current Measurement",color = "blue"))
    else { 
      df8 <-dfActionCircuit() %>%  select(Alumno,Measure)
      df8$sest <- as.numeric((ifelse(df8$Measure =="Current","1",
                                     "0")))
      df8$res <- "YES"
      df8$res[df8$Alumno != input$ns_student] <- NA
      
      df8<- df8 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.)) %>%
        group_by(Alumno,res) %>% summarise(curr=sum(sest))
      
      valueBox(
        value = df8$curr, 
        "Current Measurement",color = "blue")
      }
    
  })  

  
  output$numvoltag <- renderValueBox({
    
    if(is.null(dfImport())) return( valueBox(
      
      "--", 
      "Voltage Measurement",color = "blue"))
    else { 
      
    
    df9 <-dfActionCircuit() %>%  select(Alumno,Measure)
    
    df9$sest <- as.numeric((ifelse(df9$Measure =="Voltage","1",
                                   "0")))
    
    df9$res <- "YES"
    df9$res[df9$Alumno != input$ns_student] <- NA
    
    df9<- df9 %>% select(Alumno,res,sest) %>%  filter(complete.cases(.))
    df9 <- df9 %>% group_by(Alumno,res) %>% summarise(voltag=sum(sest))
    
    valueBox(
      value = df9$voltag, 
      "Voltage Measurement",color = "blue")
    }
    
  })  
  
  output$numerror <- renderValueBox({
    if(is.null(dfImport())) return( valueBox(
      "--", 
      "Measures not evaluated",color = "blue"))
    else { 
    
    dfx <-dfActionCircuit() %>%  select(Alumno,Measure)
    
    dfx$sest <- as.numeric((ifelse(dfx$Measure =="Error","1",
                                   "0")))
    dfx$res <- "YES"
    dfx$res[dfx$Alumno != input$ns_student] <- NA
    
    dfx<- dfx %>% select(Alumno,res,sest) %>%
      filter(complete.cases(.)) %>%
      group_by(Alumno,res) %>% summarise(error=sum(sest))
    
    valueBox(
      value = dfx$error, 
      "Error",color = "blue")
    }
  })  
  
  output$lcn_circuits <- renderDataTable({
    if(is.null(dfImport())) return(NULL)
    
    df5 <-dfActionCircuit() %>%  select(Alumno,CircuitoNormalizado)
    df5$co <- factor(ifelse(df5$Alumno == input$ns_student,"YES",
                            NA))
    
    df5<- df5 %>% select(Alumno,CircuitoNormalizado,co) %>%
      filter(complete.cases(.))%>%
      group_by(Alumno,CircuitoNormalizado) %>%
      summarise(TimTes=length(CircuitoNormalizado)) %>%
      arrange(desc(TimTes)) %>%
      ungroup(Alumno,CircuitoNormalizado) %>%
      select(CircuitoNormalizado,TimTes)
    
    names(df5)[names(df5) == 'CircuitoNormalizado'] <- 'Normalized Circuits'
    names(df5)[names(df5) == 'TimTes'] <- 'Times Tested'
    
    datatable(df5)
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
  

  
  observeEvent(input$"StrucInfo", {
    # match input$todo to the right task from the reactive tasks() (by id)
    # (I'm sure there's a simpler way of doing this)
 
    # show a custom-made modal for the task (input$todo) that was just clicked
    showModal(modalDialog(title = "Dashboard Structure",
                          HTML("This dashboard is divided in 4 folders: <br/> <b> Data Input </b> <br/> Data loading and
                                      main information <br/> <b> Global Results </b> <br/>
                                     Global information about the time spent and the circuits performed by users <br/>
                                    <b> Circuit-based Analysis </b> <br/> Detailed information about each of the circuits
                                    performed <br/> <b> User-specific Results </b> <br/> Detailed information about the actions performed
                                    by each of the users
                                     "),
                           footer = tagList(actionButton("closeDS", "OK"))
    ))
    observeEvent(input$closeDS, {
      removeModal()
      
    })
  })
  
  observeEvent(input$"Glossary", {
    # match input$todo to the right task from the reactive tasks() (by id)
    # (I'm sure there's a simpler way of doing this)
    
    # show a custom-made modal for the task (input$todo) that was just clicked
    showModal(modalDialog(title = "Glossary of Terms",
                          HTML(" <b> User:</b> A person who uses VISIR, can be a student or not <br/>
                                         <b> Action:</b> User interactions with the client that generate a message on the server <br/>
                               <b> Circuit:</b> Set of electrical components as placed and connected in the breadboard <br/>
                               <b> Normalized Circuit:</b> Standardization of the circuits to achieve a representation independent of the position in the breadboard <br/>
                               <b> Date:</b> Day of the month or year in which the user interact with the client <br/>
                               <b> Time:</b> The indefinite continued progress of the user in each of his interactions with VISIR, time events with large gaps of time have been neglected  <br/>
                               <b> Timeline:</b> Sequence of actions distributed over time <br/>
                               <b> Measurement:</b> Each of the posible actions performed in a circuit <br/>
                               
                               
                               "),
                           footer = tagList(actionButton("closeG", "OK"))
    ))
    observeEvent(input$closeG, {
      removeModal()
      
    })
  }) 
  

  
  
})

