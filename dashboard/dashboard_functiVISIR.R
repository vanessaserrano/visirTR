FunctionActionCircuit <- function (dfVISIR_acciones) {
 
  
  observaciones <- TRUE
  
  replaceMany <- function(x, find, replace) {
    for(i in 1:length(find)) {
      x <- gsub(find[i], replace[i], x, fixed=T)
    }
    x
  }
  
  # From package "Deducer"
  perm <- function (vec, duplicates = FALSE) 
  {
    if (!is.vector(vec)) 
      stop("vec must be a vector")
    n <- length(vec)
    if (n == 1) 
      return(as.matrix(vec[1]))
    else if (n < 2) 
      return(NULL)
    z <- matrix(1)
    if (all(!duplicated(vec))) 
      duplicates = TRUE
    for (i in 2:n) {
      x <- cbind(z, i)
      a <- c(1:i, 1:(i - 1))
      z <- matrix(0, ncol = ncol(x), nrow = i * nrow(x))
      z[1:nrow(x), ] <- x
      for (j in 2:i - 1) {
        z[j * nrow(x) + 1:nrow(x), ] <- x[, a[1:i + j]]
      }
    }
    dimnames(z) <- NULL
    z <- apply(z, c(1, 2), function(x) vec[x])
    if (!duplicates) 
      z[!duplicated(z), ]
    else z
  }
 #ANTIGUO
  
numAcciones<-nrow(dfVISIR_acciones)


  
dfVISIR_accionesOrdenado <- dfVISIR_acciones[
  order(dfVISIR_acciones$Alumno,
        dfVISIR_acciones$FechaHoraEnvio),]



timeLimit <- 900
vecDiffTime<-rep(NA,numAcciones)
vecDiffTimeCum<-rep(0, numAcciones)
vecDiffTimeCumReal<-rep(0, numAcciones)

  
for(i in 2:numAcciones) {
  if(dfVISIR_acciones$Alumno[i]==dfVISIR_acciones$Alumno[i-1]) {
    thisTime<-as.numeric(as.POSIXct(as.character(dfVISIR_acciones$FechaHoraEnvio[i]),
                                    format="%m/%d/%Y %H:%M:%S"))
    previousTime<-as.numeric(as.POSIXct(as.character(dfVISIR_acciones$FechaHoraEnvio[i-1]),
                                        format="%m/%d/%Y %H:%M:%S"))
    vecDiffTime[i]<-thisTime-previousTime
    
    vecDiffTimeCum[i]<-vecDiffTime[i]+vecDiffTimeCum[i-1]
    vecDiffTimeCumReal[i]<-ifelse(vecDiffTime[i]>timeLimit,0,vecDiffTime[i])+vecDiffTimeCumReal[i-1]
  } else {
    vecDiffTime[i]<-NA
    vecDiffTimeCum[i]<-0
    vecDiffTimeCumReal[i]<-0
  }
}
dfVISIR_accionesOrdenado$TiempoDesdeAccionAnterior <- vecDiffTime / 60
dfVISIR_accionesOrdenado$TiempoAcumulado <- vecDiffTimeCum / 60
dfVISIR_accionesOrdenado$TiempoAcumuladoCorregido <- vecDiffTimeCumReal / 60

ordenAlumnos <- dfVISIR_accionesOrdenado %>% group_by(Alumno) %>% 
  summarise(TiempoAcumuladoCorregidoMax = max(TiempoAcumuladoCorregido)) %>%
  arrange(TiempoAcumuladoCorregidoMax)


dfVISIR_accionesOrdenado$Alumno <- factor(dfVISIR_accionesOrdenado$Alumno, 
                                          levels=ordenAlumnos$Alumno, ordered=TRUE)

  #ANTIGUO

  #NUEVO
dfVISIR_accionesOrdenado$EsCircuito <- grepl("<circuitlist>",dfVISIR_accionesOrdenado$DatosEnviadosXML)

if(observaciones) {
  table(dfVISIR_accionesOrdenado$EsCircuito)
}
numCircuitos <- sum(dfVISIR_accionesOrdenado$EsCircuito)

dfVISIR_accionesOrdenado$NumCircuito <- 0
dfVISIR_accionesOrdenado$NumAccion <- 1
for(i in 2:nrow(dfVISIR_accionesOrdenado)) {
  if(dfVISIR_accionesOrdenado$Alumno[i] == dfVISIR_accionesOrdenado$Alumno[i-1]) {
    dfVISIR_accionesOrdenado$NumAccion[i] <- dfVISIR_accionesOrdenado$NumAccion[i-1] + 1
    if(dfVISIR_accionesOrdenado$EsCircuito[i]) {
      dfVISIR_accionesOrdenado$NumCircuito[i] <- dfVISIR_accionesOrdenado$NumCircuito[i-1] + 1 
    } else {
      dfVISIR_accionesOrdenado$NumCircuito[i] <- dfVISIR_accionesOrdenado$NumCircuito[i-1] 
    }
  }
}
dfVISIR_accionesOrdenado$NumCircuito[!dfVISIR_accionesOrdenado$EsCircuito]<-NA #Aquellos circuitos distintos de EScircuito son NA

#### 1B.ACCIONES CON CIRCUITO ####
dfVISIR_accionesCircuito <- dfVISIR_accionesOrdenado[dfVISIR_accionesOrdenado$EsCircuito,] 

tempCircuitos<-as.character(rep(NA,numCircuitos))
for(i in 1:numCircuitos){
  #El primer resultat de regexec es el match global, el 2:n son els extrets
  tempRegExpCircuito<-regexec("<circuitlist>([^<]*)",as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]))
  tempCircuitos[i]<-substr(dfVISIR_accionesCircuito$DatosEnviadosXML[i],tempRegExpCircuito[[1]][2],
                           tempRegExpCircuito[[1]][2]+attr(tempRegExpCircuito[[1]],"match.length")[2]-1)
  
}

tempMMConectado<-grepl("DMM_",dfVISIR_accionesCircuito$DatosEnviadosXML)
tempMMOperativo<-grepl("<dmm_function value=",dfVISIR_accionesCircuito$DatosEnviadosXML)
tempHayMedida<-tempMMConectado & tempMMOperativo

tempMedidas<-rep("",numCircuitos)
for(i in 1:numCircuitos){
  if(tempHayMedida[i]) {
    #El primer resultat de regexec es el match global, el 2:n son els extrets
    tempRegExpMedida<-regexec("<dmm_function value=.(.)",
                              as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]))
    print(tempRegExpMedida)
    tempMedidas[i]<-substr(as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]),
                           tempRegExpMedida[[1]][2],tempRegExpMedida[[1]][2]+4)
  }
}


dfVISIR_accionesCircuito<-cbind(dfVISIR_accionesCircuito,
                                Circuito = tempCircuitos,
                                MultimetroConectado=tempMMConectado,
                                MultimetroEncendido=tempMMOperativo,
                                EsMedida = tempHayMedida,
                                Medida = tempMedidas)

dfVISIR_accionesCircuito$Circuito<-gsub("W_X","/W_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
dfVISIR_accionesCircuito$Circuito<-gsub("R_X","/R_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
dfVISIR_accionesCircuito$Circuito<-substring(dfVISIR_accionesCircuito$Circuito,2)

normalizarCircuito<-function(x) {
  # x es una cadena
  if(is.null(x)) return(NA)
  if(is.na(x)) return(NA)
  if(x=="") return(NA)
  
  circuito <- as.character(x)
  circuito <- gsub("([^A-Z0-9])0([^A-Z0-9])", "\\1GND\\2", circuito)
  circuito <- gsub("([^A-Z0-9])0$", "\\1GND", circuito)
  circuito <- gsub("A([0-9][^0-9])", "A0\\1", circuito)
  circuito <- gsub("F1([0-9])", "A6\\1", circuito)
  circuito <- gsub("F2([0-9])", "A7\\1", circuito)
  circuito <- gsub("F3([0-9])", "A8\\1", circuito)
  circuito <- gsub("F([0-9])", "A5\\1", circuito)
  
  componentes <- strsplit(circuito,"/",fixed=TRUE)[[1]]
  for(i in 1:length(componentes)) {
    conectores <- strsplit(componentes[i], " ")[[1]]
    
    if(conectores[2] > conectores[3]) {
      aux <- conectores[3]
      conectores[3] <- conectores[2]
      conectores[2] <- aux
    }
    
    if(conectores[1] == "W_X" & substr(conectores[2],1,1) =="A") {
      componentes <- gsub(conectores[2], conectores[3], componentes, fixed=TRUE)
      componentes[i] <- ""
    } else {
      componentes[i] <- paste(conectores, collapse=" ")
    }
  }
  componentes <- componentes[order(gsub("A[0-9][0-9]","Axx",componentes))]
  circuito <- paste(componentes, collapse="/")  
  circuito <- gsub("^/*","",circuito)
  
  if(is.na(circuito)) return(NA)
  if(circuito=="") return(NA)
  
  componentes <- strsplit(circuito,"/",fixed=TRUE)[[1]]
  for(i in 1:length(componentes)) {
    conectores <- strsplit(componentes[i], " ")[[1]]
    
    if(conectores[2] > conectores[3]) {
      aux <- conectores[3]
      conectores[3] <- conectores[2]
      conectores[2] <- aux
    }
    
    componentes[i] <- paste(conectores, collapse=" ")
  }
  componentes <- componentes[order(gsub("A[0-9][0-9]","Axx",componentes))]
  circuito <- paste(componentes, collapse="/")  
  
  # unificar nodos
  nodos <- gregexpr("A[0-9][0-9]",circuito)[[1]]
  if(nodos[[1]]==-1)
    nodos <- character(0)
  else {
    nodos <- sapply(nodos, function(x) {substr(circuito,x,x+2)})
    nodos <- unique(nodos)
  }
  if(length(nodos)>0) {
    nodosUnif <- c(paste("P0",1:9,sep=""),paste("P",10:99,sep=""))
    nodosUnif <- nodosUnif[1:length(nodos)]
    matNodosUnif <- perm(nodosUnif)
    
    r_circuitos <- character(nrow(matNodosUnif))
    for(i in 1:nrow(matNodosUnif))
      r_circuitos <- replaceMany(circuito, nodos, matNodosUnif[i,])
    #print(r_circuitos)
    circuito <- min(r_circuitos)
  }
  return(circuito)
}



normalizarCircuitos<-function(x) {
  y <- character(length(x))
  for (i in 1:length(x)) {
    y[i]<-normalizarCircuito(x[i])
  }
  return(y)
}

tempCircuitoNormalizado <- normalizarCircuitos(dfVISIR_accionesCircuito$Circuito)

dfVISIR_accionesCircuito$CircuitoNormalizado <- tempCircuitoNormalizado

esCircuitoCerrado <- function(x) {
  # x es una cadena
  if(is.null(x)) return(NA)
  if(is.na(x)) return(NA)
  if(x=="") return(FALSE)
  
  circuito <- as.character(x)
  circuito <- gsub("([^A-Z0-9])0([^A-Z0-9])", "\\1GND\\2", circuito)
  circuito <- gsub("([^A-Z0-9])0$", "\\1GND", circuito)
  circuito <- gsub("A([0-9][^0-9])", "A0\\1", circuito)
  circuito <- gsub("F1([0-9])", "A6\\1", circuito)
  circuito <- gsub("F2([0-9])", "A7\\1", circuito)
  circuito <- gsub("F3([0-9])", "A8\\1", circuito)
  circuito <- gsub("F([0-9])", "A5\\1", circuito)
  circuito <- gsub("/", " ", circuito)
  
  codigos <- strsplit(circuito," ",fixed=TRUE)[[1]]
  codigos <- gsub("\\s+", " ", codigos)
  codigos <- gsub("^[^A].*", "", codigos)
  codigos <- paste(codigos, collapse = " ")
  codigos <- strsplit(codigos," ",fixed=TRUE)[[1]]
  codigos[codigos==""] <- NA
  !any(table(codigos)<2)
}


vecCircuitoCerrado <- sapply(dfVISIR_accionesCircuito$Circuito, esCircuitoCerrado)
dfVISIR_accionesCircuito$EsCircuitoCerrado <- vecCircuitoCerrado

colnames(dfVISIR_accionesCircuito)

vecMMMal <- grepl("(DMM_V.*DMM_A)|(DMM_A.*DMM_V)",
                  dfVISIR_accionesCircuito$CircuitoNormalizado)  
vecMMOKV <- !vecMMMal & grepl("(DMM_V.*DMM_V)",
                              dfVISIR_accionesCircuito$CircuitoNormalizado) 
vecMMOKA <- !vecMMMal & grepl("(DMM_A.*DMM_A)",
                              dfVISIR_accionesCircuito$CircuitoNormalizado) 
dfVISIR_accionesCircuito$MultimetroMal <- !vecMMOKV & !vecMMOKA  #No hay multimetro o estÃƒÂ¡ mal conectado
dfVISIR_accionesCircuito$MultimetroV <- vecMMOKV
dfVISIR_accionesCircuito$MultimetroA <- vecMMOKA
dfVISIR_accionesCircuito$MedidaCorrectaV <- dfVISIR_accionesCircuito$MultimetroV & grepl("dc v",dfVISIR_accionesCircuito$Medida)

dfVISIR_accionesCircuito$MedidaCorrectaA <- dfVISIR_accionesCircuito$MultimetroA & grepl("dc c",dfVISIR_accionesCircuito$Medida)

dfVISIR_accionesCircuito$MedidaCorrectaR <- dfVISIR_accionesCircuito$MultimetroV & grepl("resi",dfVISIR_accionesCircuito$Medida) & !grepl("DC_+",dfVISIR_accionesCircuito$DatosEnviadosXML)

Medida <- factor(ifelse(dfVISIR_accionesCircuito$MedidaCorrectaV,"Voltage",
                        ifelse(dfVISIR_accionesCircuito$MedidaCorrectaR,"Resistance",
                               ifelse(dfVISIR_accionesCircuito$MedidaCorrectaA,"Current",
                                      "NA"))),ordered=TRUE,levels=c("NA", "Voltage","Resistance","Current"))

dfVISIR_accionesCircuito$TypeofMesure <- Medida

dfVISIR_accionesCircuito$TypeofMesure <-as.factor(dfVISIR_accionesCircuito$TypeofMesure)

dfVISIR_accionesCircuito$Circuito[is.na(dfVISIR_accionesCircuito$EsCircuitoCerrado)]



return(dfVISIR_accionesCircuito)


}

FunctionOrderTime <- function (dfVISIR_acciones) {
  
  numAcciones<-nrow(dfVISIR_acciones)
  
  
  
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  
  
  
  dfVISIR_accionesOrdenado$Dates <- format(as.Date(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y")
  dfVISIR_accionesOrdenado$Hours <- format(as.POSIXct(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y %H")
  
  X_X <- dfVISIR_accionesOrdenado%>% select(Alumno,Dates,Hours,FechaHoraEnvio)
  
  X_X$FechaHoraEnvio<-as.POSIXct(as.character(X_X$FechaHoraEnvio),
                                 format="%m/%d/%Y %H:%M:%S")
  
  
  ordenAlumno <- X_X %>% group_by(Alumno,Dates,Hours) %>% arrange(FechaHoraEnvio) %>% 
    summarise(TotalTime=sum(diff(FechaHoraEnvio))) %>% mutate(TotalTime=TotalTime/60) %>% mutate(TotalTime=round(TotalTime,digits = 2)) %>% 
    group_by(Alumno) %>% summarise(TotalTime=sum(TotalTime))
  
  
  ordenAlumno$TotalTime <-as.numeric(ordenAlumno$TotalTime)
  

  return(ordenAlumno)
  
}

FunctionSSA <- function (dfVISIR_acciones) {
  
  
  
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  
  
  
  dfVISIR_accionesOrdenado$Dates <- format(as.Date(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y")
  dfVISIR_accionesOrdenado$Dates <- as.Date(dfVISIR_accionesOrdenado$Dates,"%m/%d/%Y")
  
  Acciones <- dfVISIR_accionesOrdenado %>% select(Dates,Sesion,Alumno) %>%group_by(Dates) %>% 
    summarise(Sesion=length(unique(Sesion)),Alumnos=length(unique(Alumno)),Acciones=length(Dates))
  
  
  
  return(Acciones)
  
}

FunctionTimeStud <- function (dfVISIR_acciones,dfActionCircuit) {
  
  numAcciones<-nrow(dfVISIR_acciones)
  
  
  
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  
  
  
  
  #Parte Tiempo
  
  dfVISIR_accionesOrdenado$Dates <- format(as.Date(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y")
  dfVISIR_accionesOrdenado$Hours <- format(as.POSIXct(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y %H")
  
  X_P <- dfVISIR_accionesOrdenado%>% select(Alumno,Dates,Hours,FechaHoraEnvio)
  
  X_P$FechaHoraEnvio<-as.POSIXct(as.character(X_P$FechaHoraEnvio),
                                 format="%m/%d/%Y %H:%M:%S")
  

  TimeStud <- X_P %>% group_by(Alumno,Dates,Hours) %>% arrange(FechaHoraEnvio) %>% 
    summarise(TotalTime=sum(diff(FechaHoraEnvio))) %>% mutate(TotalTime=TotalTime/60) %>% mutate(TotalTime=round(TotalTime,digits = 2)) %>% 
    group_by(Alumno) %>% summarise(TotalTime=sum(TotalTime))
  
 
  #Parte Circuitos
  
  CircuTimebyStud <- dfActionCircuit %>% select(Alumno,Circuito) %>%  group_by(Alumno) %>% 
    summarise(NumCircu=length(unique(Circuito)))
  
  
  # Mix Both Data Frames (Student Circuits Time)
  zz <- merge(CircuTimebyStud, TimeStud, all = TRUE)
  
  zz$TotalTime <- as.numeric(zz$TotalTime)
  
  MeanNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(Numcirc=length(unique(Circuito)))
  MeanNumCircSVV <- round(mean(MeanNAcircu$Numcirc),digits = 2)
  
  zz$NumCircu <- as.numeric(zz$NumCircu)
  
  zz$Evaluation <- ifelse(zz$NumCircu > MeanNumCircSVV, "UpAverageCircuits","DownAverageCircuits")
  
  zz$Evaluation <- as.factor(zz$Evaluation)
  
  
  return(zz)

  
}

FunctionMTS <- function (dfVISIR_acciones) {
  
  
  
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  
  
  
  dfVISIR_accionesOrdenado$Dates <- format(as.Date(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y")
  dfVISIR_accionesOrdenado$Hours <- format(as.POSIXct(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y %H")
  dfVISIR_accionesOrdenado$Dates <- as.Date(dfVISIR_accionesOrdenado$Dates,"%m/%d/%Y")
  
  dfVISIR_accionesOrdenado$FechaHoraEnvio<-as.POSIXct(as.character(dfVISIR_accionesOrdenado$FechaHoraEnvio),
                                                      format="%m/%d/%Y %H:%M:%S")
  
  Totaltimebydate <- dfVISIR_accionesOrdenado %>% select(Alumno,Dates,Hours,FechaHoraEnvio) %>% group_by(Alumno,Dates,Hours) %>%
    arrange(FechaHoraEnvio) %>% summarise(Diffti=sum(diff(FechaHoraEnvio))) %>%
    group_by(Dates,Alumno) %>% summarise(Time=sum(Diffti)) %>% mutate(Time=Time/60) %>% 
    mutate(Time=round(Time,digits = 2))
  
  
  Totaltimebydate$Time<-as.numeric(Totaltimebydate$Time)
  class( Totaltimebydate$Dates)
  Totaltimebydate$Dates <- as.factor(Totaltimebydate$Dates)
  Totaltimebydate$Alumno <- as.factor(Totaltimebydate$Alumno)
  names(Totaltimebydate)[names(Totaltimebydate) == 'Alumno'] <- 'Student'
  
  return(Totaltimebydate)
  
}







# 1 TAB DATA INPUT

Dygraphfunc <- function(dfSSA) {
  if(is.null(dfSSA)) return(NULL)
  
  ses <- zoo(dfSSA$Sesion,dfSSA$Dates)
  con<-zoo(dfSSA$Acciones,dfSSA$Dates)
  cr<-zoo(dfSSA$Alumnos,dfSSA$Dates)
  ga2Su<-cbind(ses, con,cr)
  
  
  dygraph(ga2Su)%>% dySeries("ses", label = "Sessions",axis="y",stepPlot=TRUE,fillGraph=TRUE) %>% 
    dySeries("con",fillGraph=TRUE, label = "Actions",axis="y") %>% dySeries("cr", label = "Students",axis="y2",strokePattern = "dashed") %>%
    dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()

  
}

# 2 TAB G RES NUMBER OF CIRCUITS
   # 2.1 First Row




InfovalueBoxNCC <- function(dfActionCircuit){
  
  
  Accircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Circuito) %>% summarise(NuCC= sum(unique(EsCircuitoCerrado)))
  
  NumccircuVC<-sum(Accircu$NuCC)
  
  return(NumccircuVC)
  
}
InfovalueBoxNNC <- function(dfActionCircuit){
  
  
  Ancircu <-dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
  
  NumncircuVC<-length(unique(Ancircu$CircuitoNormalizado))
  
  return(NumncircuVC)
  
}
# 2.2 Second Row



InfovalueBoxMNCC <- function(dfActionCircuit){
  
  
  MeanCNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.)) %>%
    group_by(Alumno,Circuito) %>%  summarise(NumCcircuit=sum(unique(EsCircuitoCerrado))) %>%
    group_by(Alumno) %>%  summarise(NumCcircuitS=sum(NumCcircuit))
  
  MeanNumCCirSV <- round(mean(MeanCNAcircu$NumCcircuitS),digits = 2)
  
  return(MeanNumCCirSV)
  
}

InfovalueBoxMNNC <- function(dfActionCircuit){
  
  
  MeanNNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(Numncirc=length(unique(CircuitoNormalizado)))
  
  MeanNumNCircSV <- round(mean(MeanNNAcircu$Numncirc),digits = 2)
  
  return(MeanNumNCircSV)
  
}


  # 2.3 DYGRAPH 2

Dygraphfunc2 <- function(dfActionCircuit) {
 
  
  ADYygraph <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
  
  ADYygraph$Dates <- format(as.Date(ADYygraph$FechaHoraEnvio, format="%m/%d/%Y %H:%M:%S"),"%m/%d/%Y")
  
  ADYygraph$Dates <- as.Date(ADYygraph$Dates,"%m/%d/%Y")
  
  MeanNumCStudDate <- ADYygraph %>% group_by(Alumno,Dates) %>% summarise(MeanNumCircSD=length(unique(Circuito))) %>% #Data frame con el numero medio de ciruitos (BASIC) por estudiante por dia
    group_by(Dates) %>% summarise(MeanNumCircSD=mean(MeanNumCircSD,na.rm=TRUE)) %>% 
    mutate(MeanNumCircSD=round(MeanNumCircSD,digits = 2)) 
  
  MeanNumCCStudDate <- ADYygraph %>% group_by(Alumno,Dates,Circuito) %>% summarise(NumCCSD=sum(unique(EsCircuitoCerrado))) %>% #Data frame con el numero medio de ciruitos (Cerrados) por estudiante por dia
    group_by(Alumno,Dates) %>% summarise(NumCCD=sum(NumCCSD)) %>% group_by(Dates) %>% 
    summarise(MeanNumCCSD=mean(NumCCD,na.rm=TRUE)) %>% 
    mutate(MeanNumCCSD=round(MeanNumCCSD,digits = 2))
  
  MeanNumNCStudDate <- ADYygraph %>% group_by(Alumno,Dates) %>% summarise(MeanNumNCircSD=length(unique(CircuitoNormalizado))) %>% #Data frame con el numero medio de ciruitos (BASIC) por estudiante por dia
    group_by(Dates) %>% summarise(MeanNumNCircSD=mean(MeanNumNCircSD,na.rm=TRUE)) %>% 
    mutate(MeanNumNCircSD=round(MeanNumNCircSD,digits = 2))
  
  MeanNumCStudDate$MeanNumCCSD <- MeanNumCCStudDate$MeanNumCCSD
  
  MeanNumCStudDate$MeanNumNCSD <- MeanNumNCStudDate$MeanNumNCircSD
  
  ses <- zoo(MeanNumCStudDate$MeanNumCircSD,MeanNumCStudDate$Dates)
  con<-zoo(MeanNumCStudDate$MeanNumCCSD,MeanNumCStudDate$Dates)
  cr<-zoo(MeanNumCStudDate$MeanNumNCSD,MeanNumCStudDate$Dates)
  
  ga2Sus<-cbind(ses, con,cr)
  
  
  dygraph(ga2Sus)%>% dyAxis("x",rangePad=c(-0.05)) %>%  dySeries("ses", label = "Standard",axis="y") %>% 
    dySeries("con",label = "Closed",axis="y",drawPoints = TRUE) %>% dySeries("cr", label = "Normalized",axis="y",drawPoints = TRUE) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()
  
}




# 1 SECCION CIRCUIT DISTRIBUTION BOXES

## 1.1 STANDARD CIRCUITS

###  A )) TAB PANEL CIRCUITOS NORMALES UNICOS



InfovalueBoxNC <- function(dfActionCircuit){
  
  
  Acircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
  
  NumcircuV<-length(unique(Acircu$Circuito))
  
  return(NumcircuV)
  
}
InfovalueBoxMNC <- function(dfActionCircuit){
  
  
  MeanNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(Numcirc=length(unique(Circuito)))
  
  MeanNumCircSV <- round(mean(MeanNAcircu$Numcirc),digits = 2)
  
  return(MeanNumCircSV)
  
}
InfovalueBoxlowbound <- function(dfActionCircuit){
  
  lowboundf <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(Circuito =length(unique(Circuito)))
  
  mincircv <- min(lowboundf$Circuito)
  
  return(mincircv)
}
InfovalueBoxupbound <- function(dfActionCircuit){
  
  upboundf <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(Circuito =length(unique(Circuito)))
  
  maxcircv <- max(upboundf$Circuito)
  
  return(maxcircv)
}
InfovalueBoxmeanstud <- function(dfActionCircuit){
  
  upboundf <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(Circuito =length(unique(Circuito)))
  
  upbound <- ceiling(mean(upboundf$Circuito) + sd(upboundf$Circuito))

  
  lowbound <- floor(mean(upboundf$Circuito) - sd(upboundf$Circuito))
  
  prop <- mean(upboundf$Circuito >= lowbound &
                 upboundf()$Circuito <= upbound) * 100
  
  strx<-round(prop,0)
  
  return(strx)
}


# GRAFICOS DISTRIBUCION CIRCUITOS NORMALES

distnumcirc <- function(dfActionCircuit){
  
  
  CircByStudentD <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(Circuito =length(unique(Circuito)))
  
  return(CircByStudentD)
  
}

### B )) TAB PANEL CIRCUITOS NORMALIZADOS UNICOS


InfovalueBoxNNC <- function(dfActionCircuit){
  
  
  Ancircu <-dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
  
  NumncircuVC<-length(unique(Ancircu$CircuitoNormalizado))
  
  return(NumncircuVC)
  
}

InfovalueBoxMNNC <- function(dfActionCircuit){
  
  
  MeanNNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(Numncirc=length(unique(CircuitoNormalizado)))
  
  MeanNumNCircSV <- round(mean(MeanNNAcircu$Numncirc),digits = 2)
  
  return(MeanNumNCircSV)
  
}

InfovalueBoxlowboundN <- function(dfActionCircuit){
  
  lowboundfN <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(CircuitoNormalizado =length(unique(CircuitoNormalizado)))
  
  mincircunv <- min(lowboundfN$CircuitoNormalizado)
  
  return(mincircunv)
}

InfovalueBoxupboundN <- function(dfActionCircuit){
  
  upboundfN <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(CircuitoNormalizado =length(unique(CircuitoNormalizado)))
  
  maxcircnv <- max(upboundfN$CircuitoNormalizado)
  
  return(maxcircnv)
}


# GRAFICOS DISTRIBUCION CIRCUITOS NORMALIZADOS

distnumcircN <- function(dfActionCircuit){
  
  
  CircByStudentDN <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno)%>%  summarise(CircuitoNormalizado =length(unique(CircuitoNormalizado)))
  
  return(CircByStudentDN)
  
}


# TYPE OF MESURE


functymes <- function(dfActionCircuit){
  
  Acircu <- dfActionCircuit %>% select(CircuitoNormalizado,TypeofMesure)%>% filter(complete.cases(.))
  
  
  dfGraficobar <- Acircu %>% group_by(CircuitoNormalizado,TypeofMesure) %>%  summarise(Frequency=length(unique(TypeofMesure))) %>% 
    group_by(TypeofMesure) %>% summarise(Frequency=sum(Frequency))
  
  dfGraficobar <- mutate(dfGraficobar, Percentage = Frequency/sum(Frequency)*100) %>% 
    mutate(Percentage=round(Percentage,digits = 2))
  
  dfGraficobar$Percentage<-percent(dfGraficobar$Percentage)
  
  a <- hPlot(Percentage ~ TypeofMesure, data =dfGraficobar, type = 'bar', group.na = 'NA\'s')
  a$show('inline', include_assets = TRUE, cdn = TRUE)
  return(a) 
  
 
}


      #Type of mesure values

ValueBoxnumvolts <- function(dfActionCircuit){
  
  Voltsdf<- dfActionCircuit %>% select(CircuitoNormalizado,TypeofMesure)%>% filter(complete.cases(.)) %>% 
    group_by(CircuitoNormalizado,TypeofMesure) %>%  summarise(Frequency=length(unique(TypeofMesure))) %>% 
    group_by(TypeofMesure) %>% summarise(Frequency=sum(Frequency))  
  
  Voltsv <-unlist(c(Voltsdf[as.vector(c(4)),as.vector(c(2))]),use.names=FALSE)
  
  return(Voltsv)
}



ValueBoxnumresis <- function(dfActionCircuit){
  
  Resisdf<- dfActionCircuit %>% select(CircuitoNormalizado,TypeofMesure)%>% filter(complete.cases(.)) %>% 
    group_by(CircuitoNormalizado,TypeofMesure) %>%  summarise(Frequency=length(unique(TypeofMesure))) %>% 
    group_by(TypeofMesure) %>% summarise(Frequency=sum(Frequency))  
  
  Resisv <-unlist(c(Resisdf[as.vector(c(3)),as.vector(c(2))]),use.names=FALSE)
  
  return(Resisv)
}  

ValueBoxnumna <- function(dfActionCircuit){
  
  nadf<- dfActionCircuit %>% select(CircuitoNormalizado,TypeofMesure)%>% filter(complete.cases(.)) %>% 
    group_by(CircuitoNormalizado,TypeofMesure) %>%  summarise(Frequency=length(unique(TypeofMesure))) %>% 
    group_by(TypeofMesure) %>% summarise(Frequency=sum(Frequency))  
  
  nav <-unlist(c(nadf[as.vector(c(2)),as.vector(c(2))]),use.names=FALSE)
  
  return(nav)
}  

ValueBoxnumint <- function(dfActionCircuit){
  
  intdf<- dfActionCircuit %>% select(CircuitoNormalizado,TypeofMesure)%>% filter(complete.cases(.)) %>% 
    group_by(CircuitoNormalizado,TypeofMesure) %>%  summarise(Frequency=length(unique(TypeofMesure))) %>% 
    group_by(TypeofMesure) %>% summarise(Frequency=sum(Frequency))  
  
  intv <-unlist(c(intdf[as.vector(c(1)),as.vector(c(2))]),use.names=FALSE)
  
  return(intv)
}  






plotDistribution <- function(time=NA,maxTime=NULL,xlabel="") {
  if(is.null(time)) return(NULL)
  
  if (is.null(maxTime)) {
    maxTime<-max(time,na.rm=TRUE)
    maxTime<-ceiling(maxTime/10^floor(log(maxTime)/log(10)-1))*10^floor(log(maxTime)/log(10)-1)
  }
  
  # Freedman-Diaconis rule
  bw <- maxTime / (2 * IQR(time) / length(time)^(1/3))
  
  bw2 <- floor(bw/10^floor(log(bw)/log(10)))
  bw2<-ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2<-bw2*10^floor(log(bw)/log(10))
  
  nbins<-ceiling(diff(range(time))/bw2)
  bw2<-ifelse(nbins>=7,bw2,bw2/2+.Machine$double.eps)
  bw2 <- floor(bw2/10^floor(log(bw2)/log(10)))*10^floor(log(bw2)/log(10))
  
  ggmaxTime<-ceiling(maxTime/bw2)*bw2
  
  dense<-density(time,adjust=1,bw="SJ")
  df_density=data.frame(x=dense$x,y=dense$y/sum(dense$y)*bw2*
                          length(time)/mean(diff(dense$x)))
  theme_remove_all <- theme(
    axis.text.x = element_text(margin=margin(0,0,0,0,"lines")),
    axis.ticks.length = unit(0, "cm"))
  
  ghist<-ggplot(data=NULL,aes(x=time)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue",
                   breaks=seq(0,ggmaxTime, by=bw2))+ 
    geom_area(data=df_density,aes(x=x,y=y),
              alpha=.2, fill="#FFFF00", color="#CC6600", size=1) + 
    geom_vline(aes(xintercept=mean(time)), linetype="dashed", size=1)+
    geom_rug(alpha=.5) +
    labs(x=xlabel, y="Frequency")+
    coord_cartesian(xlim= c(0, ggmaxTime)) +
    theme_bw() + theme_remove_all +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))
  
  ghist
}

### TIEMPO ON TASK ####

## DYGRAPH 3 Tiempo medio dedicado por alumno y por fecha

 
plotlyfunc3 <- function(dfMTS) {
  if(is.null(dfMTS)) return(NULL)
  
  ggplot(data = dfMTS, aes(x = Student, y = Dates)) +
    geom_tile(aes(fill = Time))                                
  
  
  
  
}

## TIME PER STUDENT DISTRIBUTION

#INFOBOXES

InfovalueBoxTT <- function(dfOrderTime){
  
  
  
  tottime<-sum(dfOrderTime$TotalTime/60)
  
  tottime<-round(tottime,digits = 2)
  
  return(tottime)
  
}
InfovalueBoxMeT <- function(dfOrderTime){
  


  meantime <- round(mean(dfOrderTime$TotalTime),digits = 2)
  
  return(meantime)
  
}
InfovalueBoxMaxT <- function(dfOrderTime){
  
  maxtime<-round(max(dfOrderTime$TotalTime/60),digits = 2)
  
  return(maxtime)
}

InfovalueBoxMinT <- function(dfOrderTime){
  
  mintime<-round(min(dfOrderTime$TotalTime),digits = 2)
  
  return(mintime)
}





####### ANTIGUO #####
  
 # 2.1 NUMERO DE CIRCUITOS
Numcirc <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
      ggplot(dfActionCircuit,
               aes(x=cut(NumCircuito,breaks=(0:20*20))))+
    geom_bar()+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="",title="Number of circuits")
  
}

# 2.2 PROPORCIÓN DE CIRCUITOS CERRADOS BILBAO

Propcirccerrb <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Bilbao",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=factor(as.character(EsCircuitoCerrado))))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("#e08c0d","#67a6c6"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="",title="Proportion of closed circuits BILBAO")
  
}
  
# 2.3 PROPORCIÓN DE CIRCUITOS CERRADOS DONOSTIA

Propcirccerrd <- function(dfActionCircuit=NA) {
if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Donostia",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=factor(as.character(EsCircuitoCerrado))))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("#e08c0d","#67a6c6"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="",title="Proportion of closed circuits DONOSTIA")
  
  
}


# 2.4 PROPORCIÓN DE MM BIEN CONECTADOS Y CONECTADOS DONOSTIA

Propcirmmokcond <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Donostia",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=factor(as.character(!MultimetroMal))))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("red","darkgreen"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="", title="Proportion of Multimeters OK and properly connected DONOSTIA")
}

# 2.5 PROPORCIÓN DE MM BIEN CONECTADOS Y CONECTADOS BILBAO

Propcirmmokconb <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Bilbao",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=factor(as.character(!MultimetroMal))))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("red","darkgreen"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(legend.position = "none",
          axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="", title="Proportion of Multimeters OK and properly connected BILBAO")
}


# 3 SECCION
  
# 3.1 PROPORCIÓN DE LAS DISTINTAS MEDIDAS CONFORME AL NÚMERO DE CIRCUITOS DONOSTIA

Propmedynumd <-function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)

  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Donostia",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=TypeofMesure))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("red","darkgreen","blue","orange2"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="", title="Proportion of the different measures according to the number of circuits")
  
  }
# 3.2 PROPORCIÓN DE LAS DISTINTAS MEDIDAS CONFORME AL NÚMERO DE CIRCUITOS BILBAO

Propmedynumb <-function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Bilbao",],
         aes(x=cut(NumCircuito,breaks=(0:20*20)),
             fill=TypeofMesure ))+
    geom_bar(position="fill")+
    scale_fill_manual(values=c("darkgreen","red","blue","orange2"))+
    scale_x_discrete(labels=c(paste("(",0:19*20,",",1:20*20,"]",sep=""),">400"))+
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
    labs(x="Num. circuito", y="", title="Proportion of the different measures according to the number of circuits BILBAO")
  
}

# 3.3 TIEMPO ACUMULADO POR ALUMNO SEGÚN EL TIPO DE MEDIDA GENERAL

Tiempalumng <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit,
         aes(x = Alumno, y = TiempoAcumuladoCorregido, col=TypeofMesure)) + 
    geom_point(size = 1.5, shape = 4) +
    scale_color_manual(values=c("red","darkgreen","blue","orange2")) +
    theme_few() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(title="Todos")

}

# 3.4 TIEMPO ACUMULADO POR ALUMNO SEGÚN EL TIPO DE MEDIDA DONOSTIA

Tiempalummedd <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Donostia",],
         aes(x = Alumno, y = TiempoAcumuladoCorregido, col=TypeofMesure)) + 
    geom_point(size = 1.5, shape = 4, 
               position=position_jitter(height=0.2, width=0.2)) +
    scale_color_manual(values=c("red","darkgreen","blue","orange2")) +
    facet_wrap(~TypeofMesure)+
    theme_few() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(title="Donostia")

}

# 3.5 TIEMPO ACUMULADO POR ALUMNO SEGÚN EL TIPO DE MEDIDA BILBAO

Tiempalummedb <- function(dfActionCircuit=NA) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  ggplot(dfActionCircuit[dfActionCircuit$Sede=="Bilbao",],
         aes(x = Alumno, y = TiempoAcumuladoCorregido, col=TypeofMesure)) + 
    geom_point(size = 1.5, shape = 4, 
               position=position_jitter(height=0.2, width=0.2)) +
    scale_color_manual(values=c("red","darkgreen","blue","orange2")) +
    facet_wrap(~TypeofMesure)+
    theme_few() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(title="Donostia")
  
}
