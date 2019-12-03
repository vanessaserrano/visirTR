#### INTERNAL FUNCTIONS ####
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

normalizarCircuito<-function(x) {
  ## Comprobación de la cadena
  # x es una cadena
  if(is.null(x)) return(NA)
  if(is.na(x)) return(NA)
  if(x=="") return(NA)
  
  ## Substitución del 0 por GND
  circuito <- as.character(x)
  circuito <- gsub("([^A-Z0-9])0([^A-Z0-9])", "\\1GND\\2", circuito)
  circuito <- gsub("([^A-Z0-9])0$", "\\1GND", circuito)
  
  ## Cambio de codificaciones en los nodos
  circuito <- gsub("A([0-9][^0-9])", "A0\\1", circuito)
  circuito <- gsub("F1([0-9])", "A6\\1", circuito)
  circuito <- gsub("F2([0-9])", "A7\\1", circuito)
  circuito <- gsub("F3([0-9])", "A8\\1", circuito)
  circuito <- gsub("F([0-9])", "A5\\1", circuito)
  circuito <- gsub("([^A-Z0-9_])X([^A-Z0-9_])", "\\1A90\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])X$", "\\1A90", circuito)
  circuito <- gsub("([^A-Z0-9_])Y([^A-Z0-9_])", "\\1A91\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])Y$", "\\1A91", circuito)
  circuito <- gsub("([^A-Z0-9_])S([^A-Z0-9_])", "\\1A92\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])S$", "\\1A92", circuito)
  circuito <- gsub("([^A-Z0-9_])T([^A-Z0-9_])", "\\1A93\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])T$", "\\1A93", circuito)
  
  ## Creación de los listados y eliminación de los cables innecesarios
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
  
  ## Unificación de los componentes
  circuito <- paste(componentes, collapse="/")  
  circuito <- gsub("^/*","",circuito)
  
  if(is.na(circuito)) return(NA)
  if(circuito=="") return(NA)
  
  ## Reordenación de los componentes
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
  
  ## Unificación de los nodos
  nodos <- gregexpr("A[0-9][0-9]",circuito)[[1]]
  if(nodos[[1]]==-1) {
    nodos <- character(0)
  } else {
    nodos <- sapply(nodos, function(x) {substr(circuito,x,x+2)})
    nodos <- unique(nodos)
  }
  # if(length(nodos)>0 & length(nodos)<9) {
  if(length(nodos)>0) {
    nodosUnif <- c(paste("P0",1:9,sep=""),paste("P",10:99,sep=""))
    nodosUnif <- nodosUnif[1:length(nodos)]
    # matNodosUnif <- perm(nodosUnif)
    # 
    # r_circuitos <- character(nrow(matNodosUnif))
    # for(i in 1:nrow(matNodosUnif))
    #   r_circuitos[i] <- replaceMany(circuito, nodos, matNodosUnif[i,])
    # circuito <- min(r_circuitos)
    
    circuito <- replaceMany(circuito, nodos, nodosUnif)
  }
  return(circuito)
}

normalizarCircuitos<-function(x) {
  y <- character(length(x))
  for (i in 1:length(x)) {
    y[i]<-normalizarCircuito(x[i])
    # print(i)
  }
  return(y)
}

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
  circuito <- gsub("([^A-Z0-9_])X([^A-Z0-9_])", "\\1A90\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])X$", "\\1A90", circuito)
  circuito <- gsub("([^A-Z0-9_])Y([^A-Z0-9_])", "\\1A91\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])Y$", "\\1A91", circuito)
  circuito <- gsub("([^A-Z0-9_])S([^A-Z0-9_])", "\\1A92\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])S$", "\\1A92", circuito)
  circuito <- gsub("([^A-Z0-9_])T([^A-Z0-9_])", "\\1A93\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])T$", "\\1A93", circuito)
  circuito <- gsub("/", " ", circuito)
  
  codigos <- strsplit(circuito," ",fixed=TRUE)[[1]]
  codigos <- gsub("\\s+", " ", codigos)
  codigos <- gsub("^[^A].*", "", codigos)
  codigos <- paste(codigos, collapse = " ")
  codigos <- strsplit(codigos," ",fixed=TRUE)[[1]]
  codigos[codigos==""] <- NA
  !any(table(codigos)<2)
}

#### PUBLIC FUNCTIONS ####
### >> DATASETS ####
funActionCircuit <- function (dfVISIR_acciones, timeLimit = 900) {
  numAcciones<-nrow(dfVISIR_acciones)
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]

  dfVISIR_accionesOrdenado$FechaHoraEnvio <- as.factor(dfVISIR_accionesOrdenado$FechaHoraEnvio)
  
  #Tiempo de trabajo
  vecDiffTime<-rep(NA,numAcciones)
  vecDiffTimeCum<-rep(0, numAcciones)
  vecDiffTimeCumReal<-rep(0, numAcciones)
  
  for(i in 2:numAcciones) {
    if(dfVISIR_accionesOrdenado$Alumno[i]==dfVISIR_accionesOrdenado$Alumno[i-1]) {
      thisTime<-as.numeric(as.POSIXct(as.character(dfVISIR_accionesOrdenado$FechaHoraEnvio[i]),
                                      format="%Y-%m-%d %H:%M:%S"))
      previousTime<-as.numeric(as.POSIXct(as.character(dfVISIR_accionesOrdenado$FechaHoraEnvio[i-1]),
                                          format="%Y-%m-%d %H:%M:%S"))
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
  
  vecTiempoMax <- ordenAlumnos$TiempoAcumuladoCorregidoMax
  
  dfVISIR_accionesOrdenado$Alumno <- factor(dfVISIR_accionesOrdenado$Alumno, 
                                            levels=ordenAlumnos$Alumno, ordered=TRUE)
  
  dfVISIR_accionesOrdenado$EsCircuito <- grepl("<circuitlist>",
    dfVISIR_accionesOrdenado$DatosEnviadosXML)

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
  # Aquellos circuitos distintos de EsCircuito deben ser NA
  dfVISIR_accionesOrdenado$NumCircuito[!dfVISIR_accionesOrdenado$EsCircuito]<-NA 

  dfVISIR_accionesCircuito <- dfVISIR_accionesOrdenado[dfVISIR_accionesOrdenado$EsCircuito,] 
  
  tempCircuitos<-as.character(rep(NA,numCircuitos))
  for(i in 1:numCircuitos){
    #El primer resultat de regexec es el match global, el 2:n son els extrets
    tempRegExpCircuito<-regexec("<circuitlist>([^<]*)",as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]))
    tempCircuitos[i]<-substr(dfVISIR_accionesCircuito$DatosEnviadosXML[i],tempRegExpCircuito[[1]][2],
                             tempRegExpCircuito[[1]][2]+attr(tempRegExpCircuito[[1]],"match.length")[2]-1)
  }
  
  tempCircuitos <- gsub("[\n\r]","", tempCircuitos)
  
  tempMMConectado<-grepl("(?:DMM_)|(?:IPROBE_)",dfVISIR_accionesCircuito$DatosEnviadosXML)
  tempMMOperativo<-grepl("<dmm_function value=",dfVISIR_accionesCircuito$DatosEnviadosXML)
  tempHayMedida<-tempMMConectado & tempMMOperativo
  
  tempMedidas<-rep("",numCircuitos)
  for(i in 1:numCircuitos){
    if(tempHayMedida[i]) {
      #El primer resultat de regexec es el match global, el 2:n son els extrets
      tempRegExpMedida<-regexec("<dmm_function value=.(.)",
                                as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]))
      #print(tempRegExpMedida)
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
  
  # Para version HTML5
  dfVISIR_accionesCircuito$Circuito<-gsub("DMM_1 DMM_1_1 DMM_1_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("DMM_2 DMM_2_1 DMM_2_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC+6V_1 VDC+6V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC+6V_2 VDC+6V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC+25V_1 VDC+25V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC+25V_2 VDC+25V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("IPROBE_1 IPROBE_1_1 IPROBE_1_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("IPROBE_2 IPROBE_2_1 IPROBE_2_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDCCOM_1 VDCCOM_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDCCOM_2 VDCCOM_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC-25V_1 VDC-25V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC-25V_2 VDC-25V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC-6V_1 VDC-6V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC-6V_2 VDC-6V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("PROBE1_1 PROBE1_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("PROBE1_2 PROBE1_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  
  dfVISIR_accionesCircuito$Circuito<-gsub("W_X","/W_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("R_X","/R_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-substring(dfVISIR_accionesCircuito$Circuito,2)

  tempCircuitoNormalizado <- normalizarCircuitos(dfVISIR_accionesCircuito$Circuito)

  dfVISIR_accionesCircuito$CircuitoNormalizado <- tempCircuitoNormalizado

  vecCircuitoCerrado <- sapply(dfVISIR_accionesCircuito$Circuito, esCircuitoCerrado)
  dfVISIR_accionesCircuito$EsCircuitoCerrado <- vecCircuitoCerrado

  vecMMMal <- grepl("(?:DMM_V.*DMM_A)|(?:DMM_A.*DMM_V)",
                    dfVISIR_accionesCircuito$CircuitoNormalizado)  
  vecMMOKV <- !vecMMMal & grepl("(?:DMM_V.*DMM_V)|(?:DMM_1_.*DMM_1_)|(?:DMM_2_.*DMM_2_)",
                                dfVISIR_accionesCircuito$CircuitoNormalizado) 
  vecMMOKA <- !vecMMMal & grepl("(?:DMM_A.*DMM_A)|(?:IPROBE_1_.*IPROBE_1_)|(?:IPROBE_2_.*IPROBE_2_)",
                                dfVISIR_accionesCircuito$CircuitoNormalizado) 
  dfVISIR_accionesCircuito$MultimetroMal <- !vecMMOKV & !vecMMOKA  #No hay multimetro o estÃ¡ mal conectado
  dfVISIR_accionesCircuito$MultimetroV <- vecMMOKV
  dfVISIR_accionesCircuito$MultimetroA <- vecMMOKA
  dfVISIR_accionesCircuito$MedidaCorrectaV <- dfVISIR_accionesCircuito$MultimetroV & grepl("dc v",dfVISIR_accionesCircuito$Medida)
  
  dfVISIR_accionesCircuito$MedidaCorrectaA <- dfVISIR_accionesCircuito$MultimetroA & grepl("dc c",dfVISIR_accionesCircuito$Medida)
  
  dfVISIR_accionesCircuito$MedidaCorrectaR <- dfVISIR_accionesCircuito$MultimetroV & grepl("resi",dfVISIR_accionesCircuito$Medida) & !grepl("DC_+",dfVISIR_accionesCircuito$DatosEnviadosXML)
  
  Medida <- factor(ifelse(dfVISIR_accionesCircuito$MedidaCorrectaV,"Voltage",
                          ifelse(dfVISIR_accionesCircuito$MedidaCorrectaR,"Resistance",
                                 ifelse(dfVISIR_accionesCircuito$MedidaCorrectaA,"Current",
                                        "Error"))),ordered=TRUE,levels=c("Current", "Resistance","Voltage","Error"))
  
  dfVISIR_accionesCircuito$Measure <- as.factor(Medida)
  names(dfVISIR_accionesCircuito)[names(dfVISIR_accionesCircuito) == 'TiempoAcumuladoCorregido'] <- 'Time'
  
  names(dfVISIR_accionesOrdenado)[names(dfVISIR_accionesOrdenado) == 'TiempoAcumuladoCorregido'] <- 'Time'
  
  return(dfVISIR_accionesCircuito)
}

funOrderTime <- function (dfVISIR_accionesOrdenado) {
  ordenAlumno <- dfVISIR_accionesOrdenado %>% select(Alumno, Time) %>% group_by(Alumno) %>% 
    summarise(TotalTime=max(Time))
  
  ordenAlumno$TotalTime <-as.numeric(ordenAlumno$TotalTime)
  return(ordenAlumno)
}

FunctionSSA <- function (dfVISIR_acciones) {
  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  dfVISIR_accionesOrdenado$Dates <- format(as.Date(dfVISIR_accionesOrdenado$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")
  dfVISIR_accionesOrdenado$Dates <- as.Date(dfVISIR_accionesOrdenado$Dates,"%Y-%m-%d")
  
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
  TimeStud <- dfActionCircuit %>% select(Alumno,Time) %>% group_by(Alumno) %>% 
    summarise(TotalTime=max(Time))
  TimeStud$TotalTime <-as.numeric(TimeStud$TotalTime)
  TimeStud$TotalTime <- round(TimeStud$TotalTime/60,digits = 2)

  #Parte Circuitos
  CircuTimebyStud <- dfActionCircuit %>% select(Alumno,Circuito) %>%  group_by(Alumno) %>% 
    summarise(NumCircu=length(Circuito))
  
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


FunctionTimeStudNorm <- function (dfVISIR_acciones,dfActionCircuit) {
  numAcciones<-nrow(dfVISIR_acciones)

  dfVISIR_accionesOrdenado <- dfVISIR_acciones[
    order(dfVISIR_acciones$Alumno,
          dfVISIR_acciones$FechaHoraEnvio),]
  
  #Parte Tiempo
  TimeStud <- dfActionCircuit %>% select(Alumno,Time) %>% group_by(Alumno) %>% 
    summarise(TotalTime=max(Time))
  TimeStud$TotalTime <-as.numeric(TimeStud$TotalTime)
  TimeStud$TotalTime <- round(TimeStud$TotalTime/60,digits = 2)

  #Parte Circuitos
  CircuTimebyStud <- dfActionCircuit %>% select(Alumno,CircuitoNormalizado) %>%  group_by(Alumno) %>% 
    summarise(NumCircu=length(unique(CircuitoNormalizado)))
  
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
  
  dfVISIR_accionesOrdenado$Dates <- as.Date(dfVISIR_accionesOrdenado$Dates,"%Y-%m-%d")
  dfVISIR_accionesOrdenado$ConnectionIni <- grepl("@@@initial::request@@@",dfVISIR_accionesOrdenado$DatosEnviadosXML)
  dfVISIR_accionesOrdenado$ConnectionFina <- grepl("@@@finish@@@",dfVISIR_accionesOrdenado$DatosEnviadosXML)
  dfVISIR_accionesOrdenado$Connection <-factor(ifelse(dfVISIR_accionesOrdenado$ConnectionIni,"Initial",
                                                      ifelse(dfVISIR_accionesOrdenado$ConnectionFina,"Final",
                                                             "NA")))
  X_P <- dfVISIR_accionesOrdenado%>% select(Alumno,Dates,FechaHoraEnvio,Connection)
  X_P$FechaHoraEnvio <- as.numeric(as.POSIXct(as.character(X_P$FechaHoraEnvio),
                                              format="%Y-%m-%d %H:%M:%S"))
  Totaltimebydate <- X_P %>% mutate(seqid = cumsum(Connection=="Initial")) %>%
    group_by(Alumno,Dates,seqid) %>% summarise(TimeSpent=sum(diff(FechaHoraEnvio))) %>% 
    group_by(Alumno,Dates) %>% summarise(TimeSpent=sum(TimeSpent)) %>%
    mutate(TimeSpent=TimeSpent/3600) %>% mutate(TimeSpent=round(TimeSpent,digits = 2))
  return(Totaltimebydate)
  
}

distnumcirc <- function(dfActionCircuit){
  print(str(dfActionCircuit))
  CircByStudentD <- dfActionCircuit %>% select(Alumno,Circuito,FechaHoraEnvio) %>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(Circuito =length(Circuito))
  return(CircByStudentD)
}

distnumcircN <- function(dfActionCircuit){
  CircByStudentDN <- dfActionCircuit %>% select(Alumno,CircuitoNormalizado) %>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% summarise(CircuitoNormalizado =length(unique(CircuitoNormalizado)))
  return(CircByStudentDN)
}

### >> GRAPHS ####
## Time vs Date ####
Dygraphfunc <- function(dfMTS) {
  if(is.null(dfMTS)) return(NULL)
  
  Totaltimebydate2 <-dfMTS %>% group_by(Dates)%>% 
    summarise(Time=sum(TimeSpent,na.rm=TRUE)) %>%mutate(Time=Time) %>%  mutate(Time=round(Time,digits = 2))
  Totaltime <- zoo(Totaltimebydate2$Time,Totaltimebydate2$Dates)
  Tot<-cbind(Totaltime)
  dygraph(Tot)%>% dyAxis("y",rangePad=c(-0.05),label = "Time (in h)") %>% dySeries("Totaltime", label = "Total Time",axis="y")  %>%
    dyOptions(axisLineWidth = 1.5,drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()
}

## Time on task vs User & Date ####
fplotlyfunc3 <- function(dfMTS) {
  if(is.null(dfMTS)) return(NULL)
  
  dfMTS$TimeSpent<-as.numeric(dfMTS$TimeSpent)
  dfMTS$Dates <- as.factor(dfMTS$Dates)
  dfMTS$Alumno <- as.factor(dfMTS$Alumno)
  names(dfMTS)[names(dfMTS) == 'Alumno'] <- 'User'
  names(dfMTS)[names(dfMTS) == 'TimeSpent'] <- 'Time'
  
  return(dfMTS)
}

## Heat map: Number of circuits vs USer & Date ####
fciruserdate <- function(dfActionCircuit) {
  if(is.null(dfActionCircuit)) return(NULL)
  
  dfActionCircuit$Dates <- format(as.Date(dfActionCircuit$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")
  Circuitsperuserdate <- dfActionCircuit %>% select(Alumno,Dates,Circuito) %>% group_by(Alumno,Dates) %>%
    summarise(Diffti=length(Circuito)) %>%  group_by(Dates,Alumno) %>% summarise(Circuits=sum(Diffti)) 
  Circuitsperuserdate$Circuits<-as.numeric(Circuitsperuserdate$Circuits)
  Circuitsperuserdate$Dates <- as.factor(Circuitsperuserdate$Dates)
  Circuitsperuserdate$Alumno <- as.factor(Circuitsperuserdate$Alumno)
  names(Circuitsperuserdate)[names(Circuitsperuserdate) == 'Alumno'] <- 'User'
  
  
  return(Circuitsperuserdate)
}

## Circuits Timeline vs User ####
timelineuser<- function(dfActionCircuit) {
  ggplot(dfActionCircuit, aes(x = Alumno, y = Time,color=Mesure)) + 
    geom_point(size = 1.5, alpha = 0.8) +
    theme_few()+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+scale_fill_manual(values=gra)
}

## Circuits vs Date ####
Dygraphfunc2 <- function(dfActionCircuit) {
  ADYygraph <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>% filter(complete.cases(.))
  ADYygraph$Dates <- format(as.Date(ADYygraph$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")
  ADYygraph$Dates <- as.Date(ADYygraph$Dates,"%Y-%m-%d")
  TotcircuDate <- ADYygraph %>% group_by(Dates) %>%  summarise(TotalCircuits=length(Circuito))
  TotNormcircuDate <- ADYygraph %>% group_by(Dates) %>%  summarise(TotalnormCircuits=length(unique(CircuitoNormalizado)))
  TotcircuDate$TotalnormCircuits <- TotNormcircuDate$TotalnormCircuits

  ses<- zoo(TotcircuDate$TotalCircuits,TotcircuDate$Dates)
  Totcircu<-cbind(ses)
  
  dygraph(Totcircu)%>% dyAxis("y",rangePad=c(-0.05),label = "Circuits") %>%  dySeries("ses", label = "Total Circuits",axis="y") %>% 
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>% dyLegend(width = 400)%>% dyRangeSelector()
}


plotDistribution <- function(values=NA,maxValues=NULL,xlabel="") {
  if(is.null(values)) return(NULL)
  
  if (is.null(maxValues)) {
    maxValues<-max(values,na.rm=TRUE)
    maxValues<-ceiling(maxValues/10^floor(log(maxValues)/log(10)-1))*10^floor(log(maxValues)/log(10)-1)
  }
  
  # Freedman-Diaconis rule
  bw <- maxValues / (2 * IQR(values) / length(values)^(1/3))
  
  bw2 <- floor(bw/10^floor(log(bw)/log(10)))
  bw2<-ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2<-bw2*10^floor(log(bw)/log(10))
  
  nbins <- ceiling(max(values)/bw2)
  nbins <- min(20, length(values)/4 , nbins)
  
  bw <- max(values) / nbins
  bw2 <- floor(bw/10^floor(log(bw)/log(10)))
  bw2<-ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2<-bw2*10^floor(log(bw)/log(10))
  
  bw2<-ifelse(nbins>=7,bw2,bw2/2+.Machine$double.eps)
  bw2 <- floor(bw2/10^floor(log(bw2)/log(10)))*10^floor(log(bw2)/log(10))
  
  ggmaxValues<-ceiling(maxValues/bw2)*bw2
  
  dense<-density(values,adjust=1,bw="SJ")
  df_density=data.frame(x=dense$x,y=dense$y/sum(dense$y)*bw2*
                          length(values)/mean(diff(dense$x)))
  theme_remove_all <- theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  
  ghist<-ggplot(data=NULL,aes(x=values)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue",
                   breaks=seq(0,ggmaxValues, by=bw2))+ 
    geom_area(data=df_density,aes(x=x,y=y),
              alpha=.2, fill="#FFFF00", color="#CC6600", size=1) + 
    geom_vline(aes(xintercept=mean(values)), linetype="dashed", size=1)+
    geom_rug(alpha=.5) +
    labs(x=xlabel, y="Frequency")+
    coord_cartesian(xlim= c(0, ggmaxValues)) +
    theme_bw() + theme_remove_all +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))
  
  ghist
}


### >> CALCULATIONS ####
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

InfovalueBoxNC <- function(dfActionCircuit){
  Acircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito)
  NumcircuV<-length(Acircu$Circuito)
  return(NumcircuV)
}

InfovalueBoxMNC <- function(dfActionCircuit){
  MeanNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito)%>% group_by(Alumno) %>% summarise(Numcirc=length(Circuito))
  MeanNumCircSV <- round(mean(MeanNAcircu$Numcirc),digits = 2)
  return(MeanNumCircSV)
}

InfovalueBoxlowbound <- function(dfActionCircuit){
  lowboundf <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito)%>%
    group_by(Alumno)%>%  summarise(Circuito =length(Circuito))
  mincircv <- min(lowboundf$Circuito)
  return(mincircv)
}

InfovalueBoxupbound <- function(dfActionCircuit){
  upboundf <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito)%>%
    group_by(Alumno)%>%  summarise(Circuito =length(Circuito))
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

InfovalueBoxTT <- function(dfOrderTime){
  tottime<-sum(dfOrderTime$TotalTime/60)
  tottime<-round(tottime,digits = 2)
  return(tottime)
}

InfovalueBoxMeT <- function(dfOrderTime){
  meantime <- round(mean(dfOrderTime$TotalTime/60),digits = 2)
  return(meantime)
}

InfovalueBoxMaxT <- function(dfOrderTime){
  maxtime<-round(max(dfOrderTime$TotalTime/60),digits = 2)
  return(maxtime)
}

InfovalueBoxMinT <- function(dfOrderTime){
  mintime<-round(min(dfOrderTime$TotalTime/60),digits = 2)
  return(mintime)
}


