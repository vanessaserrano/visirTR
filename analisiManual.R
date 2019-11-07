options(install.packages.check.source = "no")

pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","DT","gplots",
         "xts","dygraphs","scales",
         "formattable","treemap","viridis","cat")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

# source("dashboard/dashboard_functiVISIR.R")
inFile <- list(datapath="data/an2014sm.csv")

df <- read.table(inFile$datapath, header=F, sep=",", quote="\"",
                 stringsAsFactors = FALSE)
colnames(df) <- c("Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
                  "DatosEnviadosXML","DatosRecibidosXML","Tarea")
df$Dates <- format(as.Date(df$FechaHoraEnvio, format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d")

# dfActionCircuit<- funActionCircuit(df)
# funActionCircuit <- function (dfVISIR_acciones, timeLimit = 900) {
  
dfVISIR_acciones <- df
timeLimit <- 900

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


##### NORMALITZACIÓ ####

x <- dfVISIR_accionesCircuito$Circuito[grepl("25V",dfVISIR_accionesCircuito$Circuito)][1:100]
x <- "W_X DMM_VHI A12/W_X DMM_VLO A20/W_X DC_+25V A12/W_X A20 0/R_X A12 A16 10k/R_X A16 A20 10k"
x <- "W_X DC_+25V A58/W_X DC_COM 0/W_X 0 A17/R_X A13 A17 1k/R_X A58 A22 10k/R_X A58 A22 10k"


normr<-character()
normr[1] <- x

if(is.null(x)) return(NA)
if(is.na(x)) return(NA)
if(x=="") return(NA)

circuito <- as.character(x)

circuito <- gsub("([^A-Z0-9])0([^A-Z0-9])", "\\1GND\\2", circuito)
circuito <- gsub("([^A-Z0-9])0$", "\\1GND", circuito)
normr[2] <- circuito

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
normr[3] <- circuito


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
normr[4] <- circuito

# if(is.na(circuito)) return(NA)
# if(circuito=="") return(NA)

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
normr[5] <- circuito

# unificar nodos
nodos <- gregexpr("A[0-9][0-9]",circuito)[[1]]
if(nodos[[1]]==-1) {
  nodos <- character(0)
} else {
  nodos <- sapply(nodos, function(x) {substr(circuito,x,x+2)})
  nodos <- unique(nodos)
}

if(length(nodos)>0 & length(nodos)<9) {
  nodosUnif <- c(paste("P0",1:9,sep=""),paste("P",10:99,sep=""))
  nodosUnif <- nodosUnif[1:length(nodos)]
  matNodosUnif <- perm(nodosUnif)
  
  r_circuitos <- character(nrow(matNodosUnif))
  for(i in 1:nrow(matNodosUnif))
    r_circuitos[i] <- replaceMany(circuito, nodos, matNodosUnif[i,])
  #print(r_circuitos)
  circuito <- min(r_circuitos)
}
normr[6] <- circuito

#################################

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
  circuito <- gsub("([^A-Z0-9_])X([^A-Z0-9_])", "\\1A90\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])X$", "\\1A90", circuito)
  circuito <- gsub("([^A-Z0-9_])Y([^A-Z0-9_])", "\\1A91\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])Y$", "\\1A91", circuito)
  circuito <- gsub("([^A-Z0-9_])S([^A-Z0-9_])", "\\1A92\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])S$", "\\1A92", circuito)
  circuito <- gsub("([^A-Z0-9_])T([^A-Z0-9_])", "\\1A93\\2", circuito)
  circuito <- gsub("([^A-Z0-9_])T$", "\\1A93", circuito)
  
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
  if(nodos[[1]]==-1) {
    nodos <- character(0)
  } else {
    nodos <- sapply(nodos, function(x) {substr(circuito,x,x+2)})
    nodos <- unique(nodos)
  }
  if(length(nodos)>0 & length(nodos)<9) {
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
    # print(i)
  }
  return(y)
}


tempCircuitoNormalizado <- normalizarCircuitos(dfVISIR_accionesCircuito$Circuito)

########################

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

dfVISIR_accionesCircuito


