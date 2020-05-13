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

replaceMany <- function(x, find, replace) {
  for(i in 1:length(find)) {
    x <- gsub(find[i], replace[i], x, fixed=T)
  }
  x
}

# source("dashboard/dashboard_functiVISIR.R")
#inFile <- list(datapath="data/an2014sm.csv")
inFile <- list(datapath="data/VISIR_logs/an2014.txt")

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
  if(length(nodos)>0 & length(nodos)<9) {
    # if(length(nodos)>0) {
    nodosUnif <- c(paste("P0",1:9,sep=""),paste("P",10:99,sep=""))
    nodosUnif <- nodosUnif[1:length(nodos)]
    matNodosUnif <- perm(nodosUnif)
    
    r_circuitos <- character(nrow(matNodosUnif))
    for(i in 1:nrow(matNodosUnif))
      r_circuitos[i] <- replaceMany(circuito, nodos, matNodosUnif[i,])
    circuito <- min(r_circuitos)
    
    # circuito <- replaceMany(circuito, nodos, nodosUnif)
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



#### Ampliació Francesc ####

## Valor Resolución
tempResolucion<-as.character(rep(NA,numCircuitos))
for(i in 1:numCircuitos){
  tempRegExpResolucion <-regexec("<dmm_resolution value=([^<]*)",
                                 as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
  tempResolucion[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpResolucion[[1]][2],
                            tempRegExpResolucion[[1]][2]+
                              attr(tempRegExpResolucion[[1]],"match.length")[2]-1)
}

tempResolucion <- gsub("[\n\">/]","", tempResolucion)


## Valor Resultado
tempResultado<-as.character(rep(NA,numCircuitos))
for(i in 1:numCircuitos){
  tempRegExpResultado <-regexec("<dmm_result value=([^<]*)",
                                 as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
  tempResultado[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpResultado[[1]][2],
                            tempRegExpResultado[[1]][2]+
                              attr(tempRegExpResultado[[1]],"match.length")[2]-1)
}

tempResultado <- gsub("[\n\">/]","", tempResultado)


## Valor Voltaje
tempVoltaje<-as.character(rep(NA,numCircuitos))
for(i in 1:numCircuitos){
  tempRegExpVoltaje <-regexec('dc_output channel="25V.*?<dc_voltage_actual value="([^"]*)',
                              as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
  tempVoltaje[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpVoltaje[[1]][2],
                           tempRegExpVoltaje[[1]][2]+
                             attr(tempRegExpVoltaje[[1]],"match.length")[2]-1)
}

## Adición de columnas en dfVISIR_accionesCircuito
dfVISIR_accionesCircuito<-cbind(dfVISIR_accionesCircuito,
                                Resultado = as.numeric(as.character(tempResultado)),
                                Resolucion = as.numeric(as.character(tempResolucion)),
                                Voltaje = as.numeric(as.character(tempVoltaje)))

dfVISIR_accionesCircuito$Relación_Resultado_Voltaje <- dfVISIR_accionesCircuito$Resultado / dfVISIR_accionesCircuito$Voltaje

## Vector con los 22 circuitos normalizados
circ_norm <- c("R_X DC_+25V DMM_AHI 1k/R_X DC_+25V DMM_ALO 1k/R_X DMM_ALO GND 10k/W_X DC_COM GND",
               "R_X DC_+25V DMM_VHI 10k/R_X DC_+25V DMM_VHI 1k/R_X DMM_VHI DMM_VLO 1k/W_X DC_COM GND/W_X DMM_VLO GND",
               "R_X DC_+25V DMM_VHI 1k/R_X DMM_VHI DMM_VLO 10k/R_X DMM_VHI DMM_VLO 10k/R_X DMM_VLO GND 1k/W_X DC_COM GND",
               "R_X DC_+25V DMM_VLO 10k/R_X DC_+25V DMM_VLO 1k/R_X DMM_VLO GND 1k/W_X DC_+25V DMM_VHI",
               "R_X DC_+25V DMM_VLO 10k/W_X DC_+25V DMM_VHI/W_X DMM_VLO GND",
               "R_X DC_+25V DMM_VLO 1k/R_X DC_+25V GND 1k/R_X DMM_VLO GND 10k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",
               "R_X DC_+25V GND 10k/R_X DC_+25V GND 10k/W_X DC_+25V DMM_VHI/W_X DMM_VLO GND",
               "R_X P01 DC_+25V 10k/R_X P01 DMM_ALO 1k/R_X DC_+25V DMM_ALO 1k/W_X DC_COM GND/W_X DMM_AHI GND",
               "R_X P01 DC_+25V 10k/R_X P02 DC_+25V 1k/R_X P01 GND 10k/R_X P02 GND 1k/W_X DC_+25V DMM_VHI/W_X DC_COM GND/W_X DMM_VLO GND",
               "R_X P01 DC_+25V 1k/R_X P01 DMM_VLO 10k/R_X P01 DMM_VLO 10k/R_X DMM_VLO GND 1k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",
               "R_X P01 DC_+25V 1k/R_X P01 GND 1k/R_X DC_+25V DMM_VLO 10k/R_X DMM_VLO GND 10k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",
               "R_X P01 DMM_ALO 10k/R_X P02 DMM_ALO 1k/R_X P01 GND 10k/R_X P02 GND 1k/W_X DC_+25V DMM_AHI/W_X DC_COM GND",
               "R_X P01 DMM_VHI 10k/R_X P01 DMM_VHI 10k/R_X P01 GND 1k/R_X DC_+25V DMM_VHI 1k/W_X DC_+25V DMM_VLO/W_X DC_COM GND",
               "R_X P01 DMM_VHI 10k/R_X P01 DMM_VLO 10k",
               "R_X P01 DMM_VHI 10k/R_X P02 DMM_VHI 1k/R_X P02 DMM_VLO 10k/R_X P01 DMM_VLO 1k",
               "R_X P01 DMM_VHI 1k/R_X P01 DMM_VHI 1k/R_X P02 DMM_VLO 10k/R_X P02 DMM_VLO 10k",
               "R_X P01 DMM_VLO 1k/R_X DMM_VHI DMM_VLO 1k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",
               "R_X P01 GND 1k/R_X DMM_ALO GND 10k/R_X DMM_ALO GND 1k/W_X DC_+25V DMM_AHI",
               "R_X P01 P02 10k/R_X P01 DMM_VLO 1k/R_X P02 GND 10k/R_X DMM_VHI DMM_VLO 1k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",
               "R_X P01 P02 10k/R_X P01 P02 10k/R_X P01 DC_+25V 1k/R_X P02 DMM_ALO 1k/W_X DC_COM GND/W_X DMM_AHI GND",
               "R_X P01 P02 1k/R_X P01 DMM_VHI 1k",
               "W_X DC_+25V DMM_VHI/W_X DC_COM GND/W_X DMM_VLO GND")


## Histograma de los 22 circuitos normalizados analizados
dfHISTO <- data.frame(Normalizado = dfVISIR_accionesCircuito$CircuitoNormalizado[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[1]],
                  Resultado = dfVISIR_accionesCircuito$Resultado[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[1]],
                  Voltaje = dfVISIR_accionesCircuito$Voltaje[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[1]],
                  Resolucion = dfVISIR_accionesCircuito$Resolucion[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[1]],
                  Medida = dfVISIR_accionesCircuito$Medida[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[1]])

for (i in 2:length(circ_norm)) {
  dfHISTO <- rbind(dfHISTO, data.frame(Normalizado = dfVISIR_accionesCircuito$CircuitoNormalizado[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[i]],
                               Resultado = (dfVISIR_accionesCircuito$Resultado[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[i]]),
                   Voltaje = dfVISIR_accionesCircuito$Voltaje[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[i]],
                   Resolucion = dfVISIR_accionesCircuito$Resolucion[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[i]],
                   Medida = dfVISIR_accionesCircuito$Medida[dfVISIR_accionesCircuito$CircuitoNormalizado==circ_norm[i]]))
}
  

#table(dfVISIR_accionesCircuito$CircuitoNormalizado[dfVISIR_accionesCircuito$CircuitoNormalizado %in% circ_norm])
#sort(unique(dfVISIR_accionesCircuito$CircuitoNormalizado))[1001:2000]

dfHISTO$Relacion_Resultado_Voltaje <- dfHISTO$Resultado / dfHISTO$Voltaje
dfHISTO <- dfHISTO[!is.na(dfHISTO$Normalizado),]
dfHISTO_R <- dfHISTO[dfHISTO$Medida=="resis",]
dfHISTO_A <- dfHISTO[dfHISTO$Medida=="dc cu",]
dfHISTO_V <- dfHISTO[dfHISTO$Medida=="dc vo",]

nombres <- c("Circuit 1.1","Circuit 1.2","Circuit 1.3","Circuit 1.4","Circuit 1.5","Circuit 1.6",
             "Circuit 1.7","Circuit 1.8","Circuit 1.9","Circuit 1.10","Circuit 1.11","Circuit 1.12","Circuit 1.13",
             "Circuit 1.14","Circuit 1.15","Circuit 1.16","Circuit 1.17","Circuit 1.18","Circuit 1.19","Circuit 1.20",
             "Circuit 1.21","Circuit 1.22")
names(nombres) <- circ_norm


# Histogramas Resultado y Relación resultado-voltaje
ggplot(dfHISTO, aes(Resultado)) + labs(x = "Resultat", y = "Frequència") + theme(legend.position="none") +
  geom_histogram(fill="#0099CC") + facet_wrap(~ Normalizado, ncol=4, scales = ("free"), labeller=labeller(Normalizado = nombres)) +
  theme_bw() + theme(legend.position='none')

ggplot(dfHISTO, aes(Relacion_Resultado_Voltaje)) +  labs(x = "Resultat", y = "Frequència") +
  geom_histogram(fill="#0099CC") + facet_wrap(~ Normalizado, ncol=4, scales = ("free"), labeller=labeller(Normalizado = nombres)) +
  theme_bw() + theme(legend.position='none')



## Adición del fragmento más significativo (función dentro del archivo "Fragment mes significatiu.R")
for (i in 1:nrow(dfVISIR_accionesCircuito)){
  dfVISIR_accionesCircuito$CircuitoSignificativo[i] <- fragmentoSignificativo(as.character(dfVISIR_accionesCircuito$CircuitoNormalizado[i]))
}


## Data frame para milestones ##
df_milestone <- data.frame(CircuitoNormalizado=dfVISIR_accionesCircuito$CircuitoNormalizado,
                           CircuitoSignificativo=dfVISIR_accionesCircuito$CircuitoSignificativo,
                           Resultado=dfVISIR_accionesCircuito$Resultado,
                           Voltaje=dfVISIR_accionesCircuito$Voltaje,
                           Circuito=dfVISIR_accionesCircuito$Circuito,
                           Medida=dfVISIR_accionesCircuito$Medida)

df_milestone$XML <- paste("<circuito>", df_milestone$Circuito,"</circuito>",
                          "<circuitoNormalizado>", df_milestone$CircuitoNormalizado, "</circuitoNormalizado>",
                          "<circuitoSignificativo>", df_milestone$CircuitoSignificativo, "</circuitoSignificativo>",
                          "<resultado>",df_milestone$Resultado,"</resultado",
                          "<voltaje>", df_milestone$Voltaje, "</voltaje>",
                          "<medida>", df_milestone$Medida, "</medida>", sep="")

milestone <- paste("<circuito>", df_milestone$Circuito,"</circuito>",
                   "<circuitoNormalizado>", df_milestone$CircuitoNormalizado, "</circuitoNormalizado>",
                   "<circuitoSignificativo>", df_milestone$CircuitoSignificativo, "</circuitoSignificativo>",
                   "<resultado>",df_milestone$Resultado,"</resultado",
                   "<voltaje>", df_milestone$Voltaje, "</voltaje>",
                   "<medida>", df_milestone$Medida, "</medida>", sep="")

write.csv(milestone, file="milestone.csv")



## Duda para enviar a Javier (DEUSTO) ##
# aaa <- dfHISTO[dfHISTO$Normalizado=="R_X DC_+25V DMM_VLO 1k/R_X DC_+25V GND 1k/R_X DMM_VLO GND 10k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",]

df_2 <- dfVISIR_accionesCircuito[dfVISIR_accionesCircuito$CircuitoNormalizado=="R_X DC_+25V DMM_VLO 1k/R_X DC_+25V GND 1k/R_X DMM_VLO GND 10k/W_X DC_+25V DMM_VHI/W_X DC_COM GND",]
df_Circ6 <- data.frame(Circuito=df_2$Circuito, CircuitoNormalizado=df_2$CircuitoNormalizado, Resultado=df_2$Resultado,
                       Voltaje=df_2$Voltaje, Resolución=df_2$Resolucion)
df_Circ6$Relación_Resulrado_Voltaje <- df_Circ6$Resultado / df_Circ6$Voltaje
df_Circ6 <- df_Circ6[!is.na(df_Circ6$Circuito),]

write.csv(df_Circ6, file="Circ_6.csv")

