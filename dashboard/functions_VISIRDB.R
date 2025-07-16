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
  if(x=="") return("")
    
  ## Substitución del 0 por GND
  circuito <- as.character(x)
  circuito <- gsub("([^A-Z0-9])0([^A-Z0-9])", "\\1GND\\2", circuito)
  circuito <- gsub("([^A-Z0-9])0$", "\\1GND", circuito)
  circuito <- gsub("([^A-Z0-9])0[/]", "\\1GND/", circuito)
  
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
    
    if(conectores[1] == "W_X" & conectores[2] == conectores[3]) {
      componentes[i] <- ""
    } else {
      if(conectores[1] == "W_X" & substr(conectores[2],1,1) =="A") {
        componentes <- gsub(conectores[2], conectores[3], componentes, fixed=TRUE)
        componentes[i] <- ""
      } else {
        componentes[i] <- paste(conectores, collapse=" ")
      }
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
    
    if(conectores[2] > conectores[3] & 
       conectores[1] %in% c("W_X", "R_X", "L_X", "C_X")) {
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
  }
  return(circuito)
}


normalizarCircuitos<-function(x) {
  
  # y <- character(length(x))
  # for (i in 1:length(x)) {
  #   y[i]<-normalizarCircuito(x[i])
  # }
  # return(y)
  # nCores <- detectCores() %/% 2
  
  nCores <- detectCores()-1
  # coresSel <- rep(1:nCores,each=ceiling(length(x)/nCores))
  
  cl <- makeCluster(nCores)
  clusterExport(cl, list("perm","replaceMany","normalizarCircuito"))
  y <- parSapply(cl, x, function(x) sapply(x,normalizarCircuito),
                 chunk.size = ceiling(length(x)/nCores))
  stopCluster(cl)
  y
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

simplificarCircuito <- function(circuito){
  if (is.na(circuito)) return(NA)
  if (circuito=="") return("")
  
  circuito_prin <- NA
  componentes <- strsplit(circuito,"/",fixed=TRUE)[[1]]
  conectores <- strsplit(componentes, " ")
  df <- data.frame(componentes)
  
  for (i in 1:nrow(df)) {
    df[i,2:4] <- unlist(strsplit(df[i,1],split="[ ]"))[1:3]
  }

  df <- data.frame(V3=c(as.character(df$V3),as.character(df$V4)),
                   V4=c(as.character(df$V4),as.character(df$V3)),
                   stringsAsFactors = F)

  # el circuito debe tener los dos cables del multimetro, 
  # sino devuelve cadena vacía
  valid <- (grepl("DMM_VLO", circuito) & grepl("DMM_VHI", circuito)) |
    (grepl("DMM_ALO", circuito) & grepl("DMM_AHI", circuito)) | 
    (grepl("DMM_1_1", circuito) & grepl("DMM_1_2", circuito)) | 
    (grepl("IPROBE_1_1", circuito) & grepl("IPROBE_1_2", circuito)) |
    (grepl("DMM_2_1", circuito) & grepl("DMM_2_2", circuito)) | 
    (grepl("IPROBE_2_1", circuito) & grepl("IPROBE_2_2", circuito))
  if (!valid) return("")
  
  nodesf1 <- NA          # nodo inicial del fragmento principal
  if (grepl("DMM_VLO", circuito) & grepl("DMM_VHI", circuito)) nodesf1 <- "DMM_VLO"
  if (grepl("DMM_ALO", circuito) & grepl("DMM_AHI", circuito)) nodesf1 <- "DMM_ALO"
  if (grepl("DMM_1_1", circuito) & grepl("DMM_1_2", circuito)) nodesf1 <- "DMM_1_1"
  if (grepl("DMM_2_1", circuito) & grepl("DMM_2_2", circuito)) nodesf1 <- "DMM_2_1"
  if (grepl("IPROBE_1_1", circuito) & grepl("IPROBE_1_2", circuito)) nodesf1 <- "IPROBE_1_1"
  if (grepl("IPROBE_2_1", circuito) & grepl("IPROBE_2_2", circuito)) nodesf1 <- "IPROBE_2_1"
  
  if(is.na(nodesf1)) return("")
  
  internal <- data.frame(
    V3=c("DMM_VHI","DMM_AHI","DMM_1_1","DMM_2_1","IPROBE_1_1","IPROBE_2_1"),
    V4=c("DMM_VLO","DMM_ALO","DMM_1_2","DMM_2_2","IPROBE_1_2","IPROBE_2_2"),
    stringsAsFactors = FALSE)
  internal <- rbind(internal, 
                    data.frame(V3=internal$V4,
                               V4=internal$V3,
                               stringsAsFactors = FALSE))
  df <- rbind(df,internal)
  
  nodesf1anterior <- character(0)       # nodos fragmento principal paso anterior
  
  a <- 1
  while (length(nodesf1)!=length(nodesf1anterior)) {
    nodesf1anterior <- nodesf1
    for (i in 1:nrow(df)) {
      if (df[i,1] %in% nodesf1) {
        nodesf1 <- unique(c(nodesf1, df[i,2]))
      }
    }
  }
  
  y <- 0
  
  for (i in 1:length(componentes)){
    if (sum(df[i,1] %in% nodesf1)==0) {
      componentes <- componentes[-(i-y)]
      y <- y + 1
    }
  }
  
  circuito_prin <- paste(componentes, collapse = "/")
  
  # eliminar componentes cortocircuitados
  componentes <- strsplit(circuito_prin,"/",fixed=TRUE)[[1]]
  if(is.null(componentes)) return(NA)
  if(length(componentes)==0) return(NA)
  
  for(i in 1:length(componentes)) {
    if (!is.na(componentes[i]) & componentes[i]!="") {  
      conectores <- strsplit(componentes[i], " ")[[1]]
      if(is.na(conectores[2]==conectores[3]) | conectores[2]==conectores[3]) {
        componentes[i] <- ""
      }
    }
  }
  componentes <- componentes[order(gsub("P[0-9][0-9]","Pxx",componentes))]
  circuito_prin <- paste(componentes, collapse="/")  
  circuito_prin <- gsub("^/*","",circuito_prin)
  
  circuito_prin <- gsub("P0([0-9])","A\\1",circuito_prin)
  circuito_prin <- gsub("P([1-9][0-9])","A\\1",circuito_prin)
  circuito_prin <- normalizarCircuito(circuito_prin)
  
  return(circuito_prin)
}


#### PUBLIC FUNCTIONS ####
### >> DATASETS ####
import_logFile <- function(inFile,sessionID) {
  if (is.null(inFile)) return(NULL)
  print(paste("pre-Log-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  # status$init <- T
  logFileName_ext <<- inFile$name  
  filePath <- inFile$datapath
  
  if(grepl("[.]csv$",inFile$name, ignore.case = T)) {
    lines <- vroom_lines(filePath, n_max = 20)
    clines <- grepl(",",lines)
    linesToSkip <- min(which(clines)) - 1
    
    df <- vroom(filePath, col_names=F, delim=",", skip=linesToSkip,
                quote="\"", n_max=1000)
    
    if (length(unique(pull(df[,1]))) == 1) {
      df <- vroom(filePath, delim=",", quote="\"", na=c("NA"), show_col_types = F,
                  col_names=c("TBR","Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
                              "DatosEnviadosXML","DatosRecibidosXML"),
                  col_types="ccccccc", escape_double = T, escape_backslash = F,
                  skip = linesToSkip)
      df <- df[,-1]
      df$Tarea <- NA
    } else {
      if (ncol(df) == 7) {
        df <- vroom(filePath, delim=",", quote="\"", na=c("NA"), show_col_types = F,
                  col_names=c("Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
                              "DatosEnviadosXML","DatosRecibidosXML","Tarea"),
                  col_types="ccccccc", escape_double = T, escape_backslash = F,
                  skip = linesToSkip)
      } else {
        df <- vroom(filePath, delim=",", quote="\"", na=c("NA"), show_col_types = F,
                  col_names=c("TBR", "Alumno","Sesion","FechaHoraEnvio","FechaHoraRespuesta",
                              "DatosEnviadosXML","DatosRecibidosXML","Tarea"),
                  col_types="cccccccc", escape_double = T, escape_backslash = F,
                  skip = linesToSkip)
        df <- df[,-1]
      }
    }
  }
  if(grepl("[.]json$",inFile$name, ignore.case = T)) {
    lines <- vroom_lines(filePath)
    lines_ext <<- lines
    
    alumnos <- regexpr('"user_id": ".*?"',lines)
    alumnos <- substr(lines, alumnos + 12, alumnos + attr(alumnos, "match.length") - 2) 
    
    sesiones <- regexpr('"session_id": ".*?"',lines)
    sesiones <- substr(lines, sesiones + 15, sesiones + attr(sesiones, "match.length") - 2) 
    
    horasE <- regexpr('"request_start_time": ".*?"',lines)
    horasE <- substr(lines, horasE + 23, horasE + attr(horasE, "match.length") - 2) 
    
    horasR <- regexpr('"request_end_time": ".*?"',lines)
    horasR <- substr(lines, horasR + 21, horasR + attr(horasR, "match.length") - 2) 
    
    # We assume that HIVE json logs only include experiments
    datosE_circ <- regexpr('"req": ".*?"',lines)
    datosE_circ <- substr(lines, datosE_circ + 8, datosE_circ +
                            attr(datosE_circ, "match.length") - 4)
    datosE_circ <- gsub("\\n", "\n", datosE_circ, fixed=T)
    
    datosE_mm <- grepl('"dmm":',lines)
    datosE_mm_mode <- regexpr('"mode": ".*?"',lines)
    datosE_mm_mode <- substr(lines, datosE_mm_mode + 9, datosE_mm_mode +
                               attr(datosE_mm_mode, "match.length") - 2) 
    
    # Multimeter resolution is set to 3.5 as fixed default
    datosE_mm <- ifelse(datosE_mm,
                        paste0("<multimeter id=\"1\"><dmm_function value=\"",
                               datosE_mm_mode,
                               "\"></dmm_function><dmm_resolution value=\"3.5\"></dmm_resolution></multimeter>"),
                        "")
    
    datosE <- paste0("<protocol version=\"internal\"><request><circuit><circuitlist>",
                     datosE_circ,
                     "</circuitlist></circuit>",
                     datosE_mm,
                     "</request></protocol>")
    
    
    datosR_result <- regexpr('"dmm_1": [{]"result": [-e.0-9]*',lines)
    datosR_result <- substr(lines, datosR_result + 20, datosR_result +
                            attr(datosR_result, "match.length")-1)
    
    # Multimeter resolution is set to 3.5 as fixed default
    # Power source voltage is not available in HIVE (TO BE REMEMBERED
    # WHEN CREATING MILESTONES)
    datosR <- paste0("<protocol version=\"internal\"><response><multimeter id=\"1\">",
                     "<dmm_function value=\"",
                     datosE_mm_mode,
                     "\"/><dmm_resolution value=\"3.5\"/>",
                     "<dmm_result value=\"",
                     datosR_result,
                     "\"/></multimeter>",
                     "<dcpower><dc_outputs></dc_outputs></dcpower>",
                     "</response></protocol>")
    
    df <- data.frame(
      Alumno=alumnos,
      Sesion=sesiones,
      FechaHoraEnvio=horasE,
      FechaHoraRespuesta=horasR,
      DatosEnviadosXML=datosE,
      DatosRecibidosXML=datosR,
      Tarea="hive"
    )
  }
  
  # status <- paste0("done-", format(Sys.time(),"%Y%m%d%H%M%S"))
  # save(status, file=paste0(sessionID,"_loa.rda"))  
  print(paste("post-Log-",format(Sys.time(),"%Y%m%d%H%M%S")))
  (dfLoadLog_ext <<- df)
}

create_dfActions <- function(df,sessionID) {
  if (is.null(df)) return(NULL)
  print(paste("pre-Act-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  df$FechaHoraEnvio <- gsub("T"," ",df$FechaHoraEnvio)
  df$FechaHoraRespuesta <- gsub("T"," ",df$FechaHoraRespuesta)
  
  df$Dates <- format(as.Date(df$FechaHoraEnvio,
                             format="%Y-%m-%d %H:%M:%S"),
                     "%Y-%m-%d")

  df$DatosEnviadosXML <- gsub("\n","", df$DatosEnviadosXML, fixed=T)
  df$DatosRecibidosXML <- gsub("\n","", df$DatosRecibidosXML, fixed=T)
  
  df <- df[!duplicated(paste(df$Alumno,
                             df$Sesion,
                             df$FechaHoraEnvio,
                             df$DatosEnviadosXML,
                             df$DatosRecibidosXML)),]
  # filtering for DC
  prefix <- "<request[^>]*>.*<circuit>.*"
  sel <- !grepl(paste0(prefix,"VFGENA_1_1"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"D_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"C_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"CE_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"L_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"OP_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix,"POT_X"),df$DatosEnviadosXML) &
    !grepl(paste0(prefix," PROBE"),df$DatosEnviadosXML)
  df <- df[sel,]
  
  # status <- paste0("done-", format(Sys.time(),"%Y%m%d%H%M%S"))
  # save(status, file=paste0(sessionID,"_imp.rda"))  
  print(paste("post-Act-",format(Sys.time(),"%Y%m%d%H%M%S")))
  (dfImport_ext <<- df)
}

create_dfActionTime <- function (dfVISIR_acciones, timeLimit = 900) {
  if(is.null(dfVISIR_acciones)) return(NULL)
  print(paste("pre-Time-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
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
  # Sys.sleep(1)  
  print(paste("mid-Time-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  dfVISIR_accionesOrdenado$TiempoDesdeAccionAnterior <- vecDiffTime / 60
  dfVISIR_accionesOrdenado$TiempoAcumulado <- vecDiffTimeCum / 60
  dfVISIR_accionesOrdenado$TiempoAcumuladoCorregido <- vecDiffTimeCumReal / 60
  
  ordenAlumnos <- dfVISIR_accionesOrdenado %>% group_by(Alumno) %>% 
    summarise(TiempoAcumuladoCorregidoMax = max(TiempoAcumuladoCorregido)) %>%
    arrange(TiempoAcumuladoCorregidoMax)
  
  vecTiempoMax <- ordenAlumnos$TiempoAcumuladoCorregidoMax
  
  dfVISIR_accionesOrdenado$Alumno <- factor(dfVISIR_accionesOrdenado$Alumno, 
                                            levels=ordenAlumnos$Alumno, ordered=TRUE)

  print(paste("post-Time-",format(Sys.time(),"%Y%m%d%H%M%S")))
  (dfActionTime_ext <<- dfVISIR_accionesOrdenado)
}

create_dfActionCircuit <- function (dfActionTime) {
  if(is.null(dfActionTime)) return(NULL)
  print(paste("pre-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  dfVISIR_accionesOrdenado <- dfActionTime
  numAcciones<-nrow(dfVISIR_accionesOrdenado)
    
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
  print(paste("mid1-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  dfVISIR_accionesCircuito <- dfVISIR_accionesOrdenado[dfVISIR_accionesOrdenado$EsCircuito,] 
  
  tempCircuitos<-as.character(rep(NA,numCircuitos))
  for(i in 1:numCircuitos){
    #El primer resultat de regexec es el match global, el 2:n son els extrets
    tempRegExpCircuito<-regexec("<circuitlist>([^<]*)</circuitlist>",as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]))
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
      tempMedidas[i]<-substr(as.character(dfVISIR_accionesCircuito$DatosEnviadosXML[i]),
                             tempRegExpMedida[[1]][2],tempRegExpMedida[[1]][2]+4)
    }
  }
  
  tempMedidas[tempMedidas=="off\">"] <- ""
  
  dfVISIR_accionesCircuito<-cbind(dfVISIR_accionesCircuito,
                                  Circuito = tempCircuitos,
                                  MultimetroConectado=tempMMConectado,
                                  MultimetroEncendido=tempMMOperativo,
                                  EsMedida = tempHayMedida,
                                  Medida = tempMedidas)
  
  # Para version HTML5
  dfVISIR_accionesCircuito$Circuito<-gsub("DMM_1 DMM_1_1 DMM_1_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("DMM_2 DMM_2_1 DMM_2_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDC[-+][0-9]*V_[0-9] VDC[-+][0-9]*V_[0-9]_[0-9]","",dfVISIR_accionesCircuito$Circuito)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC+6V_1 VDC+6V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC+6V_2 VDC+6V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC+25V_1 VDC+25V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC+25V_2 VDC+25V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("IPROBE_1 IPROBE_1_1 IPROBE_1_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("IPROBE_2 IPROBE_2_1 IPROBE_2_2","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDCCOM_1 VDCCOM_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("VDCCOM_2 VDCCOM_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC-25V_1 VDC-25V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC-25V_2 VDC-25V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC-6V_1 VDC-6V_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  #dfVISIR_accionesCircuito$Circuito<-gsub("VDC-6V_2 VDC-6V_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("PROBE1_1 PROBE1_1_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("PROBE1_2 PROBE1_2_1","",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  
  dfVISIR_accionesCircuito$Circuito<-gsub("W_X","/W_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("R_X","/R_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("D_X","/D_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("C_X","/C_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("OP_X","/OP_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("L_X","/L_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  dfVISIR_accionesCircuito$Circuito<-gsub("POT_X","/POT_X",dfVISIR_accionesCircuito$Circuito,fixed=TRUE)
  
  dfVISIR_accionesCircuito$Circuito<-substring(dfVISIR_accionesCircuito$Circuito,2)

  print(paste("mid2-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  tempCircuitoNormalizado <- normalizarCircuitos(dfVISIR_accionesCircuito$Circuito)

  dfVISIR_accionesCircuito$CircuitoNormalizado <- tempCircuitoNormalizado

  vecCircuitoCerrado <- sapply(dfVISIR_accionesCircuito$Circuito, esCircuitoCerrado)
  dfVISIR_accionesCircuito$EsCircuitoCerrado <- vecCircuitoCerrado

  print(paste("mid3-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  vecMMMal <- grepl("(?:DMM_V.*DMM_A)|(?:DMM_A.*DMM_V)",
                    dfVISIR_accionesCircuito$CircuitoNormalizado)  
  vecMMOKV <- !vecMMMal & grepl("(?:DMM_V.*DMM_V)|(?:DMM_1_.*DMM_1_)|(?:DMM_2_.*DMM_2_)",
                                dfVISIR_accionesCircuito$CircuitoNormalizado) 
  vecMMOKA <- !vecMMMal & grepl("(?:DMM_A.*DMM_A)|(?:IPROBE_1_.*IPROBE_1_)|(?:IPROBE_2_.*IPROBE_2_)",
                                dfVISIR_accionesCircuito$CircuitoNormalizado) 
  dfVISIR_accionesCircuito$MultimetroMal <- !vecMMOKV & !vecMMOKA  #No hay multimetro o estÃ¡ mal conectado
  dfVISIR_accionesCircuito$MultimetroV <- vecMMOKV
  dfVISIR_accionesCircuito$MultimetroA <- vecMMOKA
  dfVISIR_accionesCircuito$MedidaCorrectaV <- dfVISIR_accionesCircuito$MultimetroV & 
    grepl("dc v",dfVISIR_accionesCircuito$Medida)
  
  dfVISIR_accionesCircuito$MedidaCorrectaA <- dfVISIR_accionesCircuito$MultimetroA & 
    grepl("dc c",dfVISIR_accionesCircuito$Medida)
  
  # Consider removing check for power source here
  dfVISIR_accionesCircuito$MedidaCorrectaR <- dfVISIR_accionesCircuito$MultimetroV & 
    grepl("resi",dfVISIR_accionesCircuito$Medida) & 
    !grepl("DC_+",dfVISIR_accionesCircuito$DatosEnviadosXML)
  
  Medida <- factor(ifelse(dfVISIR_accionesCircuito$MedidaCorrectaV,"Voltage",
                          ifelse(dfVISIR_accionesCircuito$MedidaCorrectaR,"Resistance",
                                 ifelse(dfVISIR_accionesCircuito$MedidaCorrectaA,"Current",
                                        "Error"))),ordered=TRUE,levels=c("Current", "Resistance","Voltage","Error"))
  
  dfVISIR_accionesCircuito$Measure <- as.factor(Medida)
  names(dfVISIR_accionesCircuito)[names(dfVISIR_accionesCircuito) == 'TiempoAcumuladoCorregido'] <- 'Time'
  
  names(dfVISIR_accionesOrdenado)[names(dfVISIR_accionesOrdenado) == 'TiempoAcumuladoCorregido'] <- 'Time'
  
  # Valor Resolución
  tempResolucion<-as.character(rep(NA,numCircuitos))
  for(i in 1:numCircuitos){
    tempRegExpResolucion <-regexec("<dmm_resolution value=([^<]*)",
                                   as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
    tempResolucion[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpResolucion[[1]][2],
                              tempRegExpResolucion[[1]][2]+
                                attr(tempRegExpResolucion[[1]],"match.length")[2]-1)
  }
  
  tempResolucion <- gsub("[\n\">/]","", tempResolucion)
  
  # Valor Resultado
  tempResultado<-as.character(rep(NA,numCircuitos))
  for(i in 1:numCircuitos){
    tempRegExpResultado <-regexec("<dmm_result value=([^<]*)",
                                  as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
    tempResultado[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpResultado[[1]][2],
                             tempRegExpResultado[[1]][2]+
                               attr(tempRegExpResultado[[1]],"match.length")[2]-1)
  }
  
  tempResultado <- gsub("[\n\">/]","", tempResultado)
  
  # Valor Voltaje
  tempVoltaje<-as.character(rep(NA,numCircuitos))
  for(i in 1:numCircuitos){
    tempRegExpVoltaje <-regexec('dc_output channel="25V.*?<dc_voltage_actual value="([^"]*)',
                                as.character(dfVISIR_accionesCircuito$DatosRecibidosXML[i]))
    tempVoltaje[i]<-substr(dfVISIR_accionesCircuito$DatosRecibidosXML[i],tempRegExpVoltaje[[1]][2],
                           tempRegExpVoltaje[[1]][2]+
                             attr(tempRegExpVoltaje[[1]],"match.length")[2]-1)
  }
  
  # Adición de columnas en dfVISIR_accionesCircuito
  dfVISIR_accionesCircuito<-cbind(dfVISIR_accionesCircuito,
                                  Resultado = as.numeric(as.character(tempResultado)),
                                  Resolucion = as.numeric(as.character(tempResolucion)),
                                  Voltaje = as.numeric(as.character(tempVoltaje)))
  
  # Cálculo Relación
  dfVISIR_accionesCircuito$RatioResVol <- dfVISIR_accionesCircuito$Resultado / dfVISIR_accionesCircuito$Voltaje
  
  print(paste("mid4-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  # Circuito Significativo (Simplificado)
  for (i in 1:nrow(dfVISIR_accionesCircuito)){
    dfVISIR_accionesCircuito$CircuitoSimplificado[i] <- simplificarCircuito(as.character(dfVISIR_accionesCircuito$CircuitoNormalizado[i]))
  }
  
  # Datos XML para ser analizados con milestones
  dfVISIR_accionesCircuito$XML <- paste("<circuit>", dfVISIR_accionesCircuito$Circuito,"</circuit>",
                            "<normalizedCircuit>", dfVISIR_accionesCircuito$CircuitoNormalizado, "</normalizedCircuit>",
                            "<simplifiedCircuit>", dfVISIR_accionesCircuito$CircuitoSimplificado, "</simplifiedCircuit>",
                            "<result>",dfVISIR_accionesCircuito$Resultado,"</result>",
                            "<voltage>", dfVISIR_accionesCircuito$Voltaje, "</voltage>",
                            "<measure>", dfVISIR_accionesCircuito$Medida, "</measure>", sep="")
  
  print(paste("post-AC-",format(Sys.time(),"%Y%m%d%H%M%S")))
  (dfActionCircuit_ext <<- dfVISIR_accionesCircuito)
}

aggreg_dfActionCircuit_xStud <- function (dfActions, dfActionCircuit) {
  if(is.null(dfActions) || is.null(dfActionCircuit)) return(NULL)
  print(paste("pre-ACxStud-",format(Sys.time(),"%Y%m%d%H%M%S")))
  zz <- dfActions %>% group_by(Alumno) %>% 
    summarise(TotalTime=max(TiempoAcumuladoCorregido),.groups="drop") %>% 
    mutate(TotalTime=round(TotalTime/60,digits = 2))    #in hours
  
  zz1 <- dfActionCircuit %>% group_by(Alumno) %>% 
    summarise(NumCircu=length(Circuito),
              NumExp = length(Circuito),
              NumCCircu=length(unique(Circuito)),
              NumNCircu=length(unique(CircuitoNormalizado)),
              NumSCircu=length(unique(CircuitoSimplificado)),
              NumMesE=sum(Measure=="Error"),
              NumMesC=sum(Measure=="Current"),
              NumMesV=sum(Measure=="Voltage"),
              NumMesR=sum(Measure=="Resistance"),
                                     .groups = "drop")
  zz <- merge(zz,zz1,all=TRUE)
  zz[is.na(zz)] <- 0
  print(paste("post-ACxStud-",format(Sys.time(),"%Y%m%d%H%M%S")))
  (sumxStud_ext <<- zz)
}

aggreg_dfActionCircuit_xStudDate <- function (dfAccionesTiempo, dfCircuitos, sessionID) {
  if(is.null(dfAccionesTiempo) || is.null(dfCircuitos)) return(NULL)

  print(paste("pre-ACxStudDate-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  sumxStudDate <- dfAccionesTiempo %>%  group_by(Alumno,Dates) %>%
    arrange(Alumno,Dates) %>%
    summarise(TimeToDate=max(TiempoAcumuladoCorregido),.groups="drop") %>%
    mutate(TimeInDate = c(0,diff(TimeToDate))) %>%
    mutate(TimeInDate = ifelse(c(F,Alumno[-1]==Alumno[-length(Alumno)]),TimeInDate,TimeToDate)) %>% 
    mutate(TimeSpent = round(TimeInDate/60, digits=2))
  circxStudDate <- dfCircuitos  %>%  group_by(Alumno,Dates) %>%
    arrange(Alumno,Dates) %>%
    summarise(NumCircu=length(Circuito),
              NumExp = length(Circuito),
              NumCCircu=length(unique(Circuito)),
              NumNCircu=length(unique(CircuitoNormalizado)),
              NumSCircu=length(unique(CircuitoSimplificado)),
              .groups="drop")
  sumxStudDate <- merge(sumxStudDate,circxStudDate,all=TRUE)
  sumxStudDate[is.na(sumxStudDate)] <- 0

  # status <- paste0("done-", format(Sys.time(),"%Y%m%d%H%M%S"))
  # save(status, file=paste0(sessionID,"_std.rda"))   
  print(paste("post-ACxStudDate-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  (sumxStudDate_ext <<- sumxStudDate)
}

aggreg_xDate <- function(dfAC_xStudDate) {
  if(is.null(dfAC_xStudDate)) return(NULL)
  
  print(paste("ACxDate-",format(Sys.time(),"%Y%m%d%H%M%S")))
  
  (sumxDate_ext <<- dfAC_xStudDate %>% group_by(Dates) %>% 
     summarise(NumExp = sum(NumExp),
               TotalTime = sum(TimeInDate)/60,.groups="drop"))
}

### >> GRAPHS ####
## Time vs Date ####
createPlot_TimexDate <- function(dfACxD) {
  if(is.null(dfACxD)) return(NULL)
  
  Totaltimebydate2 <- dfACxD %>% 
    mutate(Time=round(TotalTime,digits = 2))
  
  tt <- data.frame(Dates = as.character(seq(min(as.Date(Totaltimebydate2$Dates)),
                                    max(as.Date(Totaltimebydate2$Dates)), by=1)))
  Totaltimebydate2 <- merge(Totaltimebydate2,tt,all=T)
  Totaltimebydate2[is.na(Totaltimebydate2)] <- 0
  
  Totaltimebydate2$Dates <- as.Date(Totaltimebydate2$Dates, tz="GMT")
  
  Tot <- xts(Totaltimebydate2$Time, Totaltimebydate2$Dates)
  colnames(Tot) <- "Time"
  
  dygraph(Tot) %>%
    dyAxis("y",rangePad=2,label = "Time, in h") %>%
    dySeries("Time", label = "Time, in h", axis="y")  %>%
    dyOptions(axisLineWidth = 1.5,drawGrid = FALSE, 
              useDataTimezone = FALSE, axisLabelFontSize=11) %>%
    dyLegend(width = 400) %>% dyRangeSelector()
}

## Circuits Timeline vs User ####
# timelineuser<- function(dfActionCircuit) {
#   ggplot(dfActionCircuit, aes(x = Alumno, y = Time, color = Mesure)) + 
#     geom_point(size = 1.5, alpha = 0.8) +
#     theme_few()+
#     theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())+scale_fill_manual(values=gra)
# }

## Experiments per Date ####
createPlot_ExpxDate <- function(dfACxD) {
  if(is.null(dfACxD)) return(NULL)
  
  tt <- data.frame(Dates = as.character(seq(min(as.Date(dfACxD$Dates)),
                                            max(as.Date(dfACxD$Dates)), by=1)))
  dfACxD1 <- merge(dfACxD,tt,all=T)
  dfACxD1[is.na(dfACxD1)] <- 0

  dfACxD1$Dates <- as.Date(dfACxD1$Dates, tz="GMT")
  
  ses<- xts(dfACxD1$NumExp,dfACxD1$Dates)
  colnames(ses) <- "ses"

  dygraph(ses) %>% dyAxis("y",rangePad=c(-0.05),label = "Experiments") %>%
    dySeries("ses", label = "Experiments",axis="y") %>% 
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE,
              useDataTimezone=FALSE, axisLabelFontSize=11) %>%
    dyLegend(width = 400)%>% dyRangeSelector()
}

plotDistribution <- function(values=NA,minValues=NULL,maxValues=NULL,xlabel="") {
  if(is.null(values)) return(NULL)
  
  if (is.null(maxValues)) {
    maxValues<-max(values,na.rm=TRUE)
    maxValues<-ceiling(maxValues/10^floor(log10(maxValues)-1))*10^floor(log10(maxValues)-1)
  }
  
  if (is.null(minValues)) {
    minValues<-min(values,na.rm=TRUE)
    if(minValues>0) 
      minValues<-floor(minValues/10^floor(log10(minValues)-1))*10^floor(log10(minValues)-1)
  }
  
  # Freedman-Diaconis rule
  bw <- (maxValues-minValues) / (2 * IQR(values) / length(values)^(1/3))
  
  bw2 <- floor(bw/10^floor(log10(bw)))
  bw2 <- ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2 <- bw2*10^floor(log10(bw))
  
  # check the number of bins is OK
  nbins <- ceiling((maxValues-minValues)/bw2)
  nbins <- max(7, nbins)
  nbins <- min(20, length(values)/4, nbins)
  
  bw <- (maxValues-minValues)/nbins
  bw2 <- floor(bw/10^floor(log10(bw)))
  bw2 <- ifelse(bw2>=5,5,ifelse(bw2>=2,2,1))
  bw2 <- bw2*10^floor(log10(bw))
  
  bw2 <- ifelse(nbins>=7,bw2,bw2/2+.Machine$double.eps)
  bw2 <- floor(bw2/10^floor(log10(bw2)))*10^floor(log10(bw2))
  
  ggminValues<-floor(minValues/bw2)*bw2
  ggmaxValues<-ceiling(maxValues/bw2)*bw2
  
  theme_remove_all <- theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

  g <- ggplot(data=NULL,aes(x=values)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue",
                   binwidth=bw2, boundary=ggmaxValues)+ 
    geom_density(aes(x=values,y=after_stat(density)*length(values)*bw2),
              color="#CC6600", size=1) + 
    geom_vline(aes(xintercept=mean(values)), linetype="dashed", size=1)
  if(length(values) <=100) g <- g + geom_rug(alpha=0.5)
  
  g + labs(x=xlabel, y="Frequency")+
    coord_cartesian(xlim= c(ggminValues, ggmaxValues)) +
    theme_bw() + theme_remove_all +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          axis.title = element_text(size=14),
          axis.text= element_text(size=11))
}


### >> CALCULATIONS ####
## Time ####
InfovalueBoxTT <- function(dfACxStud){
  tottime<-sum(dfACxStud$TotalTime)
  round(tottime,digits = 2)
}

InfovalueBoxMeT <- function(dfACxStud){
  round(mean(dfACxStud$TotalTime),digits = 2)
}

InfovalueBoxMaxT <- function(dfACxStud){
  round(max(dfACxStud$TotalTime),digits = 2)
}

InfovalueBoxMinT <- function(dfACxStud){
  round(min(dfACxStud$TotalTime),digits = 2)
}

## Experiments #### 
InfovalueBoxTNumExp <- function(dfACxStud){
  return(sum(dfACxStud$NumExp))
}

InfovalueBoxMeanExp <- function(dfACxStud){
  return(round(mean(dfACxStud$NumExp), digits=2))
}

InfovalueBoxMinExp <- function(dfACxStud){
  return(min(dfACxStud$NumExp))
}

InfovalueBoxMaxExp <- function(dfACxStud){
  return(max(dfACxStud$NumExp))
}

## Unique, normalized ####
InfovalueBoxMinNorm <- function(dfACxStud){
  return(min(dfACxStud$NumNCircu))
}

InfovalueBoxMeanNorm <- function(dfACxStud){
  return(round(mean(dfACxStud$NumNCircu), digits=2))
}

InfovalueBoxMaxNorm <- function(dfACxStud){
  return(max(dfACxStud$NumNCircu))
}

InfovalueBoxGNumNorm <- function(dfActionCircuit){
  return(length(unique(dfActionCircuit$CircuitoNormalizado)))
}

## Unique, simplified ####
InfovalueBoxMinSimp <- function(dfACxStud){
  return(min(dfACxStud$NumSCircu))
}

InfovalueBoxMeanSimp <- function(dfACxStud){
  return(round(mean(dfACxStud$NumSCircu), digits=2))
}

InfovalueBoxMaxSimp <- function(dfACxStud){
  return(max(dfACxStud$NumSCircu))
}

InfovalueBoxGNumSimp <- function(dfActionCircuit){
  return(length(unique(dfActionCircuit$CircuitoSimplificado)))
}

## Closed Circuits? ####
InfovalueBoxNCC <- function(dfActionCircuit){
  Accircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Circuito) %>%
    summarise(NuCC= sum(unique(EsCircuitoCerrado)),.groups="drop")
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
    group_by(Alumno,Circuito) %>% 
    summarise(NumCcircuit=sum(unique(EsCircuitoCerrado)),.groups="drop") %>%
    group_by(Alumno) %>%  summarise(NumCcircuitS=sum(NumCcircuit),.groups="drop")
  MeanNumCCirSV <- round(mean(MeanCNAcircu$NumCcircuitS),digits = 2)
  return(MeanNumCCirSV)
}

InfovalueBoxMNNC <- function(dfActionCircuit){
  MeanNNAcircu <- dfActionCircuit %>% select(Alumno,NumCircuito,Circuito,CircuitoNormalizado,EsCircuitoCerrado,MultimetroMal,FechaHoraEnvio)%>%
    filter(complete.cases(.)) %>%  group_by(Alumno) %>% 
    summarise(Numncirc=length(unique(CircuitoNormalizado)),.groups="drop")
  MeanNumNCircSV <- round(mean(MeanNNAcircu$Numncirc),digits = 2)
  return(MeanNumNCircSV)
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

## Measure ####
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

#### WORK INDICATORS ####

#With Milestones
generatedfActionsMilestones <- function (actions, dfMilestones) {
  if(is.null(actions)) return(NULL)
  if(is.null(dfMilestones)) return(NULL)
  
  dfActionsSorted <- actions
  
  milestones <- as.character(dfMilestones$milNames)
  regExps <- as.character(dfMilestones$regExps)
  postLogicEval <- as.character(dfMilestones$logTests)
  
  # milestonesRegExp <- regExps
  # milestonesName <- milestones
  # milestonesLogEv <- postLogicEval
  
  # Milestones per accio
  testVector<-as.character(dfActionsSorted$XML)
  regExps<-paste("(?=",regExps,")",sep="")
  
  dfRegExpsPerElement<-data.frame()
  for(i in 1:length(testVector)) {
    lisRegExpsElement<-logical(length(regExps))
    for (j in 1:length(regExps)) {
      regexecResults<-gregexpr(regExps[j],testVector[i],perl=TRUE)
      
      evaluated<-FALSE
      for(k in 1:length(regexecResults[[1]])) {
        result<-regexecResults[[1]]
        if(result[1]!=-1 & length(result)>0) {
          m<-NULL
          
          if(!is.null(attr(regexecResults[[1]],"capture.start"))) {
            
            result<-as.vector(attr(regexecResults[[1]],"capture.start")[k,])
            attr(result,"match.length")<-as.vector(attr(regexecResults[[1]],"capture.length")[k,])
            
            for(l in 1:length(result)) {
              posMatch<-result[l]
              lenMatch<-attr(result,"match.length")[l]
              m<-c(m,substr(as.character(testVector[i]),
                            posMatch,posMatch+lenMatch-1))
            }
          }
        }
        evaluated<-evaluated | (postLogicEval[j]=="" & result[1]!=-1)
        
        if(postLogicEval[j]!="" & result[1]!=-1) {
          evaluated<-evaluated | as.logical(eval(parse(text=postLogicEval[j])))
        }			
      }
      lisRegExpsElement[j]<-ifelse(is.na(evaluated),F,evaluated)
    }
    dfRegExpsPerElement<-rbind(dfRegExpsPerElement,lisRegExpsElement)
  }
  colnames(dfRegExpsPerElement)<-milestones
  
  (dfActionsObsItems_ext <<- cbind(dfActionsSorted,dfRegExpsPerElement))
}  

generatedfUsersMilestones <- function (actionsMilestones,dfMilestones) {
  if(is.null(actionsMilestones)) return(NULL)
  if(is.null(dfMilestones)) return(NULL)
  
  dfActionsSortedMilestones<-actionsMilestones

  ## Creación de ID y filename
  dfActionsSortedMilestones$number <- seq(1,nrow(dfActionsSortedMilestones),1)
  dfActionsSortedMilestones$filename <- 1

  milestones <- as.character(dfMilestones$milNames)
  regExps <- as.character(dfMilestones$regExps)
  postLogicEval <- as.character(dfMilestones$logTests)
  
  milestonesRegExp <- regExps
  milestonesName <- milestones
  milestonesLogEv <- postLogicEval
  
  milestones <- regExps
  milestones[1:length(milestonesName)]<-milestonesName
  postLogicEval<-rep("",length(regExps))
  postLogicEval[1:length(milestonesLogEv)]<-milestonesLogEv
  
  # Muntatge de dfSessions
  vecSessions<-unique(as.character(dfActionsSortedMilestones[,"Sesion"]))
  
  vecMinId<-character(length(vecSessions))
  vecMaxId<-character(length(vecSessions))
  vecMinTime<-character(length(vecSessions))
  vecMaxTime<-character(length(vecSessions))
  vecNActions<-numeric(length(vecSessions))
  vecUser<-character(length(vecSessions))
  vecFile<-character(length(vecSessions))
  vecNSeq<-numeric(length(vecSessions))
  vecTimeOnTask<-numeric(length(vecSessions))
  
  dfMilestonesPerSession<-data.frame(matrix(nrow=length(vecSessions),
                                            ncol=length(milestones)*4),row.names=as.character(vecSessions))
  
  
  colnames(dfMilestonesPerSession)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                      paste("dt_",milestones,sep=""),
                                      paste("dtr_",milestones,sep="")) 
  
  for (i in 1:length(vecSessions)) {
    dfActionsPerSession<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"Sesion"])==vecSessions[i],]
    dfActionsPerSessionSorted<-dfActionsPerSession[order(dfActionsPerSession[,"number"]),]
    #dfActionsPerSessionSorted<-dfActionsPerSession

    primer<-head(dfActionsPerSessionSorted,1)
    darrer<-tail(dfActionsPerSessionSorted,1)
    vecMinId[i]<-primer[,"number"]
    vecMaxId[i]<-darrer[,"number"]
    vecMinTime[i]<-as.character(primer[,"FechaHoraEnvio"])
    vecMaxTime[i]<-as.character(darrer[,"FechaHoraRespuesta"])
    vecNActions[i]<-nrow(dfActionsPerSessionSorted)
    vecFile[i]<-as.character(primer[,"filename"])
    vecUser[i]<-as.character(primer[,"Alumno"])
    vecNSeq[i]<-max(table(dfActionsPerSessionSorted[,"number"]))
    #vecTimeOnTask[i]<-darrer[,"diff_time_cum_real"]/60
    vecTimeOnTask[i]<-darrer[,"Time"]/60
    
    for(m in 1:length(milestones)) {
      dfActionPerSessionMilestone<-dfActionsPerSessionSorted[
        dfActionsPerSessionSorted[,milestones[m]]==TRUE,]
      dfMilestonesPerSession[i,paste("n_",milestones[m],sep="")]<-
        nrow(dfActionPerSessionMilestone)
      if(nrow(dfActionPerSessionMilestone)>0) {
        dfMilestonesPerSession[i,paste("t_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"TiempoDesdeAccionAnterior"])
        dfMilestonesPerSession[i,paste("dt_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"TiempoAcumulado"])
        dfMilestonesPerSession[i,paste("dtr_",milestones[m],sep="")]<-
          as.character(head(dfActionPerSessionMilestone,1)[,"Time"])
      }
    }
  }
  
  vecSessionStart<-strptime(vecMinTime, "%Y%m%d%H%M%OS")
  vecSessionEnd<-strptime(vecMaxTime, "%Y%m%d%H%M%OS")
  vecDurationSession<-as.numeric(difftime(vecSessionEnd,vecSessionStart,units="mins"))
  
  dfSessions<-data.frame(vecSessions,vecFile,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
                         vecMaxTime,vecDurationSession,vecTimeOnTask,dfMilestonesPerSession)
  
  #colnames(dfSessions)<-c("session","filename","user","n_actions","n_seq","min_number","max_number",
  #                        "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerSession))

  #dfSessions<-data.frame(vecSessions,vecUser,vecNActions,vecNSeq,vecMinId,vecMaxId,vecMinTime,
  #                       vecMaxTime,vecDurationSession,vecTimeOnTask,dfMilestonesPerSession)
  
  colnames(dfSessions)<-c("Sesion","filename","Alumno","n_actions","n_seq","min_number","max_number",
                         "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerSession))

  # Muntatge de dfUsers
  vecUsers<-levels(dfActionsSortedMilestones[,"Alumno"])

  vecMinTime<-character(length(vecUsers))
  vecMaxTime<-character(length(vecUsers))
  vecNActions<-numeric(length(vecUsers))
  vecNSessions<-numeric(length(vecUsers))
  vecNFiles<-numeric(length(vecUsers))
  vecNSeq<-numeric(length(vecUsers))
  vecDuration<-numeric(length(vecUsers))
  vecTimeOnTask<-numeric(length(vecUsers))
  
  dfMilestonesPerUser<-data.frame(matrix(nrow=length(vecUsers),
                                         ncol=length(milestones)*4),row.names=as.character(vecUsers))
  
  colnames(dfMilestonesPerUser)<-c(paste("n_",milestones,sep=""),paste("t_",milestones,sep=""),
                                   paste("dt_",milestones,sep=""),
                                   paste("dtr_",milestones,sep=""))
  
  for (i in 1:length(vecUsers)) {
    dfActionsPerUser<-dfActionsSortedMilestones[as.character(dfActionsSortedMilestones[,"Alumno"])==vecUsers[i],]
    dfSessionsPerUser<-dfSessions[as.character(dfSessions[,"Alumno"])==vecUsers[i],]

    dfActionsPerUserSorted<-dfActionsPerUser[order(dfActionsPerUser[,"Sesion"],dfActionsPerUser[,"number"]),]
    
    dfSessionsPerUserSorted<-dfSessionsPerUser[order(dfSessionsPerUser[,"Sesion"]),]
    
    dfSessionsPerUserSorted$duration_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    dfSessionsPerUserSorted$time_on_task_previous<-rep(0,nrow(dfSessionsPerUserSorted))
    
    if (nrow(dfSessionsPerUserSorted)>1) {
      for(s in 2:nrow(dfSessionsPerUserSorted)) {
        dfSessionsPerUserSorted$duration_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"Sesion"])
                                                                                        < as.character(dfSessionsPerUserSorted[s,"Sesion"])),"duration"])
        dfSessionsPerUserSorted$time_on_task_previous[s]<-sum(dfSessionsPerUserSorted[which(as.character(dfSessionsPerUserSorted[,"Sesion"])
                                                                                            < as.character(dfSessionsPerUserSorted[s,"Sesion"])),"time_on_task"])	
      }
    }
    
    vecMinTime[i]<- NA
    vecMaxTime[i]<-NA
    
    vecNActions[i]<-NA
    vecNSessions[i]<-NA
    vecNFiles[i]<-NA
    
    # com a suma de sessions, en minuts
    vecDuration[i]<- NA
    vecTimeOnTask[i]<-NA
    
    for(m in 1:length(milestones)) {
      dfMilestonesPerUser[i,paste("n_",milestones[m],sep="")]<-
        sum(dfSessionsPerUserSorted[,paste("n_",milestones[m],sep="")],
            na.rm=TRUE)

      if(dfMilestonesPerUser[i,paste("n_",milestones[m],sep="")] > 0) {
        dfMilestonesPerUser[i,paste("t_",milestones[m],sep="")]<-
          min(dfSessionsPerUserSorted[,paste("t_",milestones[m],sep="")],
              na.rm=TRUE)
        dfMilestonesPerUser[i,paste("dt_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerUserSorted[,paste("dt_",milestones[m],sep="")])+
                dfSessionsPerUserSorted[,"duration_previous"]*60,na.rm=TRUE)
        dfMilestonesPerUser[i,paste("dtr_",milestones[m],sep="")]<-
          min(as.numeric(dfSessionsPerUserSorted[,paste("dtr_",milestones[m],sep="")])+
                dfSessionsPerUserSorted[,"time_on_task_previous"]*60,na.rm=TRUE)
      }
    }	
  }

  # dfUsers <- data.frame(vecUsers,vecNSessions,vecNFiles,vecNActions,vecMinTime,
  #                      vecMaxTime,vecDuration,vecTimeOnTask,dfMilestonesPerUser)
  # 
  # colnames(dfUsers)<-c("Alumno","n_sessions","n_files","n_actions",
  #                      "min_time","max_time","duration","time_on_task",colnames(dfMilestonesPerUser))
  
  dfUsers<-data.frame(vecUsers,
                      dfMilestonesPerUser[,paste("n_",milestones,sep="")]>0)
  
  
  rownames(dfUsers)<-NULL
  colnames(dfUsers)<-c("Alumno",milestones)
  dfUsers$Alumno <- factor(dfUsers$Alumno,
                           levels=levels(dfActionsSortedMilestones$Alumno),
                           ordered=T)
  
  (dfObsItems_xUser_ext <<- dfUsers)
}

generatedStudentsMilestonesEv <- function (studentsObsMilestones,dfMilestonesEv) {
  if(is.null(studentsObsMilestones)) return(NULL)

  if(is.null(dfMilestonesEv)) {
    df<-studentsObsMilestones
    # colnames(df)[1]<-"student"
  } else {
    df <- data.frame(as.character(studentsObsMilestones[,1]))
    om <- studentsObsMilestones[,-1]
    
    evNames <- as.character(dfMilestonesEv[,1])
    evTests <- as.character(dfMilestonesEv[,2])
    evTests <- gsub("om['","om[,'",evTests,fixed=TRUE)
    
    for(j in 1:nrow(dfMilestonesEv)) {
      df <- cbind(df,as.logical(eval(parse(text=evTests[j]))))
    }              
    colnames(df)<-c("Alumno",evNames)
    df$Alumno <- factor(as.character(df$Alumno),
                        levels=levels(studentsObsMilestones$Alumno),
                        ordered=T)
  }
  
  df$grades <- (rowSums(df[,2:length(df)])/(length(df)-1))*100
  (dfEvMilestones_xUser_ext <<- df)
}

generatedfObs <- function(actionsMilestones, milestones) {
  selCols <- c("filename","user","number","time","diff_time_cum","diff_time_cum_real")[
    c("filename","user","number","time","diff_time_cum","diff_time_cum_real") %in%
      colnames(actionsMilestones)] 
  actionsMilestones[
    apply(actionsMilestones[,milestones],1,sum)>0,
    c(selCols,milestones)]
}


#COLORS
getdfOIColors <- function(dfObsItems, dfEVMil) {
  if(is.null(dfObsItems)) return(NULL)
  
  dfObsItemsEvMilest<-data.frame(item=as.character(dfObsItems$milNames),
                                 stringsAsFactors = FALSE)
  if(is.null(dfEVMil)) {
    dfObsItemsEvMilest$evMil <- dfObsItemsEvMilest$item
  } else {
    dfObsItemsEvMilest$evMil <- 
      sapply(dfObsItemsEvMilest$item,
             function(x) {paste(ifelse(grepl(x,dfEVMil$evTests),
                                       as.character(dfEVMil$milNames),""),collapse = "")})
  }
  
  ncolors <- length(unique(dfObsItemsEvMilest$evMil[dfObsItemsEvMilest$evMil!=""]))
  colorsMil <- rep(c(brewer.pal(9,"Set1"),brewer.pal(12,"Set3")),100)[1:ncolors]
  if (ncolors!=length(unique(dfObsItemsEvMilest$evMil))) colorsMil <- c(colorsMil, "grey60")
  
  dfEvColor <- data.frame(evMil=unique(dfObsItemsEvMilest$evMil),colorMil=colorsMil)
  dfObsItemsEvMilest <- merge(dfObsItemsEvMilest,dfEvColor,by="evMil") %>% arrange(item)
  dfObsItemsEvMilest$colorInk <- ifelse(sapply(dfObsItemsEvMilest$colorMil, function(x) mean(ColToRgb(x))/255)<.5,
                                        "white","black")
  dfObsItemsEvMilest
}

#GRAPHS
#With milestones.
visirtrMilestonesDifficulty <- function(bMilestones=NULL, evaluation = F) {
  if (is.null(bMilestones)) return(NULL) 

  vecExits<-100*apply(bMilestones[,-1],2,sum)
  df <- data.frame(OItems=names(vecExits),Percent=vecExits/nrow(bMilestones))
  
  theme_remove_all <- theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
  
  g <- ggplot(df,aes(x=OItems, y=Percent)) +
    geom_bar(stat="identity", color=NA, fill="#00a676")+
    labs(x=ifelse(evaluation, "Assessment milestone", "Observation item"),
         y=ifelse(evaluation,"Achievement, in %","Performance, in %"))+
    scale_x_discrete(limits=rev(levels(as.factor(df$OItems)))) +
    scale_y_continuous(limits = c(0,100), expand=c(0.01,0.01)) +
    coord_flip() +
    theme_bw() + theme_remove_all +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
             axis.title = element_text(size=14),
             axis.text= element_text(size=11))
  g
}

visirtrHeatMapAchievedMilestonePerId<-function(bMilestones=NULL,labels=NULL, evaluation=F) {
  if (is.null(bMilestones)) return(NULL)
  
  nMilestones<-data.matrix(bMilestones[,-1])
  if(!is.null(labels)) {
    rownames(nMilestones)<-labels
  } else {
    rownames(nMilestones)<-bMilestones[,1]
  }
  mode(nMilestones)<-"numeric"
  
  colHM<-c("#ff7c74","#00a676")

  bMilestonesL <- pivot_longer(bMilestones,-1,
                               names_to = "WorkIndicator",
                               values_to = "Achieved")
  bMilestonesL$Alumno <- factor(as.character(bMilestonesL$Alumno),
                                levels=levels(bMilestones$Alumno),
                                ordered=T)
  
  g <- ggplot(data = bMilestonesL, aes(x = Alumno, y = WorkIndicator)) + 
    theme_bw() +
    geom_tile(aes(fill=Achieved), width=0.9, height=0.9) +
    guides(fill=guide_legend(title=ifelse(evaluation, "Achieved", "Performed"))) +
    labs(x ="User", y= ifelse(evaluation, "Assessment milestone", "Observation item")) +
    theme(axis.title = element_text(size=14),
          axis.text= element_text(size=11),
          legend.title = element_text(size=14),
          legend.text= element_text(size=11),
          axis.text.x=element_text(angle = 90, vjust = 0.5),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
    scale_fill_manual(values=colHM) +
    scale_y_discrete(limits=rev(levels(factor(bMilestonesL$WorkIndicator))))
  
  if(nrow(bMilestones) > 50) g <- g +
    theme(axis.text.x = element_blank())
  if(ncol(bMilestones) - 1 > 30) g <- g +
    theme(axis.text.y = element_blank())
  g
    
  # heatmap.2(nMilestones,dendrogram="row",Colv=NULL,Rowv=TRUE,margins=c(6,6),
  #           cexRow=0.6,cexCol=0.8,breaks=c(-0.5,0.5,1.5),key=FALSE,trace="none",
  #           scale="none",col=colHM,
  #           colsep=0:ncol(nMilestones)-1,rowsep=0:nrow(nMilestones)-1,
  #           lmat=rbind(c(4,3),c(2,1)), 
  #           lhei=c(0.5,9.5),
  #           lwid=c(2,8))
}

## Functions not yet used
gradeDistribution <- function(dfStudentsMilestonesEv=NA) {
  if(is.null(dfStudentsMilestonesEv)) return(NULL)
  
  ggplot(data=NULL,aes(x=dfStudentsMilestonesEv)) +
    geom_histogram(color="black", alpha=.2, fill="skyblue", binwidth=5, boundary = 0) + 
    geom_vline(aes(xintercept=mean(dfStudentsMilestonesEv)), linetype="dashed", size=1) +
    geom_rug(alpha=.5) +
    labs(x="Grades", y="Frequency") + theme_bw()
}

nActVStoTVSGrade <- function (dfStudents=NULL, dfStudentsMilestones=NULL, id=NULL){
  
  if(is.null(dfStudents)|is.null(dfStudentsMilestones)|is.null(id)) return(NULL)
  
  df <- merge(x = dfStudents, y = dfStudentsMilestones, by.x = id, by.y = "student")
  
  ggplot(df, aes(x=time_on_task, y=n_actions)) +  
    geom_point(size = I(4), alpha = I(0.8), aes(color = grades)) +
    labs(x = "Time on Task", y = "Number of Actions") + scale_color_gradientn(
      colors=c("red4","red","white","green","forestgreen"),values=c(0,.4,.5,.6,1))+theme_bw()
}

#### REPORT ####
showReport <- function() {
  Sys.umask("011")
  td <- tempdir()
  dir.create(td, showWarnings = FALSE, recursive = FALSE, mode = "766")
  reportName <- paste0(td,"/reportVISIRDB_",
                       format(Sys.time(),"%Y%m%d%H%M%S"), ".html")
  # reportName <- knitr::knit2html("reportVISIRDB.Rmd", output=reportName)
  rmarkdown::render("reportVISIRDB.Rmd", 
                                  output_format = "html_document",
                                  output_file=reportName)
  pander::openFileInOS(normalizePath(reportName))
}
