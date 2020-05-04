#### Instal·lación de paquetes ####
options(install.packages.check.source = "no")

pckgs<-c("shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","DT","gplots",
         "xts","dygraphs","scales",
         "formattable","treemap","viridis","cat","gsubfn")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
                                             quiet=TRUE, type="binary")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}


#### FUNCIÓN FRAGMENTO MÁS SIGNIFICATIVO ####
fragmentoSignificativo <- function(circuito){
  
  circuito_prin <- NA
  
  if (is.na(circuito)==TRUE){
    return(NA)
  }
  
  componentes <- strsplit(circuito,"/",fixed=TRUE)[[1]]
  conectores <- strsplit(componentes, " ")
  df <- data.frame(componentes)

  for (i in 1:nrow(df)) {
    if (substr(df[i,1],1,3) == "R_X") {
      df[i,2] <- as.character(substr(df[i,1], 5, as.integer(nchar(componentes[i])-3)))
    }
    else {
      df[i,2] <- as.character(substr(df[i,1], 5, as.integer(nchar(componentes[i]))))
    }
  }

  for (i in 1:nrow(df)) {
    df[i,3:4] <- unlist(strsplit(df[i,2],split="[ ,.]"))
  }

  df_2 <- df
  V3 <- c(as.character(df_2$V3),as.character(df_2$V4))
  V4 <- c(as.character(df_2$V4),as.character(df_2$V3))
  df <- data.frame(V3,V4)
  df$V3 <- as.character(V3)
  df$V4 <- as.character(V4)

  ## CAMBIO: el circuito debe tener los dos cables del multimetro, sino devuelve un NA ##
  mult <- length(unique(unlist(strapply(circuito, "DMM_[A-Z]{3}"))))
  if (mult < 2){
    return(NA)
  }
  ##
  
  nodesf1 <- as.character(data.frame(strapply(circuito, "DMM_[A-Z]{3}"))[1,1])
  
  if (length(nodesf1)==1) {
  
    df <- rbind(df,c("DMM_VHI","DMM_VLO"),c("DMM_VLO","DMM_VHI"),c("DMM_AHI","DMM_ALO"),
                c("DMM_ALO","DMM_AHI"))
    nodes <- unique(c(df$V3,df$V4))
    multimetre <- nodesf1
    nodes <- gsub(nodesf1,"", nodes)
    nodesf1anterior=c("")

    a <- 1
    b <- 1

    while (!all(nodesf1==nodesf1anterior)) {
  
      nodesf1anterior <- nodesf1
      aa <- c(0,0.5)

      while (aa[length(aa)] != aa[length(aa)-1] ){
  
        for (i in 1:nrow(df)) {
          if (nodesf1[a]==df[i,1]) {
            nodesf1 <- unique(c(nodesf1, df[i,2]))
            nodes <- gsub(df[i,2],"", nodes, fixed=TRUE)
          }
        }
        aa <- c(aa,length(nodesf1))
        #a <- a + 1
          if ((a+1)<=length(nodesf1)) {
            a <- a + 1
          }
      }
      aa <- c(0,0.5)
    }
  #}
  
# Imprime el circuito significativo
    #if (length(multimetre)==1) {
      y <- 0

      for (i in 1:length(componentes)){
        if (sum(df[i,1]==nodesf1)==0) {
          componentes <- componentes[-(i-y)]
          y <- y + 1
        }
      }

      circuito_prin <- paste(componentes, collapse = "/")
      return(circuito_prin)
      
  } else {
    return(NA)
    }
}


# Prueba
#for (i in 1:3){
#  circuito$B[i] <- fragmentoSignificativo(as.character(circuito$A[i]))
#}

#fragmentoSignificativo("R_X DC_+25V DMM_ALO 1k/R_X DMM_ALO GND 1k/W_X DC_+25V DMM_AHI/W_X DC_COM GND")
