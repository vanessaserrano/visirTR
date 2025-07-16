options(install.packages.check.source = "no",
        shiny.maxRequestSize=5*1024^3)

pckgs<-c("pander",
         "shiny","shinyjs","shinythemes", "ggthemes","shinydashboard",
         "tidyverse","XML","DT","gplots",
         "xts","dygraphs","scales",
         "formattable","treemap","viridis","cat","gsubfn","vroom",
         "parallel", "markdown", "knitr", "writexl")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
 # for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/",
 #                                              quiet=TRUE, type="binary")}
for(pckg in pckgs2Install) {install.packages(pckg,repos="https://cloud.r-project.org/")}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

source("functions_VISIRDB.R")