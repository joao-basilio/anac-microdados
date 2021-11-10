rm(list=ls())

library(rvest)
library(dplyr)
library(stringr)
library(qdapRegex)

setwd("~/ANAC_MICRODADOS")

mirror <- "https://www.gov.br/anac/pt-br/assuntos/regulados/empresas-aereas/envio-de-informacoes/microdados/microdados"

pg <- as.data.frame(readLines(mirror)) %>% 
  subset(grepl("zip", `readLines(mirror)`))%>%
  mutate(`readLines(mirror)` = rm_between(`readLines(mirror)`,'"', '"', extract=TRUE))#%>% uncomment these lines to get a specific file
   #subset(grepl("basica2000-01|combinada2000-01", `readLines(mirror)`)) # subset specific files

for (i in 1:NROW(pg)){
  tryCatch({
   url <- as.character(pg[i,1])
   filename <- as.character(rm_between(url,'microdados/', '.zip', extract=TRUE))
   download(url, dest=paste("~/it210/data/", filename, '.zip', sep=""), mode="wb")
   print(i)
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



