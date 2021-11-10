
rm(list=ls())

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(vroom)
library(rstudioapi)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ** this file location as wd **
setwd("~/ANAC_MICRODADOS/data")
datayears <- 2000:2021

#----------------------------------------------------------------------------
# basica
#----------------------------------------------------------------------------

basica <- list()

for (i in 1:length(datayears)){
  files <- list.files(pattern = paste("basica",datayears[i],sep = "")) 
  basicax <- vroom(files)
  basicax <- basicax%>%
    select(nm_empresa,sg_empresa_iata, sg_empresa_icao,nm_pais,nr_voo,nr_ano_mes_referencia,
           sg_icao_origem, sg_iata_origem,sg_icao_destino,sg_iata_destino,nr_passag_pagos,
           nr_passag_gratis,kg_bagagem_livre,kg_bagagem_excesso,kg_carga_paga,kg_carga_gratis,
           kg_correio,nr_etapa,nr_rpk,nr_rtk, nr_ask,nr_ask,nr_atk,kg_peso,lt_combustivel,nm_pais)%>%
    mutate(Route = paste(sg_icao_origem,sg_icao_destino, sep = "-"))%>%
    group_by(sg_empresa_icao, nr_ano_mes_referencia,Route)%>%
    #drop_na()%>%
    summarise(sg_icao_origem = sg_icao_origem[1],
              sg_icao_destino = sg_icao_destino[1],
              nr_passag_pagos = sum(nr_passag_pagos, na.rm = T),
              nr_passag_gratis = sum(nr_passag_gratis,na.rm = T),
              kg_bagagem_livre = sum(kg_bagagem_livre,na.rm = T), 
              kg_bagagem_excesso = sum(kg_bagagem_excesso,na.rm = T),
              kg_carga_paga = sum(kg_carga_paga,na.rm = T), 
              kg_carga_gratis = sum(kg_carga_gratis,na.rm = T),
              kg_correio = sum(kg_correio,na.rm = T),
              nm_empresa = nm_empresa[1],
              decolagens = n(),
              nr_rpk = sum(nr_rpk, na.rm = T),
              nr_rtk = sum(nr_rtk,na.rm = T),
              nr_ask = sum(nr_ask,na.rm = T), 
              nr_atk = sum(nr_atk,na.rm = T),
              kg_peso = sum(kg_peso,na.rm = T), 
              lt_combustivel = sum(lt_combustivel,na.rm = T),
              nm_pais = nm_pais[1])
  
  basica[[i]] <- basicax
}

basica <- bind_rows(basica, .id = "column_label")

#### airports lat lon ####################################################################

airports_world <- read_delim("airports_world.txt", 
                             "\t", quote = "'", escape_double = FALSE, 
                             trim_ws = TRUE)

airports_o <- airports_world %>%
  select(ICAO,lat,lon)%>%
  mutate(ICAO = as.character(ICAO))%>%
  rename("sg_icao_origem" = ICAO,"lat_o" = lat,"lon_o" = lon)

airports_d <- airports_world %>%
  select(ICAO,lat,lon)%>%
  mutate(ICAO = as.character(ICAO))%>%
  rename("sg_icao_destino" = ICAO,"lat_d" = lat,"lon_d" = lon)

rm(airports_world)

basica <- merge(basica,airports_o,by = "sg_icao_origem")
basica <- merge(basica,airports_d,by = "sg_icao_destino")

###############################################################################

save(basica,file = "basica_agregada_rota_mes_empresa.rda")
write.csv(basica,file = "basica_agregada_rota_mes_empresa.csv")
