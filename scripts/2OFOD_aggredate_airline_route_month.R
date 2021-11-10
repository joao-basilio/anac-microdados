
rm(list=ls())

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(vroom)
library(rstudioapi)
library(geosphere)
library(readr)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ** this file location as wd **
setwd("~/ANAC_MICRODADOS/data")

airports_world <- read_delim("airports_world.txt", 
                             "\t", quote = "'", escape_double = FALSE, 
                             trim_ws = TRUE)

airports_o <- airports_world %>%
  select(ICAO,lat,lon)%>%
  mutate(ICAO = as.character(ICAO), lat = as.numeric(lat),lon = as.numeric(lon))%>%
  rename("sg_icao_origem" = ICAO,"lat_o" = lat,"lon_o" = lon)

airports_d <- airports_world %>%
  select(ICAO,lat,lon)%>%
  mutate(ICAO = as.character(ICAO), lat = as.numeric(lat),lon = as.numeric(lon))%>%
  rename("sg_icao_destino" = ICAO,"lat_d" = lat,"lon_d" = lon)

rm(airports_world)

datayears <- 2000:2021

#----------------------------------------------------------------------------
# combinada
#----------------------------------------------------------------------------

combinada <- list()

for (i in 1:length(datayears)){
  files <- list.files(pattern = paste("combinada",datayears[i],sep = "")) 
  files <- files[!grepl("combinada2018-05",files)] # errado no site da anac
  combinadax <- vroom(files)
  combinadax <- combinadax%>%
    select(nm_empresa,sg_empresa_iata, sg_empresa_icao,nm_pais,nr_voo,nr_ano_mes_referencia,
           sg_icao_origem, sg_iata_origem,sg_icao_destino,sg_iata_destino,nr_passag_pagos,
           nr_passag_gratis,kg_bagagem_livre,kg_bagagem_excesso,kg_carga_paga,kg_carga_gratis,
           kg_correio,nr_etapa,nm_pais)%>%
    mutate(kg_pax = nr_passag_pagos*75)%>%
    mutate(Route = paste(sg_icao_origem,sg_icao_destino, sep = "-"))%>%
    group_by(sg_empresa_icao, nr_ano_mes_referencia,Route)%>%
    #drop_na()%>%
    summarise(nr_passag_pagos = sum(nr_passag_pagos, na.rm = T),
              nr_passag_gratis = sum(nr_passag_gratis,na.rm = T),
              kg_bagagem_livre = sum(kg_bagagem_livre,na.rm = T), 
              kg_bagagem_excesso = sum(kg_bagagem_excesso,na.rm = T),
              kg_carga_paga = sum(kg_carga_paga,na.rm = T), 
              kg_carga_gratis = sum(kg_carga_gratis,na.rm = T),
              kg_correio = sum(kg_correio,na.rm = T),
              nm_empresa = nm_empresa[1],
              sg_icao_origem = sg_icao_origem[1],
              sg_icao_destino = sg_icao_destino[1],
              kg_pax = sum(kg_pax, na.rm = T),
              nm_pais = nm_pais[1])
  
  combinada[[i]] <- combinadax
}

combinada <- bind_rows(combinada, .id = "column_label")

distances <- combinada%>% ungroup()%>%
  select(sg_icao_origem,sg_icao_destino)%>%
  unique()%>%
  merge(airports_o,by = "sg_icao_origem")%>%
  merge(airports_d,by = "sg_icao_destino")%>% 
  mutate(km_distance_OD = distHaversine(cbind(lon_o,lat_o),cbind(lon_d,lat_d))/1000)%>%
  mutate(Route = paste(sg_icao_origem,sg_icao_destino, sep = "-"))%>%
  select(Route, km_distance_OD,lon_o,lat_o,lon_d,lat_d)
  
combinada <- merge(combinada, distances, by = "Route")%>%
  mutate(rtodk = ((kg_pax + kg_bagagem_livre+kg_bagagem_excesso+kg_carga_paga+
                    kg_correio)/1000)*km_distance_OD)

save(combinada,file = "combinada_agregada_rota_mes_empresa.rda")
write.csv(combinada,file = "combinada_agregada_rota_mes_empresa.csv")

