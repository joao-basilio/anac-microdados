
rm(list=ls())

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(vroom)
library(plotly)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))## ** this file location as wd **

load("combinada_agregada_rota_mes_empresa.rda")
load("basica_agregada_rota_mes_empresa.rda")

basica <- basica %>% group_by(sg_empresa_icao,nr_ano_mes_referencia)%>%
  subset(nm_pais == "BRASIL")%>%
  summarise(nr_passag_pagos = sum(nr_passag_pagos, na.rm = T),
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
                      lt_combustivel = sum(lt_combustivel,na.rm = T))


combinada <- combinada %>% group_by(sg_empresa_icao,nr_ano_mes_referencia)%>%
  subset(nm_pais == "BRASIL")%>%
  summarise(nr_passag_pagos = sum(nr_passag_pagos, na.rm = T),
                      nr_passag_gratis = sum(nr_passag_gratis,na.rm = T),
                      kg_bagagem_livre = sum(kg_bagagem_livre,na.rm = T), 
                      kg_bagagem_excesso = sum(kg_bagagem_excesso,na.rm = T),
                      kg_carga_paga = sum(kg_carga_paga,na.rm = T), 
                      kg_carga_gratis = sum(kg_carga_gratis,na.rm = T),
                      kg_correio = sum(kg_correio,na.rm = T),
                      nm_empresa = nm_empresa[1],
                      kg_pax = sum(kg_pax, na.rm = T),
                      rtodk = sum(rtodk, na.rm = T))

df <- merge(basica,combinada,by = c("sg_empresa_icao","nr_ano_mes_referencia"))%>%
  mutate(FE = rtodk/lt_combustivel,
         FE2 = nr_rtk/lt_combustivel,
         circuity = nr_rtk/rtodk,
         month = as.numeric(factor(nr_ano_mes_referencia)),
         year_month = nr_ano_mes_referencia,
         year = substr(nr_ano_mes_referencia,1,4))%>%
  mutate(airline = as.character(sg_empresa_icao))%>%
  #mutate(nm_empresa = gsub( " .*$", "", nm_empresa.x ))%>%
  mutate(nm_empresa = gsub( "[^a-zA-Z].*$", "", nm_empresa.x ))%>%
  group_by(nr_ano_mes_referencia)%>%
  mutate(total = sum(rtodk))%>%
  group_by(nr_ano_mes_referencia,airline)%>%
  mutate(total2 = (sum(rtodk)/total)*100)%>%
  subset(rtodk > total*0.01)%>%
  subset(year > 2001)%>%
  ungroup()

#print(unique(df$airline))

## circuity -------------------------------------------------------------------------------------
circuity <- plot_ly(df, x = ~month, y = ~circuity,  
               colors =  "Paired") 
circuity <- circuity %>% add_lines(color = ~airline)


## FE -------------------------------------------------------------------------------------

FE <- plot_ly(df, x = ~month, y = ~FE,  
               colors =  "Paired", name = ~nm_empresa) 
FE <- FE %>% add_lines(color = ~airline)


## FE2 -------------------------------------------------------------------------------------

FE2 <- plot_ly(df, x = ~month, y = ~FE2,  
               colors =  "Paired") 
FE2 <- FE2 %>% add_lines(color = ~airline)


## rtodk -------------------------------------------------------------------------------------

rtodk <- plot_ly(df, x = ~month, y = ~rtodk,  
               colors =  "Paired") 
rtodk <- rtodk %>% add_lines(color = ~airline)

## rtk -------------------------------------------------------------------------------------

rtk <- plot_ly(df, x = ~month, y = ~nr_rtk,  
               colors =  "Paired") 
rtk <- rtk %>% add_lines(color = ~airline)


## fuel -------------------------------------------------------------------------------------

fuel <- plot_ly(df, x = ~month, y = ~lt_combustivel,  
               colors =  "Paired") 
fuel <- fuel %>% add_lines(color = ~airline)


