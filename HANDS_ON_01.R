# SCRIPT + STUDENT INFO ---------------------------------------------------
# NOMBRE: CAROLINA ROMERO DELGADO
# EXP: 22169651
# TEMA: HANDS_ON_01


# LOADING LIBS ------------------------------------------------------------
install.packages(c("tidyverse","dplyr","janitor"))
install.packages(c("jsonlite","readr"))
install.packages(c("leaflet"))
#library("dplyr","janitor","jsonlite")
library(dplyr)
library(readr)
library(janitor)
library(tidyverse)
library(leaflet)
#library(openxlsx)


# LOADING DATA ------------------------------------------------------------
exp_22169651 <- jsonlite::fromJSON("https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/")


# SHORTCUTS ---------------------------------------------------------------

# limpiar la consola = CONTROL + l
# %>%  = SHIFT + CONTROL + M
# Control + enter = run line
# shift control R --> AÑADIR NUEVO BLOQUE 

#git add .
#git commit -m "mensaje"
#git push origin nombre_rama


# GIT COMMANDS ------------------------------------------------------------

# pwd = current location
# git status = info about a repo
# git commit = Add a comment
# git add . = Add the current dir to the entire repo
# git push -u origin main = send to the remote repo (Github)


# CLI COMMANDS ------------------------------------------------------------

# pwd = shows the current dir
# ls = list terminal 
# mkdir = create a dir
# cd = change dir
# clear = limpiar terminal 


# BASIC INSTRUCTIONS ------------------------------------------------------
#operators
# and = & or ,
# or = |
isa <- 8 # assigning values


# TIDYVERSE COMMANDS ------------------------------------------------------


# 27 SEPTIEMBRE  ----------------------------------------------------------


str(exp_22169651) #get datatype
df <- exp_22169651$ListaEESSPrecio #get readable data  
df %>% dplyr::glimpse()

df %>% janitor::clean_names() %>% glimpse() #te quita los espacios 


# working W pipes (OPT. MODE) ---------------------------------------------

cd <- df %>% janitor::clean_names() %>% glimpse() #te quita los espacios 

clean_data <- df %>% readr::type_convert(locale = readr::locale(decimal_mark= ",")) %>% janitor::clean_names() %>% as_tibble()

clean_data %>% glimpse()


# DEALING W DATA ----------------------------------------------------------
#COMO SE FILTRA
#villa_gas <- clean_data %>% select(precio_gasoleo_a,rotulo,direccion,localidad) %>% 
  #(localidad=="VILLAVICIOSA DE ODON") glimpse()

villa_boa <- clean_data %>% select(precio_gasoleo_a,rotulo,direccion,localidad) %>%  
  filter(localidad=="VILLAVICIOSA DE ODON"|localidad=="BOADILLA DEL MONTE") %>% 
  arrange(precio_gasoleo_a) %>% glimpse()
  
  

madrid_gas <- clean_data %>% select(precio_gasoleo_a,rotulo,direccion,localidad,provincia) %>%  
  filter(provincia=="MADRID") %>% 
  arrange(precio_gasoleo_a) %>% glimpse()

# STORING DATA ------------------------------------------------------------

#INFORME_MADRID <- write.csv(madrid_gas,"madrid_gas.csv")
#archivo <- xlsx::write.xlsx(madrid_gas,"madrid_gas.xlsx")

write_excel_csv2(madrid_gas,"madrid_gas.xlsx")


# GENERANDO REPORTES ------------------------------------------------------
#TODAS LAS ESTACIOES DE LA COMUNIDAD DE MADRID EL GASOLEO
#ORDENADO DESCENDIENTE 

gas_mad_1_55 <-clean_data %>%  select(rotulo,precio_gasoleo_a,direccion,provincia,municipio,latitud,longitud_wgs84) %>% 
  filter(provincia == "MADRID" & precio_gasoleo_a<1.55) %>% 
  arrange(desc(precio_gasoleo_a)) %>% write_excel_csv2("gas_mad_1_55.xls")

gas_mad_1_55 %>% leaflet() %>% addTiles() %>% 
  addCircleMarkers(lat = ~latitud, lng = ~longitud_wgs84, popup = ~rotulo, label =~precio_gasoleo_a) 
#~ sirve para decir que lo que estas asignando está 100% bien 
# popup --> si pincho nos sale el rotulo
# label --> si paso el raton por encima nos sale el precio 
#addtiles es una base que no tiene nada


# MÁS FILTROS -------------------------------------------------------------

BALLENOIL_MADRID <- clean_data %>% select(precio_gasoleo_a,rotulo,direccion,localidad,municipio,provincia,latitud,longitud_wgs84) %>%  
  filter(provincia =="MADRID" & rotulo=="BALLENOIL") %>% 
  arrange(precio_gasoleo_a) %>% glimpse()

BALLENOIL_MADRID %>% leaflet() %>% addTiles() %>% 
  addCircleMarkers(lat = ~latitud, lng = ~longitud_wgs84, popup = ~rotulo, label =~precio_gasoleo_a) 


#CLASIFICAR QUE GASOLINERAS SON LOWCOST Y CUÁLES NO  
# DEALING W COLS ----------------------------------------------------------
#crear columnas
LOW_COST <- clean_data %>% mutate(low_cost = !rotulo %in% c("REPSOL","CEPSA","Q8","BP","SHELL","CAMPSA","GALP"))
#modificar o crear un nuevo atributo -> mutate 
