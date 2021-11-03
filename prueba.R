library(tidyverse)
library(ggplot2)          #Graficos
library(plotly)           #Interaccion graficos
library(haven)
library(srvyr)
library(survey)
library(DT)
library(rgdal)
library(maptools)
library(readxl)
gpclibPermit()


# modifico el ecoding
# use_incov = TURE, y encoding='latin1', quiere decir: 
# agarra el archivo que esta en 'latin1' y ponelo en el encoding 
# de la maquina local (eso entiendo de la ayuda)

shape <- readOGR(dsn = ".", layer = "c004Polygon", encoding = "latin1",  use_iconv = TRUE)
shape@data$id <- rownames(shape@data)

# agrego esto para que sean solo mayuscula la primer letra,
# la 'y' queda mal, la arreglao a mano
library(stringr)
shape$popup <- str_to_title(shape$popup) %>% 
  gsub("Treinta Y Tres", "Treinta y Tres", x=.)

# create a data.frame from our spatial object
df <- fortify(shape, region = "id")
datosDF<- merge(df, shape@data, by = "id")
mapa_uru<-datosDF%>%
  dplyr::filter(popup != "Límite Contestado")





funcion1<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorA)
  
}

funcion1inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("dpto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorInf)
  
}

funcion1sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(dpto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(dpto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("dpto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$dpto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("dpto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  return(IndicadorSup)
  
}

funcion1_nacho <- function(diseno, filtro, variable) {
  Indicador<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26) %>%
    summarise(estimacion= survey_mean(eval(parse(text=variable)))) %>% 
    mutate(dpto = as_factor(dpto), e26=as_factor(e26)) 
  
  # totales por departamento
  totales<-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion = survey_mean(eval(parse(text=variable)))) %>% 
    mutate(e26 = 'Total', dpto = as_factor(dpto))
  
  # agrupo solo por sexo para Total pais por sexo
  totalesb <-diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))  %>% 
    mutate(dpto = 'Total país', e26=as_factor(e26)) 
  
  # total pais
  total <- diseno%>%
    dplyr::filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable)))) %>% 
    mutate(dpto = 'Total país', e26= 'Total') 
  
  bind_rows( list(Indicador,totales,totalesb,total) ) %>% 
    ungroup() %>% 
    mutate(inferior = pmax(0, estimacion-1.96*estimacion_se) ,
           superior=estimacion + 1.96*estimacion_se)
}



# P_2019_Terceros <- read_sav("C:/Users/Usuario/Desktop/ech2019/P_2019_Terceros.sav")
ech <- read_sav('ech2019/P_2019_Terceros.sav')

ech<- ech %>%
  mutate(e_0a14d =  ifelse(e27 <= 14, 1, 0))%>%
  mutate(e_15a24d = ifelse(e27>= 15 & e27 <= 24, 1, 0))%>%
  mutate(e_25a44d = ifelse(e27>= 25 & e27 <= 44, 1, 0))%>%
  mutate(e_45a64d = ifelse(e27>= 45 & e27 <= 64, 1, 0))%>%
  mutate(e_mas65d = ifelse(e27>= 65, 1, 0))%>%
  mutate(e_0a14 =  ifelse(e27 <= 14, 1, 0))%>%
  mutate(e_15a24 = ifelse(e27>= 15 & e27 <= 24, 2, 0))%>%
  mutate(e_25a44 = ifelse(e27>= 25 & e27 <= 44, 3, 0))%>%
  mutate(e_45a64 = ifelse(e27>= 45 & e27 <= 64, 4, 0))%>%
  mutate(e_mas65 = ifelse(e27>= 65, 5, 0))%>%
  mutate(edad_tramos = as.factor(e_0a14+e_15a24+e_25a44+e_45a64+e_mas65))%>%
  mutate(estudios_terciarios = ifelse(e215 == 1 | e215 == 2 | e218 == 1 | e218 == 2 | e221 == 1 | e221 == 2 | e224 == 1 | e224 == 2, 1,0))%>%
  mutate(activo=ifelse(pobpcoac==2 | pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
  mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
  mutate(asist= ifelse(e193 != 1 & e197 != 1 & e201 != 1 & e212 != 1 & e215 != 1 & e218 != 1 & e221 != 1 & e224 != 1, 1,0))%>%
  mutate(nini = ifelse(pobpcoac !=2 & asist==1, 1,0))%>%
  mutate(e51_2_rec = recode(e51_2, "9"=0))%>%
  mutate(e51_4_rec = recode(e51_4, "9"=0))%>%
  mutate(e51_5_rec = recode(e51_5, "9"=0))%>%
  mutate(e51_6_rec = recode(e51_6, "9"=0))%>%
  mutate(e51_7_rec = recode(e51_7, "9"=0))%>%
  mutate(e51_8_rec = recode(e51_8, "9"=0))%>%
  mutate(e51_9_rec = recode(e51_9, "9"=0))%>%
  mutate(e51_10_rec = recode(e51_10, "9"=0))%>%
  mutate(e51_11_rec = recode(e51_11, "9"=0))%>%
  mutate(a_estudio= e51_2_rec + e51_4_rec + e51_5_rec + e51_6_rec + e51_8_rec + e51_9_rec + e51_10_rec + e51_11_rec)%>%
  mutate(edmedia = ifelse((e201==1 | e212==1) & e51_7_1!=1 & (e215!=1 & e218!=1 & e221!=1 & e224!=1), 1,0))%>%
  mutate(secundaria_utu = ifelse(e51_7 == 3 & e51_7_1 == 2, 1,0))%>%
  mutate(secundaria_utu2 = ifelse(e51_5 == 3 | secundaria_utu == 1, 1,0))%>%
  mutate(adol_estudios= ifelse(e197 == 1 | e201 == 1 | e212 == 1, 1,0))%>%
  mutate(nv0 = ifelse((e51_2==9 |e51_2==0) & (e51_3==0 | e51_3==9) & e51_4==0 & e51_5==0 & e51_6==0 & e51_7==0 & e51_8==0 & e51_9==0 & e51_10==0 & e51_11==0, 0,1))%>%
  mutate(nv2= ifelse(((e51_4>0 & e51_4<9) | (e51_7 > 0 & e51_7<9 & (e51_7_1==3 | e51_7_1==2))) & (e51_8==0 | e51_8==9) & (e51_9==0 | e51_9==9) & (e51_10==0 | e51_10==9), 2,0))%>%
  mutate(nv1 = ifelse(((e51_2>0 & e51_2<9) | (e51_3>0 & e51_3<9)|(e51_7>0 & e51_7<9 & e51_7_1==4)) & (e51_4==0| e51_4==9) & nv2==0, 1,0))%>%
  mutate(nv3=ifelse(((e51_10>0 & e51_10<9) | (e51_7>0 & e51_7<9 & e51_7_1==1)) & (e51_9==0 | e51_9==9 )& (e51_11==0 | e51_11==9), 3,0))%>%
  mutate(nv4=ifelse((e51_8>0 & e51_8<9) & (e51_9==0 | e51_9==9) & (e51_10==0 | e51_10==9) & (e51_11==0 | e51_11==9), 4,0))%>%
  mutate(nv5=ifelse(((e51_9 > 0 & e51_9 < 9 ) | (e51_11>0 & e51_11<9)), 5,0))%>%
  mutate(niveledu= ifelse(nv0 ==0, 0, nv1+nv2+nv3+nv4+nv5))%>%
  mutate(e236a=as.numeric(e236))%>%
  mutate(e236_mod= recode(e236a, "0"=1))%>%
  mutate(e_14a29 = ifelse(e27>= 14 & e27 <= 29, 1, 0))%>%
  mutate(e_30a49 = ifelse(e27>= 30 & e27 <= 49, 2, 0))%>%
  mutate(e_50a64 = ifelse(e27>= 50 & e27 <= 64, 3, 0))%>%
  mutate(ed_mas65 = ifelse(e27>= 65, 4, 0))%>%
  mutate(edad_tramos_ml = e_14a29+e_30a49+e_50a64+ed_mas65)%>%
  mutate(e6a11=ifelse(e27>=6 & e27<=11,1,0))%>%
  mutate(e3a5=ifelse(e27>=3 & e27<=5,1,0))%>%
  mutate(e12a17=ifelse(e27>=12 & e27<=17,1,0))



pr<-ech%>% as_survey_design(ids=numero, weight=pesoano)

i741<- funcion1(pr, "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")
i741Inf<- funcion1inf(pr, "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")
i741Sup<- funcion1sup(pr, "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")

i741.nacho <- funcion1_nacho(pr, "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")

#mapa

m<-left_join(mapa_uru, i741, by = c("popup" = "dpto"))%>%
  ggplot(aes(x=long, y=lat, group = group,
             fill = Total)) +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  coord_equal() +
  theme(title = element_blank(),
        axis.text = element_blank())

# usando i741.nacho
m <- i741.nacho %>% filter(e26 == 'Total') %>% 
  right_join( mapa_uru, by = c("dpto" = "popup") ) %>%
  ggplot(aes(x=long, y=lat, group = group,
             fill = estimacion)) +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
  coord_equal() +
  theme(title = element_blank(),
        axis.text = element_blank())

#ggplotly(m)

# ic
tabla<-i741 %>% ungroup() %>% 
      gather(key= 'Sexo', value= 'Valor', -dpto)

tablainf<-i741Inf %>% ungroup() %>% 
  gather(key= 'Sexo', value= 'Valor', -dpto)
tablasup<-i741Sup %>% ungroup() %>% 
  gather(key= 'Sexo', value= 'Valor', -dpto)

tabla<- left_join(tabla, tablainf)
tabla<- left_join(tabla, tablasup)

h<- tabla %>% filter(Sexo== "Total") %>% 
    ggplot(aes(reorder(dpto,Valor) ,Valor, color= Valor))+ geom_pointrange(aes(ymin = inferior, ymax = superior))+
    coord_flip()+ scale_color_gradient(low = "#56B1F7", high = "#132B43")+
    labs(x= "Departamento")+
    theme(axis.text.y = element_text(face="bold"))+
    theme (axis.title = element_text(face="bold"))
  
#ggplotly(h)

# usando i741.nacho
i741.nacho %>% 
  filter(e26 == 'Total') %>% 
  ggplot( aes(reorder(dpto,estimacion), estimacion, color= estimacion) ) +
  geom_pointrange(aes(ymin = inferior, ymax = superior))+
  coord_flip()+ scale_color_gradient(low = "#56B1F7", high = "#132B43") + 
  labs(x= "Departamento")+
  theme(axis.text.y = element_text(face="bold"))+
  theme (axis.title = element_text(face="bold"))

  
  
  