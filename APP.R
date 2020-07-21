library(shiny)            #App
library(shinydashboard)   #Para tableros
library(tidyverse)
library(ggplot2)          #Graficos
library(plotly)           #Interaccion graficos
library(haven)
library(srvyr)
library(survey)
library(DT)
library(rgdal)
library(maptools)
gpclibPermit()
options(shiny.maxRequestSize = 300 * 1024^2)


shape <- readOGR(dsn = ".", layer = "c004Polygon")
shape@data$id <- rownames(shape@data)
# create a data.frame from our spatial object
df <- fortify(shape, region = "id")
datosDF<- merge(df, shape@data, by = "id")
mapa_uru<-datosDF%>%
  filter(popup != "LÍMITE CONTESTADO")
mapa_uru$popup<- recode(mapa_uru$popup,"ARTIGAS"="Artigas","CANELONES"="Canelones","CERRO LARGO"="Cerro Largo","COLONIA"="Colonia","DURAZNO"="Durazno","FLORES"="Flores","FLORIDA"="Florida","LAVALLEJA"="Lavalleja","MALDONADO"="Maldonado",
                        "MONTEVIDEO"="Montevideo","PAYSANDÚ"="Paysandú","RÍO NEGRO"="Río Negro","RIVERA"="Rivera","ROCHA"="Rocha","SALTO"="Salto","SAN JOSÉ"="San José","SORIANO"="Soriano","TACUAREMBÓ"="Tacuarembó","TREINTA Y TRES"="Treinta y Tres")


#####Funciones######


funcion1<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("depto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorA)
  
}

funcion2<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  return(Ind0)
  
}

funcion3<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  
  return(IndicadorA)
}

funcion4<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:7] -(1.96*Indicadorb[,2:7])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  sup[sup>1]<-1
  
  
  return(IndicadorA)
  
}

funcion5<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  sup[sup>1]<-1
  
  return(IndicadorA)
  
  
}

funcion6<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("depto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorA)
}

funcion7<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(ind)
  
}

funcion8<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total - (1.96*desv$Total_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(ind)
  
  
}

funcion9<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  inf[inf<0]<-0
  
  return(Ind0)
  
  
}


funcion1inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("depto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ),
           Hombre= ifelse(Hombre<=0,0,Hombre),
           Mujer= ifelse(Mujer<=0,0,Mujer),
           Total= ifelse(Total<=0,0,Total))
  
  return(IndicadorInf)
  
}

funcion1sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"),
           Hombre= ifelse(Hombre>=1,1,Hombre),
           Mujer= ifelse(Mujer>=1,1,Mujer),
           Total= ifelse(Total>=1,1,Total))

  
  return(IndicadorSup)
  
}

funcion2inf<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  inf<- inf%>%
    select(auxe)
  
  sup<- sup%>%
    select(auxe)
  
  inf[inf<0]<-0
  
  
  return(inf)
  
}

funcion2sup<-function(diseno, filtro, variable, a1, b)  {
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  nombres<-c("00_dpto")
  tot0<-departamentos
  desvios0<-departamentos
  l<-length(b)
  a2<-paste0(a1,"_se")
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
             group_by(dpto,e26)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion))%>%
             spread(e26, estimacion))
    
    totalesb<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      group_by(e26)%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesbi<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot))%>%
      spread(sexo, estimacion.tot)
    
    
    totalesbi<-cbind("Total País",totalesbi)
    colnames(totalesbi)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalesbi))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,e26,estimacion_se))%>%
             spread(e26, estimacion_se))
    
    totalesse<-totalesb%>%
      mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
      select(c(sexo,estimacion.tot_se))%>%
      spread(sexo, estimacion.tot_se)
    
    
    totalesse<-cbind("Total País",totalesse)
    colnames(totalesse)<-c("dpto", "Hombre", "Mujer")
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
    
    
    nombres<-cbind(nombres, paste("Hombre", a1[i], sep="_"), paste("Mujer", a1[i], sep="_"))        
    
    
    tot<- paste("Total", i, sep="")
    assign(tot, diseno%>%
             filter(eval(parse(text=filtro)))%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    totales2<- paste("Tot", i, sep="")
    assign(totales2, eval(parse(text=tot))%>%
             select(c(dpto,estimacion)))
    
    totalesb2<-diseno%>%
      filter(eval(parse(text=filtro)))%>%
      summarise(estimacion.tot= survey_mean(eval(parse(text=variable))==b[i]))
    
    totalesb2<-cbind("Total País",totalesb2)
    colnames(totalesb2)<-c("dpto", "estimacion", "estimacion_se")
    
    totalesbb2<-totalesb2%>%
      select(c(dpto, estimacion))
    
    assign(totales2,rbind(eval(parse(text=totales2)),totalesbb2))
    
    tot0<-left_join(tot0, eval(parse(text=totales2)), by="dpto")
    
    
    totales3<-paste("desvios", i, sep="")
    assign(totales3, eval(parse(text=tot))%>%
             select(c(dpto,estimacion_se)))
    
    totales2se<-totalesb2%>%
      select(c(dpto, estimacion_se))
    
    assign(totales3,rbind(eval(parse(text=totales3)),totales2se))
    
    
    desvios0<-left_join(desvios0, eval(parse(text=totales3)), by="dpto")
    
    
    
    
  }
  colnames(tot0)<-c("00_dpto",paste0("Total_",a1))
  colnames(desvios0)<-c("00_dpto",paste0("Total_",a2))
  colnames(Ind0)<-nombres  
  colnames(desv0)<-nombres  
  
  Ind0<- left_join(Ind0, tot0)
  desv0<-left_join(desv0, desvios0)
  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  auxe<- order(colnames(Ind0)) 
  
  
  Ind0<- Ind0%>%
    select(auxe)
  
  desv0<- desv0%>%
    select(auxe)
  
  inf<- inf%>%
    select(auxe)
  
  sup<- sup%>%
    select(auxe)
  
  return(sup)
  
}

funcion3inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  
  
  
  return(inf)
}

funcion3sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  return(sup)
}

funcion4inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:7] -(1.96*Indicadorb[,2:7])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0

  
  return(inf)
  
}

funcion4sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion))%>%
    spread(edad_tramos, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot))%>%
    spread(edad_tramos, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","0a14", "15a24", "25a44", "45a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "1"="0a14","2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(depto,edad_tramos,estimacion_se))%>%
    spread(edad_tramos, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos= recode(as.factor(edad_tramos), "2"= "15a24", "3"= "25a44", "4"= "45a64", "5"="65 y más"))%>%
    select(c(edad_tramos,estimacion.tot_se))%>%
    spread(edad_tramos, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "0a14","15a24", "25a44", "45a64", "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:7] +(1.96*Indicadorb[,2:7])
  
  sup<- cbind(departamentos, sup)
  
  
  return(sup)
  
}

funcion5inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
  
  ##inferior
  inf<- IndicadorA[,2:6] -(1.96*Indicadorb[,2:6])
  
  inf<- cbind(departamentos, inf) 
  
  inf[inf<0]<-0
  
  
  return(inf)
  
  
}

funcion5sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto, edad_tramos_ml)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion))%>%
    spread(edad_tramos_ml, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(edad_tramos_ml)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot))%>%
    spread(edad_tramos_ml, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto","14a29", "30a49",  "50a64", "65 y más", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(depto,edad_tramos_ml,estimacion_se))%>%
    spread(edad_tramos_ml, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(edad_tramos_ml= recode(as.factor(edad_tramos_ml), "1"="14a29","2"= "30a49", "3"= "50a64", "4"= "65 y más"))%>%
    select(c(edad_tramos_ml,estimacion.tot_se))%>%
    spread(edad_tramos_ml, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "14a29", "30a49",  "50a64",  "65 y más", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  ##superior
  sup<- IndicadorA[,2:6] +(1.96*Indicadorb[,2:6])
  
  sup<- cbind(departamentos, sup)
  
 
  return(sup)
  
  
}

funcion6inf<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("depto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorInf)
}

funcion6sup<-function(diseno, filtro, variable)  {
  
  Indicador<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto,e26)%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  ## separo en 2 tablas, una con las estimaciones puntuales y otra con los desvíos
  IndicadorA<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion))%>%
    spread(sexo, estimacion)
  
  ## agrupo solo por dpto para tener los totales sin la desagregacion por sexo
  totales<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(dpto)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  ## también separo en 2 tablas para despues pegarlos con las anteriores
  totalesi<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot))
  
  ## pego
  IndicadorA<-cbind(IndicadorA,Total=(totalesi$estimacion.tot))
  
  #agrupo solo por sexo para los totales
  totalesb<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    group_by(e26)%>%
    summarise(estimacion.tot= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot))%>%
    spread(sexo, estimacion.tot)
  
  total<-diseno%>%
    filter(eval(parse(text=filtro)))%>%
    summarise(estimacion= survey_mean(eval(parse(text=variable))))
  
  totalesbi<-cbind("Total País",totalesbi, total[1])
  colnames(totalesbi)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorA<-rbind(IndicadorA,totalesbi)
  ## guardo la tabla en un excel
  
  ## Lo mismo para la tabla de desvíos
  Indicadorb<-Indicador%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(depto,sexo,estimacion_se))%>%
    spread(sexo, estimacion_se)
  
  totalesse<-totales%>%
    mutate(depto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(depto,estimacion.tot_se))
  
  Indicadorb<-cbind(Indicadorb,Total=(totalesse$estimacion.tot_se))
  
  #agrupo solo por sexo para los totales
  
  totalesbd<-totalesb%>%
    mutate(sexo= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))%>%
    select(c(sexo,estimacion.tot_se))%>%
    spread(sexo, estimacion.tot_se)
  
  totalesbd<-cbind("Total País",totalesbd, total[2])
  colnames(totalesbd)<-c("depto", "Hombre", "Mujer", "Total")
  
  Indicadorb<-rbind(Indicadorb,totalesbd)
  
  
  
  
  IndicadorSup<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre+(1.96*Indicadorb$Hombre), IndicadorA$Mujer+(1.96*Indicadorb$Mujer), IndicadorA$Total+(1.96*Indicadorb$Total)))
  colnames(IndicadorSup)<-c("depto", "Hombre", "Mujer", "Total")
  
  IndicadorSup<-IndicadorSup%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País"))
  
  
  
  
  IndicadorInf<- as.data.frame(cbind(IndicadorA$depto, IndicadorA$Hombre-(1.96*Indicadorb$Hombre), IndicadorA$Mujer-(1.96*Indicadorb$Mujer), IndicadorA$Total-(1.96*Indicadorb$Total)))
  colnames(IndicadorInf)<-c("depto", "Hombre", "Mujer", "Total")
  IndicadorInf<-IndicadorInf%>%
    mutate(depto= recode(as.factor(depto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                         "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(IndicadorSup)
}

funcion7inf<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(inf)
  
}

funcion7sup<- function(diseno, var1, var2) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1))
  
  total<-diseno%>%
    summarise(tasa=survey_ratio(eval(parse(text=var1))==1, eval(parse(text=var2))==1)) 
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa + (1.96*desv$tasa_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, tasa=ind$tasa - (1.96*desv$tasa_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(sup)
  
}

funcion8inf<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  inf<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total - (1.96*desv$Total_se)))   
  inf<-inf%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres","20"="Total País" ))
  
  return(inf)
  
  
}

funcion8sup<- function(diseno, variable) {
  
  tabla1<-diseno%>%
    group_by(dpto)%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-diseno%>%
    summarise(Total=survey_mean(eval(parse(text=variable))))
  
  total<-cbind(dpto= "Total País", total)
  
  ind<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,2))
  
  ind<-as.data.frame(rbind(ind, total[1:2]))
  
  desv<- tabla1%>%
    mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                        "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
    select(c(1,3))
  
  desv<-as.data.frame(rbind(desv, total[-2]))
  
  sup<-as.data.frame(cbind(dpto=ind$dpto, Total=ind$Total + (1.96*desv$Total_se)))   
  sup<-sup%>%mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres", "20"="Total País" ))
  
  return(sup)
  
  
}

funcion9inf<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  inf[inf<0]<-0
  
  return(inf)
  
  
}

funcion9sup<-function(diseno, variable, a1, b) {
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  Ind0<-departamentos
  desv0<- departamentos
  l<-length(b)
  
  
  for (i in 1:l) {
    nam<- paste("Indicador", i, sep="")
    assign(nam, diseno%>%
             mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                                 "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
             group_by(dpto)%>%
             summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
           
    )
    nam2<- paste("Ind", i, sep="")
    assign(nam2, eval(parse(text=nam))%>%
             select(c(dpto,estimacion)))
    
    total<-diseno%>%
      summarise(estimacion= survey_mean(eval(parse(text=variable))==b[i]))
    
    total<-as.data.frame(cbind(dpto= "Total País", total))
    
    totalind<-total%>%
      select(c(dpto,estimacion))
    
    
    assign(nam2,rbind(eval(parse(text=nam2)),totalind))
    
    Ind0<-left_join(Ind0, eval(parse(text=nam2)), by="dpto")
    
    
    nam3<-paste("desv", i, sep="")
    assign(nam3, eval(parse(text=nam))%>%
             select(c(dpto,estimacion_se)))
    
    totalesse<-total%>%
      select(c(dpto,estimacion_se))
    
    
    assign(nam3,rbind(eval(parse(text=nam3)),totalesse))
    
    
    desv0<-left_join(desv0, eval(parse(text=nam3)), by="dpto")
    
    
  }
  
  colnames(Ind0)<- a1  
  colnames(desv0)<-a1  
  
  
  sup<-Ind0[,-1]+ (1.96*desv0[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-Ind0[,-1]- (1.96*desv0[,-1])
  inf<-cbind(departamentos, inf)
  
  colnames(sup)<- a1  
  colnames(inf)<-a1  
  
  
  return(sup)
  
  
}

##########
## ui.R ##
##########

#### Encabezado ####


encabezado <- dashboardHeader(title = "Trabajo final",
                              tags$li(
                                a(
                                  strong("Buscar ECH"),
                                  height = 40,
                                  href = "https://http://www.ine.gub.uy/encuesta-continua-de-hogares1",
                                  title = "",
                                  target = "_blank"
                                ),
                                class = "dropdown"
                              ))

#### Barra Lateral ####

barra_lateral <- dashboardSidebar(
  
  
  
  sidebarMenu(
    menuItem("Inicio", tabName = "inicio", icon = icon("home")),
    menuItem("Cargar ECH", tabName = "cargadatos", icon = icon("upload"),
             
             #Carga de datos      
             fileInput("datos", "Base de personas .sav", multiple = TRUE,
                       accept = c(".sav", ".RData")),
             fileInput("datosH", "Base de hogares .sav", multiple = TRUE,
                       accept = c(".sav", ".RData")),
             selectInput("dis", "Diseño", choices = c("publica", "power"))),
    
    menuItem("Indicadores", icon = icon("chart-line"),
             menuSubItem("Educación", tabName = "educ", icon = icon("mortar-board")),
             menuSubItem("Salud", tabName = "Salud", icon = icon("ambulance")),
             menuSubItem("Mercado laboral", tabName = "laboral", icon = icon("wrench")),
             menuSubItem("Ingresos y bienestar", tabName = "ingresos", icon = icon("dollar-sign")),
             menuSubItem("Tecnología y comunicación", tabName = "tec", icon = icon("laptop")),
             menuSubItem("Demografía", tabName = "demo", icon = icon("globe-americas")),
             menuSubItem("Viviendas y hogares", tabName = "hogares", icon = icon("home"))
             
    )
    
  )
)

#### Contenido ####

contenido <- dashboardBody(
  
  
  tabItems(
    tabItem(tabName = "inicio", h1("INTRO")),
    tabItem(tabName = "educ", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow( 
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre", "Indicador", choices= c("Población entre 25 y 65 años con estudios terciarios por sexo"="741",
                                                                                                                                                 "Tasa neta de asistencia de 3 a 5 años en educación preescolar por sexo"="751",
                                                                                                                                                 "Tasa neta de asistencia de 12 a 17 años en educación media por sexo" ="1807",
                                                                                                                                                 "Personas de 18 años o más que completaron segundo ciclo de educación media por sexo"="740", 
                                                                                                                                                 "Tasa neta de asistencia de 0 a 2 años en educación inicial por sexo"="748", 
                                                                                                                                                 "Adolescentes de 12 a 17 años que asisten a establecimientos educativos por sexo"="732",
                                                                                                                                                 "Tasa neta de asistencia de 6 a 11 años en educación escolar por sexo"="756", 
                                                                                                                                                 "Personas mayores de 15 años que completaron media básica general o técnica por sexo"="739", 
                                                                                                                                                 "Tasa de analfabetismo en población de 15 años o más por sexo"="689", 
                                                                                                                                                 "Jovenes de 14 a 24 años que no estudian ni trabajan por sexo"="690",
                                                                                                                                                 "Personas mayores a 25 años por máximo nivel educativo alcanzado"="696",
                                                                                                                                                 "Ocupados por máximo nivel educativo alcanzado"="725",
                                                                                                                                                 "Tasa de analfabetismo de la población de 15 años o más por tramos de edad"="1020",
                                                                                                                                                 "Promedio de años de educación de las personas de 25 años o más por sexo"="1380",
                                                                                                                                                 "Tasa bruta de asistencia de 6 a 11 años a educación primaria"="747",
                                                                                                                                                 "Tasa bruta de asistencia de 3 a 5 años a educación preescolar"="746",
                                                                                                                                                 "Tasa bruta de asistencia de 12 a 17 años a educación media"="1808"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionEdu")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_edu",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_edu",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos1"), fluidRow(p(class = 'text-center', downloadButton('dd1', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfEdu")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupEdu")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "Salud", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(   
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre2", "Indicador", choices= c("Personas afiliadas a emergencias móviles por sexo"="501",
                                                                                                                                                  "Personas afiliadas a emergencias móviles por tramos de edad"="529",
                                                                                                                                                  "Personas por tipo de atención en salud"="517"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionSalud")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_salud",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_salud",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos2"), fluidRow(p(class = 'text-center', downloadButton('dd2', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfSalud")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupSalud")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "laboral", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre3", "Indicador", choices= c("Tasa de empleo por sexo"="533",
                                                                                                                                                  "Subempleo por sexo"="521",
                                                                                                                                                  "Informalidad por sexo"="526",
                                                                                                                                                  "Ocupados en establecimientos fuera del dpto por sexo"="618",
                                                                                                                                                  "Tasa de actividad por sexo"="608",
                                                                                                                                                  "Jovenes de 14 a 24 años que no estudian ni trabajan por sexo"="690",
                                                                                                                                                  "Tasa de desempleo por sexo"="502",
                                                                                                                                                  "Desocupados por última ocupación"="609",
                                                                                                                                                  "Ocupados por categoría de ocupación"="611",
                                                                                                                                                  "Tasa de desempleo por tramos de edad"="531",
                                                                                                                                                  "Tasa de empleo por tramos de edad"="534",
                                                                                                                                                  "Tasa de actividad por tramos de edad"="607",
                                                                                                                                                  "Población por condición de actividad por sexo"="610"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionLab")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_lab",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_lab",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos3"), fluidRow(p(class = 'text-center', downloadButton('dd3', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfLab")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupLab")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "ingresos", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre4", "Indicador", choices= c("Informalidad por sexo"="526",
                                                                                                                                                  "Personas en hogares en situación de pobreza por sexo"="568",
                                                                                                                                                  "Jovenes entre 14 y 29 años en situación de pobreza por sexo"="1636",
                                                                                                                                                  "Tasa de desempleo por tramos de edad"="531",
                                                                                                                                                  "Tasa de empleo por tramos de edad"="534",
                                                                                                                                                  "Hogares con hacinamiento"="577",
                                                                                                                                                  "Hogares con al menos un auto o camioneta"="1929",
                                                                                                                                                  "Hogares en situación de pobreza"="553"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionIng")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_ing",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_ing",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos4"), fluidRow(p(class = 'text-center', downloadButton('dd4', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfIng")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupIng")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "tec", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre5", "Indicador", choices= c("Personas que utilizan internet por sexo"="605",
                                                                                                                                                  "Personas con celular por sexo"="581",
                                                                                                                                                  "Utilización de computadora el último mes por sexo"="603",
                                                                                                                                                  "Frecuencia de utilización de internet por sexo"="582",
                                                                                                                                                  "Hogares con conexión a internet"="591",
                                                                                                                                                  "Hogares con computadora o laptop"="594",
                                                                                                                                                  "Hogares con al menos una computadora del Plan Ceibal"="584"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionTec")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_tec",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_tec",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos5"), fluidRow(p(class = 'text-center', downloadButton('dd5', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfTec")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupTec")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "demo", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre6", "Indicador", choices= c("Población por lugar de residencia hace 5 años"="678",
                                                                                                                                                  "Población por lugar de nacimiento"="654",
                                                                                                                                                  "Población por lugar de residencia anterior"="655"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , uiOutput("SeleccionDemo")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_demo",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_demo",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800)
                         )),
              tabPanel("Tabla", DTOutput("datos6"), fluidRow(p(class = 'text-center', downloadButton('dd6', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfDemo")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupDemo")
                             
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "hogares", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre7", "Indicador", choices= c("Hogares con hacinamiento"="577",
                                                                                                                                                  "Hogares en situación de pobreza"="553",
                                                                                                                                                  "Hogares por tipo de evacuación del sistema sanitario"="764",
                                                                                                                                                  "Hogares por fuente de energía para iluminar"="782",
                                                                                                                                                  "Hogares por origen del agua para beber y cocinar"="783",
                                                                                                                                                  "Hogares por presencia de lugar para cocinar"="765",
                                                                                                                                                  "Hogares por presencia y uso de baño"="766",
                                                                                                                                                  "Hogares por tipo de relación con la vivienda"="774"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE ,uiOutput("SeleccionHog")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_hog",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_hog",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800)
                         )),
              tabPanel("Tabla", DTOutput("datos7"), fluidRow(p(class = 'text-center', downloadButton('dd7', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfHog")
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupHog")
                             
                           )
                         )
                         
                       ))
            )
    )
  )
)

## UI - Interfaz del Usuario ##

ui <- dashboardPage(skin = "green", encabezado, barra_lateral, contenido)


##############
## server.R ##
##############

server <- function(input, output){
  
  ech <- reactive({
    inFile <- input$datos
    
    if (is.null(inFile))
      return(NULL)
    
    read_sav(input$datos$datapath)
  })
  
  echHog <- reactive({
    inFile <- input$datosH
    
    if (is.null(inFile))
      return(NULL)
    
    read_sav(input$datosH$datapath)
  })
  
  
  pr<- reactive( { 
    
    echP<- ech() %>%
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
    
    if (input$dis == "publica") pr<-echP%>% as_survey_design(ids=numero, weight=pesoano)
    else pr<-echP%>% as_survey_design(ids=UPM_ID, weight=pesoano, strata=ESTRATO)    
    
    return(pr)
  })
  
  ph<- reactive( { 
    
    echH<- echHog() %>%
      mutate(propietarios= ifelse(d8_1==1|d8_1==2|d8_1==3|d8_1==4,1,0))%>%
      mutate(Oc_con_permiso= ifelse(d8_1==6|d8_1==7|d8_1==8,1,0))%>%
      mutate(hacinamiento= ifelse((d25/d9)>2,1,0))
    
    if (input$dis == "publica") ph<-echH%>% as_survey_design(ids=numero, weight=pesoano)
    else ph<-echH%>% as_survey_design(ids=UPM_ID, weight=pesoano, strata=ESTRATO)    
    
    return(ph)
  })
  
  i741<- reactive({funcion1(pr(), "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")})
  i751<- reactive({funcion1(pr(), "e27>=3 & e27<=5","e193==1")})
  i1807<-reactive({funcion1(pr(), "e27>=12 & e27<=17", "edmedia==1")})
  i740<- reactive({funcion1(pr(), "e27>=18", "secundaria_utu2==1")})
  i748<- reactive({funcion1(pr(), "e27<=2", "e238==1")})
  i732<- reactive({funcion1(pr(), "e27>=12 & e27<=17", "adol_estudios==1")})
  i756<- reactive({funcion1(pr(), "e27>=6 & e27<=11", "e197==1")})
  i739<- reactive({funcion1(pr(), "e27>=15", "e51_4==3")})
  i689<- reactive({funcion1(pr(), "e27>=15", "e48==2")})
  i690<- reactive({funcion1(pr(), "e27>=14 & e27<=24", "nini==1")})
  i696<- reactive({funcion2(pr(), "e27>=25", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i725<- reactive({funcion2(pr(), "pobpcoac == 2", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i1020<- reactive({funcion3(pr(), "e27>=15", "e48==2")})
  i1380<- reactive({funcion6(pr(), "e27>=25", "a_estudio")})
  i747<- reactive({funcion7(pr(), "e197", "e6a11")})
  i746<- reactive({funcion7(pr(), "e193", "e3a5")})
  i1808<-reactive({funcion7(pr(), "edmedia", "e12a17")})
  
  indicador_edu<- reactive({ if(input$Nombre == "741") i741()
    else if(input$Nombre == "751") i751()
    else if(input$Nombre == "1807") i1807()
    else if(input$Nombre == "740") i740()
    else if(input$Nombre == "748") i748()
    else if(input$Nombre == "732") i732()
    else if(input$Nombre == "756") i756()
    else if(input$Nombre == "739") i739()
    else if(input$Nombre == "689") i689()
    else if(input$Nombre == "690") i690()
    else if(input$Nombre == "696") i696()
    else if(input$Nombre == "725") i725()
    else if(input$Nombre == "1020") i1020()
    else if(input$Nombre == "1380") i1380()
    else if(input$Nombre == "747") i747()
    else if(input$Nombre == "746") i746()
    else if(input$Nombre == "1808") i1808()
  })
  
  i501<- reactive({funcion1(pr(),"1==1","e46==1")})
  i529<- reactive({funcion4(pr(), "1==1", "e46==1")})
  i517<- reactive({
    pta<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      group_by(dpto)%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    #total pais
    t<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    
    pta<-rbind(pta[,2:17], t)
    
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pta<-cbind(departamentos, pta)
    
    pta_ind<- pta%>%
      select(c(1,2,4,6,8,10,12,14,16))
    
    pta_desv<- pta%>%
      select(c(1,3,5,7,9,11,13,15,17))
    
    pta_sup<- as.data.frame(cbind(departamentos, pta_ind$Asse+(1.96*pta_desv$Asse_se), pta_ind$IAMC+(1.96*pta_desv$IAMC_se),
                                  pta_ind$Policial_militar+(1.96*pta_desv$Policial_militar_se),pta_ind$Seguro_privado+(1.96*pta_desv$Seguro_privado_se),pta_ind$BPS+(1.96*pta_desv$BPS_se),
                                  pta_ind$Pol_municipal+(1.96*pta_desv$Pol_municipal_se), pta_ind$Otro+(1.96*pta_desv$Otro_se), pta_ind$Sin_cobertura+(1.96*pta_desv$Sin_cobertura_se)))
    
    colnames(pta_sup)<- colnames(pta_ind)  
    
    pta_sup<- pta_sup%>%
      mutate(Asse= ifelse(Asse>=1,1,Asse),
             IAMC= ifelse(IAMC>=1,1,IAMC),
             Policial_militar= ifelse(Policial_militar>=1,1,Policial_militar),
             Seguro_privado= ifelse(Seguro_privado>=1,1,Seguro_privado),
             BPS= ifelse(BPS>=1,1,BPS),
             Pol_municipal= ifelse(Pol_municipal>=1,1,Pol_municipal),
             Otro= ifelse(Otro>=1,1,Otro),
             Sin_cobertura= ifelse(Sin_cobertura>=1,1,Sin_cobertura))
    
    
    pta_inf<- as.data.frame(cbind(departamentos, pta_ind$Asse-(1.96*pta_desv$Asse_se), pta_ind$IAMC-(1.96*pta_desv$IAMC_se),
                                  pta_ind$Policial_militar-(1.96*pta_desv$Policial_militar_se),pta_ind$Seguro_privado-(1.96*pta_desv$Seguro_privado_se),pta_ind$BPS-(1.96*pta_desv$BPS_se),
                                  pta_ind$Pol_municipal-(1.96*pta_desv$Pol_municipal_se), pta_ind$Otro-(1.96*pta_desv$Otro_se), pta_ind$Sin_cobertura-(1.96*pta_desv$Sin_cobertura_se)))
    
    colnames(pta_inf)<- colnames(pta_ind)  
    
    pta_inf<- pta_inf%>%
      mutate(Asse= ifelse(Asse<0,0,Asse),
             IAMC= ifelse(IAMC<0,0,IAMC),
             Policial_militar= ifelse(Policial_militar<0,0,Policial_militar),
             Seguro_privado= ifelse(Seguro_privado<0,0,Seguro_privado),
             BPS= ifelse(BPS<0,0,BPS),
             Pol_municipal= ifelse(Pol_municipal<0,0,Pol_municipal),
             Otro= ifelse(Otro<0,0,Otro),
             Sin_cobertura= ifelse(Sin_cobertura<0,0,Sin_cobertura))
    
    i517<- pta_ind
    return(i517)
    
  })
  
  indicador_salud<- reactive({ if(input$Nombre2 == "501") i501()
    else if(input$Nombre2 == "529") i529()
    else if(input$Nombre2 == "517") i517()
    
  })
  
  i533<- reactive({funcion1(pr(), "e27>13", "pobpcoac==2")})
  i521<- reactive({funcion1(pr(), "pobpcoac == 2", "subempleo==1")})
  i526<- reactive({funcion1(pr(), "e27>13 & pobpcoac==2", "f82==2")})
  i618<- reactive({funcion1(pr(), "pobpcoac==2", "f80==2")})
  i608<- reactive({funcion1(pr(), "e27>13", "activo==1")})
  i502<- reactive({funcion1(pr(), "e27>13 & activo==1", "desocupados==1")})  
  i609<- reactive({funcion2(pr(), "f116==1", "f121", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i611<- reactive({funcion2(pr(), "pobpcoac==2", "f73", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i531<- reactive({funcion5(pr(), "e27>13 & activo==1", "desocupados")})
  i607<- reactive({funcion5(pr(), "e27>13", "activo==1")})
  i534<- reactive({funcion5(pr(), "e27>13", "pobpcoac==2")})
  i610<- reactive({
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pca<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto,e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    pca<-pca%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    
    pca_totales<- pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    pca_totales_dpto<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))
    
    pca_total_pais<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    ###Hombre
    pca_hombres<- pca%>%
      filter(e26=="Hombre")
    
    pca_hombres_ind<- pca_hombres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_hombres_ind)[2:5]<-paste0("Hombre_",colnames(pca_hombres_ind)[2:5])
    
    pca_tot_hom_ind<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(2,4,6,8))
    
    pca_tot_hom_ind<-cbind(dpto= "Total País", pca_tot_hom_ind)
    
    colnames(pca_tot_hom_ind)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_ind)[2:5])
    
    
    pca_hombres_ind<-rbind(pca_hombres_ind, pca_tot_hom_ind)
    
    ##desvio
    
    pca_hombres_desv<- pca_hombres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_hombres_desv)[2:5]<-paste0("Hombre_",colnames(pca_hombres_desv)[2:5])
    
    pca_tot_hom_desv<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(3,5,7,9))
    
    pca_tot_hom_desv<-cbind(dpto= "Total País", pca_tot_hom_desv)
    
    colnames(pca_tot_hom_desv)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_desv)[2:5])
    
    
    pca_hombres_desv<-rbind(pca_hombres_desv, pca_tot_hom_desv)
    
    
    
    ###Mujer
    pca_mujeres<- pca%>%
      filter(e26=="Mujer")
    
    pca_mujeres_ind<- pca_mujeres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_mujeres_ind)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_ind)[2:5])
    
    pca_tot_muj_ind<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(2,4,6,8))
    
    pca_tot_muj_ind<-cbind(dpto= "Total País", pca_tot_muj_ind)
    
    colnames(pca_tot_muj_ind)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_ind)[2:5])
    
    
    pca_mujeres_ind<-rbind(pca_mujeres_ind, pca_tot_muj_ind)
    
    ##desvio
    
    pca_mujeres_desv<- pca_mujeres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_mujeres_desv)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_desv)[2:5])
    
    pca_tot_muj_desv<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(3,5,7,9))
    
    pca_tot_muj_desv<-cbind(dpto= "Total País", pca_tot_muj_desv)
    
    colnames(pca_tot_muj_desv)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_desv)[2:5])
    
    
    pca_mujeres_desv<-rbind(pca_mujeres_desv, pca_tot_muj_desv)
    
    
    ###Totales
    
    pca_dpto_ind<- pca_totales_dpto%>%
      select(c(1,2,4,6,8))
    
    
    pca_total_pais<-cbind(dpto= "Total País", pca_total_pais)
    
    pca_total_pais_ind<-pca_total_pais%>%
      select(c(1,2,4,6,8))
    
    pca_dpto_ind<-rbind(pca_dpto_ind, pca_total_pais_ind)
    
    ##desvio
    
    pca_dpto_desv<- pca_totales_dpto%>%
      select(c(1,3,5,7,9))
    
    pca_total_pais_desv<-pca_total_pais%>%
      select(c(1,3,5,7,9))
    
    pca_dpto_desv<-rbind(pca_dpto_desv, pca_total_pais_desv)
    
    ## pegado de las 3 tablas
    pca_dpto_ind<-as.data.frame(pca_dpto_ind)
    pca_hombres_ind<-as.data.frame(pca_hombres_ind)
    pca_mujeres_ind<-as.data.frame(pca_mujeres_ind)
    
    Ind610<- left_join(pca_hombres_ind, pca_mujeres_ind,  by= "dpto")
    Ind610<- left_join(Ind610, pca_dpto_ind,  by= "dpto")
    
    ##desvios
    pca_dpto_desv<-as.data.frame(pca_dpto_desv)
    pca_hombres_desv<-as.data.frame(pca_hombres_desv)
    pca_mujeres_desv<-as.data.frame(pca_mujeres_desv)
    
    desv610<- left_join(pca_hombres_desv, pca_mujeres_desv,  by= "dpto")
    desv610<- left_join(desv610, pca_dpto_desv,  by= "dpto")
    
    ##superior
    sup610<- Ind610[,2:13] +(1.96*desv610[,2:13])
    
    sup610<- cbind(departamentos, sup610)
    
    
    ##inferior
    inf610<- Ind610[,2:13] -(1.96*desv610[,2:13])
    
    inf610<- cbind(departamentos, inf610)
    
    
    colnames(Ind610)[10:13]<-paste0("Total_",colnames(Ind610)[10:13])
    i610<-Ind610
    return(i610)
  })
  
  indicador_lab<- reactive({ if(input$Nombre3 == "533") i533()
    else if(input$Nombre3 == "521") i521()
    else if(input$Nombre3 == "526") i526()
    else if(input$Nombre3 == "618") i618()
    else if(input$Nombre3 == "608") i608()
    else if(input$Nombre3 == "690") i690()
    else if(input$Nombre3 == "502") i502()
    else if(input$Nombre3 == "609") i609()
    else if(input$Nombre3 == "611") i611()
    else if(input$Nombre3 == "531") i531()
    else if(input$Nombre3 == "607") i607()
    else if(input$Nombre3 == "534") i534()
    else if(input$Nombre3 == "610") i610()
  })
  
  i568<- reactive({funcion1(pr(),"1==1", "pobre06==1")})
  i1636<-reactive({funcion1(pr(), "e27>=14 & e27<=29", "pobre06==1")}) 
  i577<- reactive({funcion8(ph(), "hacinamiento")})
  i1929<-reactive({funcion8(ph(), "d21_18==1")})
  i553<- reactive({funcion8(ph(), "pobre06")})
  
  
  indicador_ing<- reactive({ if(input$Nombre4 == "526") i526()
    else if(input$Nombre4 == "568") i568()
    else if(input$Nombre4 == "1636") i1636()
    else if(input$Nombre4 == "531") i531()
    else if(input$Nombre4 == "534") i534()
    else if(input$Nombre4 == "577") i577()
    else if(input$Nombre4 == "1929") i1929()
    else if(input$Nombre4 == "553") i553()
    
  })
  
  i605<- reactive({funcion1(pr(), "e27>5","e62==1")})
  i581<- reactive({funcion1(pr(),"e27>5","e60==1")})
  i603<- reactive({funcion1(pr(),"e27>5","e61==1")})
  i582<- reactive({funcion2(pr(), "e27>5 & e62==1", "e65", c("01_una_al_dia","02_una_a_la_semana", "03_una_al_mes", "04_no_sabe"), c(1,2,3,4))})
  i591<- reactive({funcion8(ph(), "d21_16==1")})
  i594<- reactive({funcion8(ph(), "d21_15==1")})
  i584<- reactive({funcion8(ph(), "d21_15_1==1")})
  
  
  indicador_tec<- reactive({if(input$Nombre5 == "605") i605()
    else if(input$Nombre5 == "581") i581()
    else if(input$Nombre5 == "603") i603()
    else if(input$Nombre5 == "582") i582()
    else if(input$Nombre5 == "591") i591()
    else if(input$Nombre5 == "594") i594()
    else if(input$Nombre5 == "584") i584()
  })
  
  i678<- reactive({funcion2(pr(), "1==1", "e236_mod", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i654<- reactive({funcion2(pr(), "1==1", "e37", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i655<- reactive({funcion2(pr(), "1==1", "e39", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(0,1,2,3))})
  
  indicador_demo<- reactive({if(input$Nombre6 == "678") i678()
    else if(input$Nombre6 == "654") i654()
    else if(input$Nombre6 == "655") i655()
  })
  
  i764<- reactive({funcion9(ph(), "d16", c("dpto", "No tiene baño", "Red general", "Fosa sept_pozo negro", "Entubado_hacia_arroyo", "Otro"), c(0,1,2,3,4))})
  i782<- reactive({funcion9(ph(), "d18", c("dpto", "Energía eléctrica", "Cargador batería", "Supergas_Queroseno", "Velas"), c(1,2,3,4))})
  i783<- reactive({funcion9(ph(), "d11", c("dpto", "Red general", "Pozo surg_no proteg", "Pozo surg_proteg", "Aljibe", "Arroyo_río", "Otro"), c(1,2,3,4,5,6))})
  i765<- reactive({funcion9(ph(), "d19", c("dpto", "Lugar privado", "Compartido", "No tiene"), c(1,2,3))})
  i766<- reactive({funcion9(ph(), "d15", c("dpto","No tiene", "Exclusivo", "Compartido"), c(0,1,2))})
  i774<- reactive({I774<-ph()%>%
    group_by(dpto)%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  #total pais
  t<-ph()%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  
  I774<-rbind(I774[,2:11], t)
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  I774<-cbind(departamentos, I774)
  
  I774_ind<- I774%>%
    select(c(1,2,4,6,8,10))
  
  I774_desv<- I774%>%
    select(c(1,3,5,7,9,11))
  
  sup<-I774_ind[,-1]+ (1.96*I774_desv[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-I774_ind[,-1]- (1.96*I774_desv[,-1])
  inf<-cbind(departamentos, inf)
  
  inf[inf<0]<-0
  sup[sup>1]<-1
  
  i774<- I774_ind
  return(i774)
  })
  
  indicador_hog<- reactive({if(input$Nombre7 == "577") i577()
    else if(input$Nombre7 == "553") i553()
    else if(input$Nombre7 == "764") i764()
    else if(input$Nombre7 == "782") i782()
    else if(input$Nombre7 == "783") i783()
    else if(input$Nombre7 == "765") i765()
    else if(input$Nombre7 == "766") i766()
    else if(input$Nombre7 == "774") i774()
    
  })
  
  #################
  
  #edu
  i1020Inf<- reactive({funcion3inf(pr(), "e27>=15", "e48==2")})
  i696Inf<- reactive({funcion2inf(pr(), "e27>=25", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i725Inf<- reactive({funcion2inf(pr(), "pobpcoac == 2", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i1380Inf<- reactive({funcion6inf(pr(), "e27>=25", "a_estudio")})
  i747Inf<- reactive({funcion7inf(pr(), "e197", "e6a11")})
  i746Inf<- reactive({funcion7inf(pr(), "e193", "e3a5")})
  i1808Inf<-reactive({funcion7inf(pr(), "edmedia", "e12a17")})
  i741Inf<- reactive({funcion1inf(pr(), "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")})
  i751Inf<- reactive({funcion1inf(pr(), "e27>=3 & e27<=5","e193==1")})
  i1807Inf<-reactive({funcion1inf(pr(), "e27>=12 & e27<=17", "edmedia==1")})
  i740Inf<- reactive({funcion1inf(pr(), "e27>=18", "secundaria_utu2==1")})
  i748Inf<- reactive({funcion1inf(pr(), "e27<=2", "e238==1")})
  i732Inf<- reactive({funcion1inf(pr(), "e27>=12 & e27<=17", "adol_estudios==1")})
  i756Inf<- reactive({funcion1inf(pr(), "e27>=6 & e27<=11", "e197==1")})
  i739Inf<- reactive({funcion1inf(pr(), "e27>=15", "e51_4==3")})
  i689Inf<- reactive({funcion1inf(pr(), "e27>=15", "e48==2")})
  i690Inf<- reactive({funcion1inf(pr(), "e27>=14 & e27<=24", "nini==1")})
  
  indicador_eduInf<- reactive({if(input$Nombre == "1020") i1020Inf()
    else if(input$Nombre == "696") i696Inf()
    else if(input$Nombre == "725") i725Inf()
    else if(input$Nombre == "1380") i1380Inf()
    else if(input$Nombre == "747") i747Inf()
    else if(input$Nombre == "746") i746Inf()
    else if(input$Nombre == "1808") i1808Inf()
    else if(input$Nombre == "741") i741Inf()
    else if(input$Nombre == "751") i751Inf()
    else if(input$Nombre == "1807") i1807Inf()
    else if(input$Nombre == "740") i740Inf()
    else if(input$Nombre == "748") i748Inf()
    else if(input$Nombre == "732") i732Inf()
    else if(input$Nombre == "756") i756Inf()
    else if(input$Nombre == "739") i739Inf()
    else if(input$Nombre == "689") i689Inf()
    else if(input$Nombre == "690") i690Inf()
  })
  
  i1020Sup<- reactive({funcion3sup(pr(), "e27>=15", "e48==2")})
  i696Sup<- reactive({funcion2sup(pr(), "e27>=25", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i725Sup<- reactive({funcion2sup(pr(), "pobpcoac == 2", "niveledu", c("01_Sin_instrucción", "02_Primaria", "03_Secundaria", "04_Ter_no_universitario", "05_Magi_Prof", "06_Universitario"),
                            c(0,1,2,3,4,5))})
  i1380Sup<- reactive({funcion6sup(pr(), "e27>=25", "a_estudio")})
  i747Sup<- reactive({funcion7sup(pr(), "e197", "e6a11")})
  i746Sup<- reactive({funcion7sup(pr(), "e193", "e3a5")})
  i1808Sup<-reactive({funcion7sup(pr(), "edmedia", "e12a17")})
  i741Sup<- reactive({funcion1sup(pr(), "e_25a44d ==1 | e_45a64d ==1","estudios_terciarios==1")})
  i751Sup<- reactive({funcion1sup(pr(), "e27>=3 & e27<=5","e193==1")})
  i1807Sup<-reactive({funcion1sup(pr(), "e27>=12 & e27<=17", "edmedia==1")})
  i740Sup<- reactive({funcion1sup(pr(), "e27>=18", "secundaria_utu2==1")})
  i748Sup<- reactive({funcion1sup(pr(), "e27<=2", "e238==1")})
  i732Sup<- reactive({funcion1sup(pr(), "e27>=12 & e27<=17", "adol_estudios==1")})
  i756Sup<- reactive({funcion1sup(pr(), "e27>=6 & e27<=11", "e197==1")})
  i739Sup<- reactive({funcion1sup(pr(), "e27>=15", "e51_4==3")})
  i689Sup<- reactive({funcion1sup(pr(), "e27>=15", "e48==2")})
  i690Sup<- reactive({funcion1sup(pr(), "e27>=14 & e27<=24", "nini==1")})
  
  indicador_eduSup<- reactive({if(input$Nombre == "1020") i1020Sup()
    else if(input$Nombre == "696") i696Sup()
    else if(input$Nombre == "725") i725Sup()
    else if(input$Nombre == "1380") i1380Sup()
    else if(input$Nombre == "747") i747Sup()
    else if(input$Nombre == "746") i746Sup()
    else if(input$Nombre == "1808") i1808Sup()
    else if(input$Nombre == "741") i741Sup()
    else if(input$Nombre == "751") i751Sup()
    else if(input$Nombre == "1807") i1807Sup()
    else if(input$Nombre == "740") i740Sup()
    else if(input$Nombre == "748") i748Sup()
    else if(input$Nombre == "732") i732Sup()
    else if(input$Nombre == "756") i756Sup()
    else if(input$Nombre == "739") i739Sup()
    else if(input$Nombre == "689") i689Sup()
    else if(input$Nombre == "690") i690Sup()
  })
  
  #salud
  i529Inf<- reactive({funcion4inf(pr(), "1==1", "e46==1")})
  i501Inf<- reactive({funcion1inf(pr(),"1==1","e46==1")})
  i517Inf<- reactive({
    pta<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      group_by(dpto)%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    #total pais
    t<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    
    pta<-rbind(pta[,2:17], t)
    
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pta<-cbind(departamentos, pta)
    
    pta_ind<- pta%>%
      select(c(1,2,4,6,8,10,12,14,16))
    
    pta_desv<- pta%>%
      select(c(1,3,5,7,9,11,13,15,17))
    
    pta_sup<- as.data.frame(cbind(departamentos, pta_ind$Asse+(1.96*pta_desv$Asse_se), pta_ind$IAMC+(1.96*pta_desv$IAMC_se),
                                  pta_ind$Policial_militar+(1.96*pta_desv$Policial_militar_se),pta_ind$Seguro_privado+(1.96*pta_desv$Seguro_privado_se),pta_ind$BPS+(1.96*pta_desv$BPS_se),
                                  pta_ind$Pol_municipal+(1.96*pta_desv$Pol_municipal_se), pta_ind$Otro+(1.96*pta_desv$Otro_se), pta_ind$Sin_cobertura+(1.96*pta_desv$Sin_cobertura_se)))
    
    colnames(pta_sup)<- colnames(pta_ind)  
    
    pta_sup<- pta_sup%>%
      mutate(Asse= ifelse(Asse>=1,1,Asse),
             IAMC= ifelse(IAMC>=1,1,IAMC),
             Policial_militar= ifelse(Policial_militar>=1,1,Policial_militar),
             Seguro_privado= ifelse(Seguro_privado>=1,1,Seguro_privado),
             BPS= ifelse(BPS>=1,1,BPS),
             Pol_municipal= ifelse(Pol_municipal>=1,1,Pol_municipal),
             Otro= ifelse(Otro>=1,1,Otro),
             Sin_cobertura= ifelse(Sin_cobertura>=1,1,Sin_cobertura))
    
    
    pta_inf<- as.data.frame(cbind(departamentos, pta_ind$Asse-(1.96*pta_desv$Asse_se), pta_ind$IAMC-(1.96*pta_desv$IAMC_se),
                                  pta_ind$Policial_militar-(1.96*pta_desv$Policial_militar_se),pta_ind$Seguro_privado-(1.96*pta_desv$Seguro_privado_se),pta_ind$BPS-(1.96*pta_desv$BPS_se),
                                  pta_ind$Pol_municipal-(1.96*pta_desv$Pol_municipal_se), pta_ind$Otro-(1.96*pta_desv$Otro_se), pta_ind$Sin_cobertura-(1.96*pta_desv$Sin_cobertura_se)))
    
    colnames(pta_inf)<- colnames(pta_ind)  
    
    pta_inf<- pta_inf%>%
      mutate(Asse= ifelse(Asse<0,0,Asse),
             IAMC= ifelse(IAMC<0,0,IAMC),
             Policial_militar= ifelse(Policial_militar<0,0,Policial_militar),
             Seguro_privado= ifelse(Seguro_privado<0,0,Seguro_privado),
             BPS= ifelse(BPS<0,0,BPS),
             Pol_municipal= ifelse(Pol_municipal<0,0,Pol_municipal),
             Otro= ifelse(Otro<0,0,Otro),
             Sin_cobertura= ifelse(Sin_cobertura<0,0,Sin_cobertura))
    
    i517Inf<- pta_inf
    return(i517Inf)
    
  })
  
  indicador_saludInf<- reactive({if(input$Nombre2 == "529") i529Inf()
    else if(input$Nombre2 == "501") i501Inf()
    else if(input$Nombre2 == "517") i517Inf()
  })
  
  i529Sup<- reactive({funcion4sup(pr(), "1==1", "e46==1")})
  i501Sup<- reactive({funcion1sup(pr(),"1==1","e46==1")})
  i517Sup<- reactive({
    pta<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      group_by(dpto)%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    #total pais
    t<-pr()%>%
      mutate(no_tiene=ifelse(e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2,1,0))%>%
      summarise(Asse=survey_mean(e45_1==1),
                IAMC=survey_mean(e45_2==1),
                Policial_militar=survey_mean(e45_4==1),
                Seguro_privado=survey_mean(e45_3==1),
                BPS=survey_mean(e45_5==1),
                Pol_municipal=survey_mean(e45_6==1),
                Otro=survey_mean(e45_7==1),
                Sin_cobertura=survey_mean(no_tiene))
    
    
    pta<-rbind(pta[,2:17], t)
    
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pta<-cbind(departamentos, pta)
    
    pta_ind<- pta%>%
      select(c(1,2,4,6,8,10,12,14,16))
    
    pta_desv<- pta%>%
      select(c(1,3,5,7,9,11,13,15,17))
    
    pta_sup<- as.data.frame(cbind(departamentos, pta_ind$Asse+(1.96*pta_desv$Asse_se), pta_ind$IAMC+(1.96*pta_desv$IAMC_se),
                                  pta_ind$Policial_militar+(1.96*pta_desv$Policial_militar_se),pta_ind$Seguro_privado+(1.96*pta_desv$Seguro_privado_se),pta_ind$BPS+(1.96*pta_desv$BPS_se),
                                  pta_ind$Pol_municipal+(1.96*pta_desv$Pol_municipal_se), pta_ind$Otro+(1.96*pta_desv$Otro_se), pta_ind$Sin_cobertura+(1.96*pta_desv$Sin_cobertura_se)))
    
    colnames(pta_sup)<- colnames(pta_ind)  
    
    pta_sup<- pta_sup%>%
      mutate(Asse= ifelse(Asse>=1,1,Asse),
             IAMC= ifelse(IAMC>=1,1,IAMC),
             Policial_militar= ifelse(Policial_militar>=1,1,Policial_militar),
             Seguro_privado= ifelse(Seguro_privado>=1,1,Seguro_privado),
             BPS= ifelse(BPS>=1,1,BPS),
             Pol_municipal= ifelse(Pol_municipal>=1,1,Pol_municipal),
             Otro= ifelse(Otro>=1,1,Otro),
             Sin_cobertura= ifelse(Sin_cobertura>=1,1,Sin_cobertura))
    
    i517Sup<- pta_sup
    return(i517Sup)
    
  })
  
  indicador_saludSup<- reactive({if(input$Nombre2 == "529") i529Sup()
    else if(input$Nombre2 == "501") i501Sup()
    else if(input$Nombre2 == "517") i517Sup()
  })
  
  #laboral
  
  i531Inf<- reactive({funcion5inf(pr(), "e27>13 & activo==1", "desocupados")})
  i607Inf<- reactive({funcion5inf(pr(), "e27>13", "activo==1")})
  i534Inf<- reactive({funcion5inf(pr(), "e27>13", "pobpcoac==2")})
  i609Inf<- reactive({funcion2inf(pr(), "f116==1", "f121", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i611Inf<- reactive({funcion2inf(pr(), "pobpcoac==2", "f73", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i533Inf<- reactive({funcion1inf(pr(), "e27>13", "pobpcoac==2")})
  i521Inf<- reactive({funcion1inf(pr(), "pobpcoac == 2", "subempleo==1")})
  i526Inf<- reactive({funcion1inf(pr(), "e27>13 & pobpcoac==2", "f82==2")})
  i618Inf<- reactive({funcion1inf(pr(), "pobpcoac==2", "f80==2")})
  i608Inf<- reactive({funcion1inf(pr(), "e27>13", "activo==1")})
  i502Inf<- reactive({funcion1inf(pr(), "e27>13 & activo==1", "desocupados==1")})
  i610Inf<- reactive({
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pca<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto,e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    pca<-pca%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    
    pca_totales<- pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    pca_totales_dpto<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))
    
    pca_total_pais<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    ###Hombre
    pca_hombres<- pca%>%
      filter(e26=="Hombre")
    
    pca_hombres_ind<- pca_hombres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_hombres_ind)[2:5]<-paste0("Hombre_",colnames(pca_hombres_ind)[2:5])
    
    pca_tot_hom_ind<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(2,4,6,8))
    
    pca_tot_hom_ind<-cbind(dpto= "Total País", pca_tot_hom_ind)
    
    colnames(pca_tot_hom_ind)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_ind)[2:5])
    
    
    pca_hombres_ind<-rbind(pca_hombres_ind, pca_tot_hom_ind)
    
    ##desvio
    
    pca_hombres_desv<- pca_hombres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_hombres_desv)[2:5]<-paste0("Hombre_",colnames(pca_hombres_desv)[2:5])
    
    pca_tot_hom_desv<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(3,5,7,9))
    
    pca_tot_hom_desv<-cbind(dpto= "Total País", pca_tot_hom_desv)
    
    colnames(pca_tot_hom_desv)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_desv)[2:5])
    
    
    pca_hombres_desv<-rbind(pca_hombres_desv, pca_tot_hom_desv)
    
    
    
    ###Mujer
    pca_mujeres<- pca%>%
      filter(e26=="Mujer")
    
    pca_mujeres_ind<- pca_mujeres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_mujeres_ind)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_ind)[2:5])
    
    pca_tot_muj_ind<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(2,4,6,8))
    
    pca_tot_muj_ind<-cbind(dpto= "Total País", pca_tot_muj_ind)
    
    colnames(pca_tot_muj_ind)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_ind)[2:5])
    
    
    pca_mujeres_ind<-rbind(pca_mujeres_ind, pca_tot_muj_ind)
    
    ##desvio
    
    pca_mujeres_desv<- pca_mujeres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_mujeres_desv)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_desv)[2:5])
    
    pca_tot_muj_desv<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(3,5,7,9))
    
    pca_tot_muj_desv<-cbind(dpto= "Total País", pca_tot_muj_desv)
    
    colnames(pca_tot_muj_desv)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_desv)[2:5])
    
    
    pca_mujeres_desv<-rbind(pca_mujeres_desv, pca_tot_muj_desv)
    
    
    ###Totales
    
    pca_dpto_ind<- pca_totales_dpto%>%
      select(c(1,2,4,6,8))
    
    
    pca_total_pais<-cbind(dpto= "Total País", pca_total_pais)
    
    pca_total_pais_ind<-pca_total_pais%>%
      select(c(1,2,4,6,8))
    
    pca_dpto_ind<-rbind(pca_dpto_ind, pca_total_pais_ind)
    
    ##desvio
    
    pca_dpto_desv<- pca_totales_dpto%>%
      select(c(1,3,5,7,9))
    
    pca_total_pais_desv<-pca_total_pais%>%
      select(c(1,3,5,7,9))
    
    pca_dpto_desv<-rbind(pca_dpto_desv, pca_total_pais_desv)
    
    ## pegado de las 3 tablas
    pca_dpto_ind<-as.data.frame(pca_dpto_ind)
    pca_hombres_ind<-as.data.frame(pca_hombres_ind)
    pca_mujeres_ind<-as.data.frame(pca_mujeres_ind)
    
    Ind610<- left_join(pca_hombres_ind, pca_mujeres_ind,  by= "dpto")
    Ind610<- left_join(Ind610, pca_dpto_ind,  by= "dpto")
    
    ##desvios
    pca_dpto_desv<-as.data.frame(pca_dpto_desv)
    pca_hombres_desv<-as.data.frame(pca_hombres_desv)
    pca_mujeres_desv<-as.data.frame(pca_mujeres_desv)
    
    desv610<- left_join(pca_hombres_desv, pca_mujeres_desv,  by= "dpto")
    desv610<- left_join(desv610, pca_dpto_desv,  by= "dpto")
    
    ##superior
    sup610<- Ind610[,2:13] +(1.96*desv610[,2:13])
    
    sup610<- cbind(departamentos, sup610)
    
    
    ##inferior
    inf610<- Ind610[,2:13] -(1.96*desv610[,2:13])
    
    inf610<- cbind(departamentos, inf610)
    
    
    colnames(Ind610)[10:13]<-paste0("Total_",colnames(Ind610)[10:13])
    i610Inf<-inf610
    return(i610Inf)
  })
  
  indicador_labInf<- reactive({if(input$Nombre3 == "531") i531Inf()
    else if(input$Nombre3 == "607") i607Inf()
    else if(input$Nombre3 == "534") i534Inf()
    else if(input$Nombre3 == "609") i609Inf()
    else if(input$Nombre3 == "611") i611Inf()
    else if(input$Nombre3 == "533") i533Inf()
    else if(input$Nombre3 == "521") i521Inf()
    else if(input$Nombre3 == "526") i526Inf()
    else if(input$Nombre3 == "618") i618Inf()
    else if(input$Nombre3 == "608") i608Inf()
    else if(input$Nombre3 == "502") i502Inf()
    else if(input$Nombre3 == "690") i690Inf()
    else if(input$Nombre3 == "610") i610Inf()
  })
  
  i531Sup<- reactive({funcion5sup(pr(), "e27>13 & activo==1", "desocupados")})
  i607Sup<- reactive({funcion5sup(pr(), "e27>13", "activo==1")})
  i534Sup<- reactive({funcion5sup(pr(), "e27>13", "pobpcoac==2")})
  i609Sup<- reactive({funcion2sup(pr(), "f116==1", "f121", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i611Sup<- reactive({funcion2sup(pr(), "pobpcoac==2", "f73", c("01_As_privado","02_As_público", "03_Miembro_coop_de_prod", "04_Patrón", "05_Cp_sin_local", "06_Cp_con_local", "07_Miembro_hogar_no_rem", "08_Programa_social_empleo"), c(1,2,3,4,5,6,7,8))})
  i533Sup<- reactive({funcion1sup(pr(), "e27>13", "pobpcoac==2")})
  i521Sup<- reactive({funcion1sup(pr(), "pobpcoac == 2", "subempleo==1")})
  i526Sup<- reactive({funcion1sup(pr(), "e27>13 & pobpcoac==2", "f82==2")})
  i618Sup<- reactive({funcion1sup(pr(), "pobpcoac==2", "f80==2")})
  i608Sup<- reactive({funcion1sup(pr(), "e27>13", "activo==1")})
  i502Sup<- reactive({funcion1sup(pr(), "e27>13 & activo==1", "desocupados==1")})
  i610Sup<- reactive({
    departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                      "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
    
    pca<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto,e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    pca<-pca%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    
    pca_totales<- pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(e26)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(e26= recode(as.factor(e26), "1"= "Hombre", "2"= "Mujer"))
    
    pca_totales_dpto<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      group_by(dpto)%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))%>%
      mutate(dpto= recode(as.factor(dpto), "1"="Montevideo","2"="Artigas","3"="Canelones","4"="Cerro Largo","5"="Colonia","6"="Durazno","7"="Flores","8"="Florida","9"="Lavalleja",
                          "10"="Maldonado","11"="Paysandú","12"="Río Negro","13"="Rivera","14"="Rocha","15"="Salto","16"="San José","17"="Soriano","18"="Tacuarembó","19"="Treinta y Tres" ))
    
    pca_total_pais<-pr()%>%
      mutate(desocupados=ifelse(pobpcoac==3 | pobpcoac==4 | pobpcoac==5,1,0))%>%
      mutate(inactivos=ifelse(pobpcoac==6 | pobpcoac==7 | pobpcoac==8 | pobpcoac==9 | pobpcoac==10 | pobpcoac==11,1,0))%>%
      summarise(Menores14=survey_mean(pobpcoac==1),
                Ocupados=survey_mean(pobpcoac==2),
                Desocupados=survey_mean(desocupados),
                Inactivos=survey_mean(inactivos))
    
    ###Hombre
    pca_hombres<- pca%>%
      filter(e26=="Hombre")
    
    pca_hombres_ind<- pca_hombres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_hombres_ind)[2:5]<-paste0("Hombre_",colnames(pca_hombres_ind)[2:5])
    
    pca_tot_hom_ind<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(2,4,6,8))
    
    pca_tot_hom_ind<-cbind(dpto= "Total País", pca_tot_hom_ind)
    
    colnames(pca_tot_hom_ind)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_ind)[2:5])
    
    
    pca_hombres_ind<-rbind(pca_hombres_ind, pca_tot_hom_ind)
    
    ##desvio
    
    pca_hombres_desv<- pca_hombres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_hombres_desv)[2:5]<-paste0("Hombre_",colnames(pca_hombres_desv)[2:5])
    
    pca_tot_hom_desv<-pca_totales%>%
      filter(e26=="Hombre")%>%
      select(c(3,5,7,9))
    
    pca_tot_hom_desv<-cbind(dpto= "Total País", pca_tot_hom_desv)
    
    colnames(pca_tot_hom_desv)[2:5]<-paste0("Hombre_",colnames(pca_tot_hom_desv)[2:5])
    
    
    pca_hombres_desv<-rbind(pca_hombres_desv, pca_tot_hom_desv)
    
    
    
    ###Mujer
    pca_mujeres<- pca%>%
      filter(e26=="Mujer")
    
    pca_mujeres_ind<- pca_mujeres%>%
      select(c(1,3,5,7,9))
    
    
    colnames(pca_mujeres_ind)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_ind)[2:5])
    
    pca_tot_muj_ind<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(2,4,6,8))
    
    pca_tot_muj_ind<-cbind(dpto= "Total País", pca_tot_muj_ind)
    
    colnames(pca_tot_muj_ind)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_ind)[2:5])
    
    
    pca_mujeres_ind<-rbind(pca_mujeres_ind, pca_tot_muj_ind)
    
    ##desvio
    
    pca_mujeres_desv<- pca_mujeres%>%
      select(c(1,4,6,8,10))
    
    
    colnames(pca_mujeres_desv)[2:5]<-paste0("Mujer_",colnames(pca_mujeres_desv)[2:5])
    
    pca_tot_muj_desv<-pca_totales%>%
      filter(e26=="Mujer")%>%
      select(c(3,5,7,9))
    
    pca_tot_muj_desv<-cbind(dpto= "Total País", pca_tot_muj_desv)
    
    colnames(pca_tot_muj_desv)[2:5]<-paste0("Mujer_",colnames(pca_tot_muj_desv)[2:5])
    
    
    pca_mujeres_desv<-rbind(pca_mujeres_desv, pca_tot_muj_desv)
    
    
    ###Totales
    
    pca_dpto_ind<- pca_totales_dpto%>%
      select(c(1,2,4,6,8))
    
    
    pca_total_pais<-cbind(dpto= "Total País", pca_total_pais)
    
    pca_total_pais_ind<-pca_total_pais%>%
      select(c(1,2,4,6,8))
    
    pca_dpto_ind<-rbind(pca_dpto_ind, pca_total_pais_ind)
    
    ##desvio
    
    pca_dpto_desv<- pca_totales_dpto%>%
      select(c(1,3,5,7,9))
    
    pca_total_pais_desv<-pca_total_pais%>%
      select(c(1,3,5,7,9))
    
    pca_dpto_desv<-rbind(pca_dpto_desv, pca_total_pais_desv)
    
    ## pegado de las 3 tablas
    pca_dpto_ind<-as.data.frame(pca_dpto_ind)
    pca_hombres_ind<-as.data.frame(pca_hombres_ind)
    pca_mujeres_ind<-as.data.frame(pca_mujeres_ind)
    
    Ind610<- left_join(pca_hombres_ind, pca_mujeres_ind,  by= "dpto")
    Ind610<- left_join(Ind610, pca_dpto_ind,  by= "dpto")
    
    ##desvios
    pca_dpto_desv<-as.data.frame(pca_dpto_desv)
    pca_hombres_desv<-as.data.frame(pca_hombres_desv)
    pca_mujeres_desv<-as.data.frame(pca_mujeres_desv)
    
    desv610<- left_join(pca_hombres_desv, pca_mujeres_desv,  by= "dpto")
    desv610<- left_join(desv610, pca_dpto_desv,  by= "dpto")
    
    ##superior
    sup610<- Ind610[,2:13] +(1.96*desv610[,2:13])
    
    sup610<- cbind(departamentos, sup610)
    
    
    
    colnames(Ind610)[10:13]<-paste0("Total_",colnames(Ind610)[10:13])
    i610Sup<-sup610
    return(i610Sup)
  })
  
  indicador_labSup<- reactive({if(input$Nombre3 == "531") i531Sup()
    else if(input$Nombre3 == "607") i607Sup()
    else if(input$Nombre3 == "534") i534Sup()
    else if(input$Nombre3 == "609") i609Sup()
    else if(input$Nombre3 == "611") i611Sup()
    else if(input$Nombre3 == "533") i533Sup()
    else if(input$Nombre3 == "521") i521Sup()
    else if(input$Nombre3 == "526") i526Sup()
    else if(input$Nombre3 == "618") i618Sup()
    else if(input$Nombre3 == "608") i608Sup()
    else if(input$Nombre3 == "502") i502Sup()
    else if(input$Nombre3 == "690") i690Sup()
    else if(input$Nombre3 == "610") i610Sup()
  })
  #ing
  
  i568Inf<- reactive({funcion1inf(pr(),"1==1", "pobre06==1")})
  i1636Inf<-reactive({funcion1inf(pr(), "e27>=14 & e27<=29", "pobre06==1")})
  i577Inf<- reactive({funcion8inf(ph(), "hacinamiento")})
  i1929Inf<-reactive({funcion8inf(ph(), "d21_18==1")})
  i553Inf<- reactive({funcion8inf(ph(), "pobre06")})
  
  indicador_ingInf<- reactive({if(input$Nombre4 == "531") i531Inf()
    else if(input$Nombre4 == "534") i534Inf()
    else if(input$Nombre4 == "526") i526Inf()
    else if(input$Nombre4 == "568") i568Inf()
    else if(input$Nombre4 == "1636") i1636Inf()
    else if(input$Nombre4 == "577") i577Inf()
    else if(input$Nombre4 == "1929") i1929Inf()
    else if(input$Nombre4 == "553") i553Inf()
  })
  
  i568Sup<- reactive({funcion1sup(pr(),"1==1", "pobre06==1")})
  i1636Sup<-reactive({funcion1sup(pr(), "e27>=14 & e27<=29", "pobre06==1")})
  i577Sup<- reactive({funcion8sup(ph(), "hacinamiento")})
  i1929Sup<-reactive({funcion8sup(ph(), "d21_18==1")})
  i553Sup<- reactive({funcion8sup(ph(), "pobre06")})
  
  indicador_ingSup<- reactive({if(input$Nombre4 == "531") i531Sup()
    else if(input$Nombre4 == "534") i534Sup()
    else if(input$Nombre4 == "526") i526Sup()
    else if(input$Nombre4 == "568") i568Sup()
    else if(input$Nombre4 == "1636") i1636Sup()
    else if(input$Nombre4 == "577") i577Sup()
    else if(input$Nombre4 == "1929") i1929Sup()
    else if(input$Nombre4 == "553") i553Sup()
  })
  
  #Tec
  
  i582Inf<- reactive({funcion2inf(pr(), "e27>5 & e62==1", "e65", c("01_una_al_dia","02_una_a_la_semana", "03_una_al_mes", "04_no_sabe"), c(1,2,3,4))})
  i605Inf<- reactive({funcion1inf(pr(), "e27>5","e62==1")})
  i581Inf<- reactive({funcion1inf(pr(),"e27>5","e60==1")})
  i603Inf<- reactive({funcion1inf(pr(),"e27>5","e61==1")})
  i591Inf<- reactive({funcion8inf(ph(), "d21_16==1")})
  i594Inf<- reactive({funcion8inf(ph(), "d21_15==1")})
  i584Inf<- reactive({funcion8inf(ph(), "d21_15_1==1")})
  
  indicador_tecInf<- reactive({if(input$Nombre5 == "582") i582Inf()
    else if(input$Nombre5 == "605") i605Inf()
    else if(input$Nombre5 == "581") i581Inf()
    else if(input$Nombre5 == "603") i603Inf()
    else if(input$Nombre5 == "591") i591Inf()
    else if(input$Nombre5 == "594") i594Inf()
    else if(input$Nombre5 == "584") i584Inf()
  })
  
  i582Sup<- reactive({funcion2sup(pr(), "e27>5 & e62==1", "e65", c("01_una_al_dia","02_una_a_la_semana", "03_una_al_mes", "04_no_sabe"), c(1,2,3,4))})
  i605Sup<- reactive({funcion1sup(pr(), "e27>5","e62==1")})
  i581Sup<- reactive({funcion1sup(pr(),"e27>5","e60==1")})
  i603Sup<- reactive({funcion1sup(pr(),"e27>5","e61==1")})
  i591Sup<- reactive({funcion8sup(ph(), "d21_16==1")})
  i594Sup<- reactive({funcion8sup(ph(), "d21_15==1")})
  i584Sup<- reactive({funcion8sup(ph(), "d21_15_1==1")})
  
  indicador_tecSup<- reactive({if(input$Nombre5 == "582") i582Sup()
    else if(input$Nombre5 == "605") i605Sup()
    else if(input$Nombre5 == "581") i581Sup()
    else if(input$Nombre5 == "603") i603Sup()
    else if(input$Nombre5 == "591") i591Sup()
    else if(input$Nombre5 == "594") i594Sup()
    else if(input$Nombre5 == "584") i584Sup()
  })
  #demo
  i678Inf<- reactive({funcion2inf(pr(), "1==1", "e236_mod", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i654Inf<- reactive({funcion2inf(pr(), "1==1", "e37", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i655Inf<- reactive({funcion2inf(pr(), "1==1", "e39", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(0,1,2,3))})
  
  indicador_demoInf<- reactive({if(input$Nombre6 == "678") i678Inf()
    else if(input$Nombre6 == "654") i654Inf()
    else if(input$Nombre6 == "655") i655Inf()
  })
  
  i678Sup<- reactive({funcion2sup(pr(), "1==1", "e236_mod", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i654Sup<- reactive({funcion2sup(pr(), "1==1", "e37", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(1,2,3,4))})
  i655Sup<- reactive({funcion2sup(pr(), "1==1", "e39", c("01_misma", "02_otra_loc", "03_otro_depto", "04_otro_país"), c(0,1,2,3))})
  
  indicador_demoSup<- reactive({if(input$Nombre6 == "678") i678Sup()
    else if(input$Nombre6 == "654") i654Sup()
    else if(input$Nombre6 == "655") i655Sup()
  })
  
  #hog
  i764Inf<- reactive({funcion9inf(ph(), "d16", c("dpto", "No tiene baño", "Red general", "Fosa sept_pozo negro", "Entubado_hacia_arroyo", "Otro"), c(0,1,2,3,4))})
  i782Inf<- reactive({funcion9inf(ph(), "d18", c("dpto", "Energía eléctrica", "Cargador batería", "Supergas_Queroseno", "Velas"), c(1,2,3,4))})
  i783Inf<- reactive({funcion9inf(ph(), "d11", c("dpto", "Red general", "Pozo surg_no proteg", "Pozo surg_proteg", "Aljibe", "Arroyo_río", "Otro"), c(1,2,3,4,5,6))})
  i765Inf<- reactive({funcion9inf(ph(), "d19", c("dpto", "Lugar privado", "Compartido", "No tiene"), c(1,2,3))})
  i766Inf<- reactive({funcion9inf(ph(), "d15", c("dpto","No tiene", "Exclusivo", "Compartido"), c(0,1,2))})
  i774Inf<- reactive({I774<-ph()%>%
    group_by(dpto)%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  #total pais
  t<-ph()%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  
  I774<-rbind(I774[,2:11], t)
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  I774<-cbind(departamentos, I774)
  
  I774_ind<- I774%>%
    select(c(1,2,4,6,8,10))
  
  I774_desv<- I774%>%
    select(c(1,3,5,7,9,11))
  
  sup<-I774_ind[,-1]+ (1.96*I774_desv[,-1])
  sup<-cbind(departamentos, sup)
  
  inf<-I774_ind[,-1]- (1.96*I774_desv[,-1])
  inf<-cbind(departamentos, inf)
  
  inf[inf<0]<-0
  
  
  i774Inf<- inf
  return(i774Inf)
  })
  
  indicador_hogInf<- reactive({if(input$Nombre7 == "764") i764Inf()
    else if(input$Nombre7 == "782") i782Inf()
    else if(input$Nombre7 == "783") i783Inf()
    else if(input$Nombre7 == "765") i765Inf()
    else if(input$Nombre7 == "766") i766Inf()
    else if(input$Nombre7 == "577") i577Inf()
    else if(input$Nombre7 == "553") i553Inf()
    else if(input$Nombre7 == "774") i774Inf()
  })
  
  i764Sup<- reactive({funcion9sup(ph(), "d16", c("dpto", "No tiene baño", "Red general", "Fosa sept_pozo negro", "Entubado_hacia_arroyo", "Otro"), c(0,1,2,3,4))})
  i782Sup<- reactive({funcion9sup(ph(), "d18", c("dpto", "Energía eléctrica", "Cargador batería", "Supergas_Queroseno", "Velas"), c(1,2,3,4))})
  i783Sup<- reactive({funcion9sup(ph(), "d11", c("dpto", "Red general", "Pozo surg_no proteg", "Pozo surg_proteg", "Aljibe", "Arroyo_río", "Otro"), c(1,2,3,4,5,6))})
  i765Sup<- reactive({funcion9sup(ph(), "d19", c("dpto", "Lugar privado", "Compartido", "No tiene"), c(1,2,3))})
  i766Sup<- reactive({funcion9sup(ph(), "d15", c("dpto","No tiene", "Exclusivo", "Compartido"), c(0,1,2))})
  i774Sup<- reactive({I774<-ph()%>%
    group_by(dpto)%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  #total pais
  t<-ph()%>%
    summarise(Propietarios=survey_mean(propietarios),
              Inquilinos=survey_mean(d8_1==5),
              Ocupantes_con_permiso=survey_mean(Oc_con_permiso),
              Ocupantes_sin_permiso=survey_mean(d8_1==9),
              Coop_vivienda=survey_mean(d8_1==10))
  
  I774<-rbind(I774[,2:11], t)
  
  departamentos<- data.frame(dpto=c("Montevideo","Artigas","Canelones","Cerro Largo","Colonia","Durazno","Flores","Florida","Lavalleja",
                                    "Maldonado","Paysandú","Río Negro","Rivera","Rocha","Salto","San José","Soriano","Tacuarembó","Treinta y Tres", "Total País" ))
  
  I774<-cbind(departamentos, I774)
  
  I774_ind<- I774%>%
    select(c(1,2,4,6,8,10))
  
  I774_desv<- I774%>%
    select(c(1,3,5,7,9,11))
  
  sup<-I774_ind[,-1]+ (1.96*I774_desv[,-1])
  sup<-cbind(departamentos, sup)
  
  
  i774Sup<- sup
  return(i774Sup)
  })
  
  indicador_hogSup<- reactive({if(input$Nombre7 == "764") i764Sup()
    else if(input$Nombre7 == "782") i782Sup()
    else if(input$Nombre7 == "783") i783Sup()
    else if(input$Nombre7 == "765") i765Sup()
    else if(input$Nombre7 == "766") i766Sup()
    else if(input$Nombre7 == "577") i577Sup()
    else if(input$Nombre7 == "553") i553Sup()
    else if(input$Nombre7 == "774") i774Sup()
  })
  output$datos1 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_edu(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
    
  }) 
  
  output$datos2 <- renderDT({
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_salud(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
  })
  
  output$datos3 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_lab(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
  })
  
  output$datos4 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_ing(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$datos5 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_tec(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$datos6 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_demo(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$datos7 <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_hog(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  
  output$InfEdu <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_eduInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupEdu <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_eduSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfSalud <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_saludInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupSalud <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_saludSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfLab <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_labInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupLab <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_labSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfIng <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_ingInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupIng <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_ingSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfTec <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_tecInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupTec <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_tecSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfDemo <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_demoInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupDemo <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_demoSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$InfHog <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_hogInf(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  output$SupHog <- renderDT({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    datatable(indicador_hogSup(), 
              options = list(info = F,
                             paging = F,
                             searching = T,
                             stripeClasses = F, 
                             lengthChange = F,
                             scrollX = T),
              rownames = F) %>% formatRound(c(-1), 4)
    
    
  })
  
  
  output$SeleccionEdu <- renderUI({ 
    selectInput("CatEdu", "Categoría:", choices = unique(colnames(indicador_edu())[-1])) 
  })
  
  output$SeleccionSalud <- renderUI({ 
    selectInput("CatSalud", "Categoría:", choices = unique(colnames(indicador_salud())[-1])) 
  })
  
  output$SeleccionLab <- renderUI({ 
    selectInput("CatLab", "Categoría:", choices = unique(colnames(indicador_lab())[-1])) 
  })
  
  output$SeleccionIng <- renderUI({ 
    selectInput("CatIng", "Categoría:", choices = unique(colnames(indicador_ing())[-1])) 
  })
  
  output$SeleccionTec <- renderUI({ 
    selectInput("CatTec", "Categoría:", choices = unique(colnames(indicador_tec())[-1])) 
  })
  
  output$SeleccionDemo <- renderUI({ 
    selectInput("CatDemo", "Categoría:", choices = unique(colnames(indicador_demo())[-1])) 
  }) 
  
  output$SeleccionHog <- renderUI({ 
    selectInput("CatHog", "Categoría:", choices = unique(colnames(indicador_hog())[-1])) 
  })
  
  #GRAFICO DE BARRAS 
  output$barras_edu <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<- if (input$Nombre == "741" | input$Nombre == "751" | input$Nombre == "1807"| input$Nombre == "740"| input$Nombre == "748"| input$Nombre == "690"| input$Nombre == "756"| input$Nombre == "689"| input$Nombre == "732"| input$Nombre == "739")
      indicador_edu()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre == "696" | input$Nombre == "725")
      indicador_edu()%>%
      gather(key= Categoría, value= Valor, -`00_dpto`)%>%
      separate(Categoría, c("Sexo", "Categoría"), "_0")%>%
      ggplot(aes(x=`00_dpto`, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      scale_fill_brewer(palette = "Set2")+
      facet_wrap(~Sexo)+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre == "1020")
      indicador_edu()%>%
      gather(key= Tramo_etario, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Tramo_etario))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_fill_brewer(palette = "Set2")+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre == "1380")
      indicador_edu()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Indicador")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre == "747" | input$Nombre == "746"| input$Nombre == "1808")
      indicador_edu()%>%
      ggplot(aes(x=dpto, y= tasa))+
      geom_bar(stat = "identity", fill= "lightblue")+
      coord_flip()+
      labs(x= "Departamento", y= "Indicador")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  output$barras_salud<- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<- if (input$Nombre2 == "501")
      indicador_salud()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre2 == "529")
      indicador_salud()%>%
      gather(key= Tramo_etario, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Tramo_etario))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre2 == "517")
      indicador_salud()%>%
      gather(key= Categoría, value= Valor, -dpto)%>%
      ggplot(aes(x=dpto, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  output$barras_lab<- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<-if (input$Nombre3 == "533" | input$Nombre3 == "521" | input$Nombre3 == "526"| input$Nombre3 == "618"| input$Nombre3 == "608"| input$Nombre3 == "690"| input$Nombre3 == "502")
      indicador_lab()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre3 == "609" | input$Nombre3 == "611")
      indicador_lab()%>%
      gather(key= Categoría, value= Valor, -`00_dpto`)%>%
      separate(Categoría, c("Sexo", "Categoría"), "_0")%>%
      ggplot(aes(x=`00_dpto`, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      facet_wrap(~Sexo)+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre3 == "531"| input$Nombre3 == "607" | input$Nombre3 == "534")
      indicador_lab()%>%
      gather(key= Tramo_etario, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Tramo_etario))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre3 == "610")
      indicador_lab()%>%
      gather(key= Categoría, value= Valor, -dpto)%>%
      separate(Categoría, c("Sexo", "Categoría"), "_")%>%
      ggplot(aes(x=dpto, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      facet_wrap(~Sexo)+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  output$barras_ing <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<-if (input$Nombre4 == "526" | input$Nombre4 == "568" | input$Nombre4 == "1636")
      indicador_ing()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre4 == "531"| input$Nombre4 == "534")
      indicador_ing()%>%
      gather(key= Tramo_etario, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Tramo_etario))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre4 == "577" | input$Nombre4 == "1929"| input$Nombre4 == "553")
      indicador_ing()%>%
      ggplot(aes(x=dpto, y= Total))+
      geom_bar(stat = "identity", fill= "lightblue")+
      coord_flip()+
      scale_y_continuous(labels = scales::percent)+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  output$barras_tec <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<-if (input$Nombre5 == "605" | input$Nombre5 == "581" | input$Nombre5 == "603")
      indicador_tec()%>%
      gather(key= Sexo, value= Valor, -depto)%>% 
      ggplot(aes(x=depto, y= Valor, fill= Sexo))+
      geom_bar(position = "dodge", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre5 == "582")
      indicador_tec()%>%
      gather(key= Categoría, value= Valor, -`00_dpto`)%>%
      separate(Categoría, c("Sexo", "Categoría"), "_0")%>%
      ggplot(aes(x=`00_dpto`, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      scale_fill_brewer(palette = "Set2")+
      facet_wrap(~Sexo)+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre5 == "591" | input$Nombre5 == "594"| input$Nombre5 == "584")
      indicador_tec()%>%
      ggplot(aes(x=dpto, y= Total))+
      geom_bar(stat = "identity", fill= "lightblue")+
      coord_flip()+
      scale_y_continuous(labels = scales::percent)+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  output$barras_demo <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<-indicador_demo()%>%
      gather(key= Categoría, value= Valor, -`00_dpto`)%>%
      separate(Categoría, c("Sexo", "Categoría"), "_0")%>%
      ggplot(aes(x=`00_dpto`, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      facet_wrap(~Sexo)+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title.y = element_text(face="bold", vjust = -5))+
      theme (axis.title.x = element_text(face="bold"))
    ggplotly(p)
    #ggplotly(p)
    
  })
  output$barras_hog <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    p<-if (input$Nombre7 == "577" | input$Nombre7 == "553")
      indicador_hog()%>%
      ggplot(aes(x=dpto, y= Total))+
      geom_bar(stat = "identity", fill= "lightblue")+
      coord_flip()+
      scale_y_continuous(labels = scales::percent)+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    else if (input$Nombre7 == "764" | input$Nombre7 == "782"| input$Nombre7 == "783"| input$Nombre7 == "765"| input$Nombre7 == "766"| input$Nombre7 == "774")
      indicador_hog()%>%
      gather(key= Categoría, value= Valor, -dpto)%>% 
      ggplot(aes(x=dpto, y= Valor, fill= Categoría))+
      geom_bar(position = "fill", stat = "identity")+
      scale_y_continuous(labels = scales::percent)+
      scale_fill_brewer(palette = "Set2")+
      coord_flip()+
      labs(x= "Departamento", y= "Porcentaje")+
      theme(axis.text.y = element_text(face="bold"))+
      theme (axis.title = element_text(face="bold"))
    
    ggplotly(p)
    
  })
  
  
  output$map_edu <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre == "741" | input$Nombre == "751" | input$Nombre == "1807"| input$Nombre == "740"| input$Nombre == "748"| input$Nombre == "690"| input$Nombre == "756"| input$Nombre == "689"| input$Nombre == "732"| input$Nombre == "739" | input$Nombre == "1020" | input$Nombre == "1380")
      left_join(mapa_uru, indicador_edu(), by = c("popup" = "depto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatEdu))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre == "747" | input$Nombre == "746"| input$Nombre == "1808")
      left_join(mapa_uru, indicador_edu(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatEdu))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre == "696" | input$Nombre == "725")
      left_join(mapa_uru, indicador_edu(), by = c("popup" = "00_dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatEdu))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    ggplotly(m)
    
  })
  output$map_salud <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre2 == "501" | input$Nombre2 == "529")
      left_join(mapa_uru, indicador_salud(), by = c("popup" = "depto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatSalud))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
      else if (input$Nombre2 == "517")
        left_join(mapa_uru, indicador_salud(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatSalud))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    
    ggplotly(m)
    
  })
  output$map_lab <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre3 == "533" | input$Nombre3 == "521"| input$Nombre3 == "526"| input$Nombre3 == "618"| input$Nombre3 == "608"| input$Nombre3 == "690"| input$Nombre3 == "502"| input$Nombre3 == "531" | input$Nombre3 == "607"| input$Nombre3 == "534")
      left_join(mapa_uru, indicador_lab(), by = c("popup" = "depto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatLab))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre3 == "609" | input$Nombre3 == "611")
      left_join(mapa_uru, indicador_lab(), by = c("popup" = "00_dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatLab))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre3 == "610")
      left_join(mapa_uru, indicador_lab(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatLab))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    
    ggplotly(m)
    
  })
  output$map_ing <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre4 == "526" | input$Nombre4 == "568"| input$Nombre4 == "1636"| input$Nombre4 == "531"| input$Nombre4 == "534")
      left_join(mapa_uru, indicador_ing(), by = c("popup" = "depto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatIng))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre4 == "577"| input$Nombre4 == "1929"| input$Nombre4 == "553")
      left_join(mapa_uru, indicador_ing(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatIng))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    
    ggplotly(m)
    
  })
  output$map_tec <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre5 == "605" | input$Nombre5 == "581"| input$Nombre5 == "603")
      left_join(mapa_uru, indicador_tec(), by = c("popup" = "depto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatTec))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre5 == "591"| input$Nombre5 == "594"| input$Nombre5 == "584")
      left_join(mapa_uru, indicador_tec(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatTec))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    else if (input$Nombre5 == "582")
      left_join(mapa_uru, indicador_tec(), by = c("popup" = "00_dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatTec))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    
    ggplotly(m)
  })
  output$map_demo <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-left_join(mapa_uru, indicador_demo(), by = c("popup" = "00_dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatDemo))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    
    
    ggplotly(m)
  })
  output$map_hog <- renderPlotly({
    
    inFile <- input$datos
    if (is.null(inFile))
      return('No data')
    
    m<-if (input$Nombre7 == "577"| input$Nombre7 == "553")
      left_join(mapa_uru, indicador_hog(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatHog))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
      else if (input$Nombre7 == "764"| input$Nombre7 == "782" | input$Nombre7 == "783"| input$Nombre7 == "765"| input$Nombre7 == "766"| input$Nombre7 == "774")
      left_join(mapa_uru, indicador_hog(), by = c("popup" = "dpto"))%>%
      ggplot(aes(x=long, y=lat, group = group,
                 fill = !!sym(input$CatHog))) +
      geom_polygon()  +
      geom_path(color = "white") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43")+
      coord_equal() +
      theme(title = element_blank(),
            axis.text = element_blank())
    ggplotly(m)
  })
  
  output$dd1 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_edu(), file  ,sep=";",row.names = F)
  })
  output$dd2 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_salud(), file  ,sep=";",row.names = F)
  })
  output$dd3 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_lab(), file  ,sep=";",row.names = F)
  })
  output$dd4 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_ing(), file  ,sep=";",row.names = F)
  })
  output$dd5 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_tec(), file  ,sep=";",row.names = F)
  })
  output$dd6 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_demo(), file  ,sep=";",row.names = F)
  })
  output$dd7 = downloadHandler('indicador.csv', content = function(file) {
    write.table(indicador_hog(), file  ,sep=";",row.names = F)
  })
  
  
}

shinyApp(ui, server)
