library(seriation)
library(cluster)
library(TSP)
library(gclus)
library(grid)
library(colorspace)
library(MASS)
library(mclust)
library(flexmix)
library(lattice)
library(dummies)
library(fpc)
library(tidyr)
library(dplyr)
library(lubridate)
library(reshape2)





D2014<-read.csv("Accidentalidad_georreferenciada_2014.csv",sep=",",header = TRUE)
D2015<-read.csv("Accidentalidad_georreferenciada_2015.csv",sep=",",header = TRUE)
D2016<-read.csv("Accidentalidad_georreferenciada_2016.csv",sep=",",header = TRUE)
D2017<-read.csv("Accidentalidad_georreferenciada_2017.csv",sep=",",header = TRUE)
D2018<-read.csv("Accidentalidad_georreferenciada_2018.csv",sep=",",header = TRUE)
fechaEspecial<-read.csv("Fechas_Especiales.csv",sep=",",header = TRUE)

###### concatenación de los datos
entrenamientomod<-rbind(D2014,D2015,D2016,D2017)

entrenamientomod$FECHA <- as.Date(entrenamientomod$FECHA)
fechaEspecial$FECHA <- as.Date(fechaEspecial$FECHA)
D2018$FECHA <- as.Date(D2018$FECHA)

#Nombre del d�a 

entrenamientomod$DIA_NOMBRE <-case_when( entrenamientomod$DIA_NOMBRE=="LUNES    " ~ "LUNES",
                                         entrenamientomod$DIA_NOMBRE=="MARTES   " ~ "MARTES",
                                         entrenamientomod$DIA_NOMBRE=="MIÃ???RCOLES" ~ "MIERCOLES",
                                         entrenamientomod$DIA_NOMBRE=="JUEVES   " ~ "JUEVES",
                                         entrenamientomod$DIA_NOMBRE=="VIERNES  " ~ "VIERNES",
                                         entrenamientomod$DIA_NOMBRE=="SÃ\u0081BADO   " ~ "SABADO",
                                         entrenamientomod$DIA_NOMBRE=="DOMINGO  " ~ "DOMINGO",
                                         TRUE ~ "MIERCOLES")

#Nombre del mes 
entrenamientomod$MES <-case_when( entrenamientomod$MES=="1" ~ "ENERO",
                                  entrenamientomod$MES=="2" ~ "FEBRERO",
                                  entrenamientomod$MES=="3" ~ "MARZO",
                                  entrenamientomod$MES=="4" ~ "ABRIL",
                                  entrenamientomod$MES=="5" ~ "MAYO",
                                  entrenamientomod$MES=="6" ~ "JUNIO",
                                  entrenamientomod$MES=="7" ~ "JULIO",
                                  entrenamientomod$MES=="8" ~ "AGOSTO",
                                  entrenamientomod$MES=="9" ~ "SEPTIEMBRE",
                                  entrenamientomod$MES=="10" ~ "OCTUBRE",
                                  entrenamientomod$MES=="11" ~ "NOVIEMBRE",
                                  entrenamientomod$MES=="12" ~ "DICIEMBRE",
                                  TRUE ~ "raro")


######Clase
entrenamientomod$CLASE <-case_when( entrenamientomod$CLASE=="Atropello" ~ "Atropello",
                                    entrenamientomod$CLASE=="CaÃ???da de Ocupante" ~ "Caida de Ocupante",
                                    entrenamientomod$CLASE=="Choque" ~ "Choque",
                                    entrenamientomod$CLASE=="Incendio" ~ "Incendio",
                                    entrenamientomod$CLASE=="Otro" ~ "Otros",
                                    entrenamientomod$CLASE=="Volcamiento" ~ "Volcamiento",
                                    entrenamientomod$CLASE=="Caida de Ocupante" ~ "Caida de Ocupante",
                                    entrenamientomod$CLASE=="Caida Ocupante" ~ "Caida de Ocupante",
                                    entrenamientomod$CLASE=="CaÃ???da Ocupante" ~ "Caida de Ocupante",
                                    entrenamientomod$CLASE=="Choque " ~ "Choque",
                                    entrenamientomod$CLASE=="Choque y Atropello" ~ "Otros",
                                    TRUE ~ "Otros")


####Gravedad 
entrenamientomod$GRAVEDAD <-case_when( entrenamientomod$GRAVEDAD=="HERIDO" ~ "HERIDO",
                                       entrenamientomod$GRAVEDAD=="MUERTO" ~ "MUERTO",
                                       entrenamientomod$GRAVEDAD=="SOLO DAÃ'OS" ~ "SOLO_DANOS",
                                       TRUE ~ "raro")


#####Tramo_de_via
entrenamientomod$DISENO <-case_when( entrenamientomod$DISENO=="Ciclo Ruta" ~ "Ciclo Ruta",
                                     entrenamientomod$DISENO=="Glorieta" ~ "Glorieta",
                                     entrenamientomod$DISENO=="Choque" ~ "Choque",
                                     entrenamientomod$DISENO=="Interseccion" ~ "Interseccion",
                                     entrenamientomod$DISENO=="Lote o Predio" ~ "Lote o Predio",
                                     entrenamientomod$DISENO=="Paso a Nivel" ~ "Paso a Nivel",
                                     entrenamientomod$DISENO=="Paso Elevado" ~ "Paso Elevado",
                                     entrenamientomod$DISENO=="Paso Inferior" ~ "Paso Inferior",
                                     entrenamientomod$DISENO=="Puente" ~ "Puente",
                                     entrenamientomod$DISENO=="Tramo de via" ~ "Tramo de via",
                                     entrenamientomod$DISENO=="Tunel" ~ "Tunel",
                                     entrenamientomod$DISENO=="Via peatonal" ~ "Via peatonal",
                                     TRUE ~ "Otros")



colnames(fechaEspecial) <- c("FECHA","ESPECIAL")

fechaEspecial<-fechaEspecial[,c("FECHA", "ESPECIAL")]
na.omit(fechaEspecial)

df <- inner_join(entrenamientomod,fechaEspecial,by ="FECHA")

colnames(entrenamientomod)

columnas <- c('DIA','DIA_NOMBRE','MES','CLASE','DISENO','ESPECIAL','PERIODO')

df <- df[,columnas] 

tabla <- 
df %>% group_by(DIA,DIA_NOMBRE,MES,CLASE,DISENO, ESPECIAL)%>% summarise(conteo=n())


##################################################
####Cambios sobre 2018
D2018$DIA_NOMBRE <-case_when( D2018$DIA_NOMBRE=="LUNES    " ~ "LUNES",
                              D2018$DIA_NOMBRE=="MARTES   " ~ "MARTES",
                              D2018$DIA_NOMBRE=="MIÃ???RCOLES" ~ "MIERCOLES",
                              D2018$DIA_NOMBRE=="JUEVES   " ~ "JUEVES",
                              D2018$DIA_NOMBRE=="VIERNES  " ~ "VIERNES",
                              D2018$DIA_NOMBRE=="SÃ\u0081BADO   " ~ "SABADO",
                              D2018$DIA_NOMBRE=="DOMINGO  " ~ "DOMINGO",
                              TRUE ~ "raro")

#Nombre del mes 
D2018$MES <-case_when( D2018$MES=="1" ~ "ENERO",
                       D2018$MES=="2" ~ "FEBRERO",
                       D2018$MES=="3" ~ "MARZO",
                       D2018$MES=="4" ~ "ABRIL",
                       D2018$MES=="5" ~ "MAYO",
                       D2018$MES=="6" ~ "JUNI",
                       D2018$MES=="7" ~ "JULIO",
                       D2018$MES=="8" ~ "AGOSTO",
                       D2018$MES=="9" ~ "SEPTIEMBRE",
                       D2018$MES=="10" ~ "OCTUBRE",
                       D2018$MES=="11" ~ "NOVIEMBRE",
                       D2018$MES=="12" ~ "DICIEMBRE",
                       TRUE ~ "raro")


######Clase
D2018$CLASE <-case_when( D2018$CLASE=="Atropello" ~ "Atropello",
                         D2018$CLASE=="CaÃ???da de Ocupante" ~ "Caida de Ocupante",
                         D2018$CLASE=="Choque" ~ "Choque",
                         D2018$CLASE=="Incendio" ~ "Incendio",
                         D2018$CLASE=="Otro" ~ "Otros",
                         D2018$CLASE=="Volcamiento" ~ "Volcamiento",
                         D2018$CLASE=="Caida de Ocupante" ~ "Caida de Ocupante",
                         D2018$CLASE=="Caida Ocupante" ~ "Caida de Ocupante",
                         D2018$CLASE=="CaÃ???da Ocupante" ~ "Caida de Ocupante",
                         D2018$CLASE=="Choque " ~ "Choque",
                         D2018$CLASE=="Choque y Atropello" ~ "Otros",
                         TRUE ~ "Otros")


####Gravedad 
D2018$GRAVEDAD <-case_when( D2018$GRAVEDAD=="HERIDO" ~ "HERIDO",
                            D2018$GRAVEDAD=="MUERTO" ~ "MUERTO",
                            D2018$GRAVEDAD=="SOLO DAÃ'OS" ~ "SOLO_DANOS",
                            TRUE ~ "raro")


#####Tramo_de_via
D2018$DISENO <-case_when( D2018$DISENO=="Ciclo Ruta" ~ "Ciclo Ruta",
                          D2018$DISENO=="Glorieta" ~ "Glorieta",
                          D2018$DISENO=="Choque" ~ "Choque",
                          D2018$DISENO=="Interseccion" ~ "Interseccion",
                          D2018$DISENO=="Lote o Predio" ~ "Lote o Predio",
                          D2018$DISENO=="Paso a Nivel" ~ "Paso a Nivel",
                          D2018$DISENO=="Paso Elevado" ~ "Paso Elevado",
                          D2018$DISENO=="Paso Inferior" ~ "Paso Inferior",
                          D2018$DISENO=="Puente" ~ "Puente",
                          D2018$DISENO=="Tramo de via" ~ "Tramo de via",
                          D2018$DISENO=="Tunel" ~ "Tunel",
                          D2018$DISENO=="Via peatonal" ~ "Via peatonal",
                          TRUE ~ "Otros")




df_2018 <- inner_join(D2018,fechaEspecial,by ="FECHA")
colnames(D2018)
columnas <- c('DIA','DIA_NOMBRE','MES','CLASE','DISENO','ESPECIAL')

df_2018 <- df_2018[,columnas] 

tabla_2018 <- 
  df_2018 %>% group_by(DIA,DIA_NOMBRE,MES,CLASE,DISENO, ESPECIAL)%>% summarise(conteo=n())
###########otros cambios entrenamiento
entrenamientofin<-as.data.frame(tabla)
entrenamientofin$CLASE <-case_when( entrenamientofin$CLASE=="Atropello" ~ "Atropello",
                                    entrenamientofin$CLASE=="CaÃ???da de Ocupante" ~ "Caida de Ocupante",
                                    entrenamientofin$CLASE=="Choque" ~ "Choque",
                                    entrenamientofin$CLASE=="Incendio" ~ "Otros",
                                    entrenamientofin$CLASE=="Otro" ~ "Otros",
                                    entrenamientofin$CLASE=="Volcamiento" ~ "Volcamiento",
                                    entrenamientofin$CLASE=="Caida de Ocupante" ~ "Caida de Ocupante",
                                    entrenamientofin$CLASE=="Caida Ocupante" ~ "Caida de Ocupante",
                                    entrenamientofin$CLASE=="CaÃ???da Ocupante" ~ "Caida de Ocupante",
                                    entrenamientofin$CLASE=="Choque " ~ "Choque",
                                    entrenamientofin$CLASE=="Choque y Atropello" ~ "Otros",
                                    TRUE ~ "Otros")

entrenamientofin$DIA<-as.factor(entrenamientofin$DIA)
entrenamientofin$DIA_NOMBRE<-as.factor(entrenamientofin$DIA_NOMBRE)
entrenamientofin$MES<-as.factor(entrenamientofin$MES)
entrenamientofin$CLASE<-as.factor(entrenamientofin$CLASE)
entrenamientofin$DISENO<-as.factor(entrenamientofin$DISENO)
entrenamientofin$ESPECIAL<-as.factor(entrenamientofin$ESPECIAL)
entrenamientofin$conteo<-as.integer(entrenamientofin$conteo)



###########otros cambios validacion
validacion<-as.data.frame(tabla_2018)
validacion<-as.data.frame(tabla)
validacion$CLASE <-case_when( validacion$CLASE=="Atropello" ~ "Atropello",
                              validacion$CLASE=="CaÃ???da de Ocupante" ~ "Caida de Ocupante",
                              validacion$CLASE=="Choque" ~ "Choque",
                              validacion$CLASE=="Incendio" ~ "Otros",
                              validacion$CLASE=="Otro" ~ "Otros",
                              validacion$CLASE=="Volcamiento" ~ "Volcamiento",
                              validacion$CLASE=="Caida de Ocupante" ~ "Caida de Ocupante",
                              validacion$CLASE=="Caida Ocupante" ~ "Caida de Ocupante",
                              validacion$CLASE=="CaÃ???da Ocupante" ~ "Caida de Ocupante",
                              validacion$CLASE=="Choque " ~ "Choque",
                              validacion$CLASE=="Choque y Atropello" ~ "Otros",
                                    TRUE ~ "Otros")

validacion$DIA<-as.factor(validacion$DIA)
validacion$DIA_NOMBRE<-as.factor(validacion$DIA_NOMBRE)
validacion$MES<-as.factor(validacion$MES)
validacion$CLASE<-as.factor(validacion$CLASE)
validacion$DISENO<-as.factor(validacion$DISENO)
validacion$ESPECIAL<-as.factor(validacion$ESPECIAL)
validacion$conteo<-as.integer(validacion$conteo)



#########################MODELO RANDOM FOREST  ##############################
library(randomForest)
library(e1071)
library(h2o)


set.seed(88)
modelo1<-randomForest(conteo~.,data =entrenamientofin)




prediccion1<-predict(modelo1,validacion)




resultados1<-data.frame(validacion,prediccion1,2018)
names(resultados1)[9] = "PERIODO"

  
resultados1$MES_NUMERO<-case_when( resultados1$MES=="ENERO" ~ "1",
                             resultados1$MES=="FEBRERO" ~ "2",
                             resultados1$MES=="MARZO" ~ "3",
                             resultados1$MES=="ABRIL" ~ "4",
                             resultados1$MES=="MAYO" ~ "5",
                             resultados1$MES=="JUNIO" ~ "6",
                             resultados1$MES=="JULIO" ~ "7",
                             resultados1$MES=="AGOSTO" ~ "8",
                             resultados1$MES=="SEPTIEMBRE" ~ "9",
                             resultados1$MES=="OCTUBRE" ~ "10",
                             resultados1$MES=="NOVIEMBRE" ~ "11",
                             resultados1$MES=="DICIEMBRE" ~ "12",
                             TRUE ~ "raro") 
  

resultados2<-unite(resultados1, FECHA,c(9,10,1),  sep = "-", remove = FALSE)

resultados2$FECHA<- as.Date(resultados2$FECHA)
resultados2$prediccion1 <- ceiling(resultados2$prediccion1)
resultados2$semana<- week(resultados2$FECHA)


final <- subset(resultados2, FECHA >= "2018-01-01" & FECHA <= "2018-05-01")





mensual<- dcast(final,MES~CLASE,sum,value.var="prediccion1")

semanal <- dcast(final,semana ~CLASE,sum,value.var="prediccion1")

diario <- dcast(final,FECHA~CLASE,sum,value.var="prediccion1")








prubmse<-((resultados1$conteo)-(resultados1$prediccion1))^2
prubmse<-as.vector(prubmse)
mean(prubmse)



DIA <- 31
DIA_NOMBRE<-"DOMINGO"
MES<-"FEBRERO"
CLASE<-"Atropello"
DISENO<-"Puente"
ESPECIAL<-0
conteo <-(0)

class(conteo)

prueba <- data.frame (DIA,DIA_NOMBRE, MES, CLASE, DISENO, ESPECIAL,conteo)

fff <- rbind(validacion[1,],prueba)

Predict(modelo1, fff)
########################### SOLO PARA INFORME ######################





#########################MODELO MAQUINA SOPORTE VECTORIAL ##############################

modelo2<-svm(conteo~.,data =entrenamientofin,kernel="linear",cost=10,scale = FALSE)
prediccion2<-predict(modelo2,validacion)
resultados2<-data.frame(validacion,prediccion2)
prubmse2<-((resultados2$prediccion2)-(resultados2$conteo))^2
prubmse2<-as.vector(prubmse2)
mean(prubmse2)



######################### MODELO LINEAL GENERALIZADO  ##############################


modelo3<-glm(conteo~.,data =entrenamientofin)
prediccion3<-predict(modelo3,validacion)
resultados3<-data.frame(validacion,prediccion3)
prubmse3<-((resultados3$prediccion3)-(resultados3$conteo))^2
prubmse3<-as.vector(prubmse3)
mean(prubmse3)



######################### MODELO RED NEURONAL  ##############################
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = 'conteo',
                              training_frame = as.h2o(entrenamientofin),
                              activation = 'Rectifier',
                              hidden = c(5, 5),
                              epochs = 100,
                              train_samples_per_iteration = -2)
modelo4<-neuralnet(conteo~.,data =entrenamientofin)
prob_pred <- h2o.predict(classifier, newdata = as.h2o(validacion))

prueba1<-as.data.frame(prob_pred)
resultados4<-data.frame(validacion,prueba1$predict)
prubmse4<-((resultados4$prueba1.predict)-(resultados4$conteo))^2
prubmse4<-as.vector(prubmse4)
mean(prubmse4)


