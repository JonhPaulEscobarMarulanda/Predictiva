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
library(rgdal)
library(leaflet)
library(plyr)



D2014<-read.csv(file.choose(""),sep=",",header = TRUE)
D2015<-read.csv(file.choose(""),sep=",",header = TRUE)
D2016<-read.csv(file.choose(""),sep=",",header = TRUE)
D2017<-read.csv(file.choose(""),sep=",",header = TRUE)

###### concatenación de los datos
entrenamientomod<-rbind(D2014,D2015,D2016,D2017)

entrenamientomod$MES<-as.factor(entrenamientomod$MES)
entrenamientomod$DIA<-as.factor(entrenamientomod$DIA)
###nombre del dia 
entrenamientomod$DIA_NOMBRE <-case_when( entrenamientomod$DIA_NOMBRE=="LUNES    " ~ "LUNES",
                                         entrenamientomod$DIA_NOMBRE=="MARTES   " ~ "MARTES",
                                         entrenamientomod$DIA_NOMBRE=="MIÃ???RCOLES" ~ "MIERCOLES",
                                         entrenamientomod$DIA_NOMBRE=="JUEVES   " ~ "JUEVES",
                                         entrenamientomod$DIA_NOMBRE=="VIERNES  " ~ "VIERNES",
                                         entrenamientomod$DIA_NOMBRE=="SÃ\u0081BADO   " ~ "SABADO",
                                         entrenamientomod$DIA_NOMBRE=="DOMINGO  " ~ "DOMINGO",
                                         TRUE ~ "raro")
entrenamientomod$DIA_NOMBRE<-as.factor(entrenamientomod$DIA_NOMBRE)
###nombre del mes 
entrenamientomod$MES <-case_when( entrenamientomod$MES=="1" ~ "ENERO",
                                  entrenamientomod$MES=="2" ~ "FEBRERO",
                                  entrenamientomod$MES=="3" ~ "MARZO",
                                  entrenamientomod$MES=="4" ~ "ABRIL",
                                  entrenamientomod$MES=="5" ~ "MAYO",
                                  entrenamientomod$MES=="6" ~ "JUNI",
                                  entrenamientomod$MES=="7" ~ "JULIO",
                                  entrenamientomod$MES=="8" ~ "AGOSTO",
                                  entrenamientomod$MES=="9" ~ "SEPTIEMBRE",
                                  entrenamientomod$MES=="10" ~ "OCTUBRE",
                                  entrenamientomod$MES=="11" ~ "NOVIEMBRE",
                                  entrenamientomod$MES=="12" ~ "DICIEMBRE",
                                  TRUE ~ "raro")
entrenamientomod$MES<-as.factor(entrenamientomod$MES)

###### clase
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
                                    entrenamientomod$CLASE=="Choque y Atropello" ~ "Choque con Atropello",
                                    TRUE ~ "Otros")
entrenamientomod$CLASE<-as.factor(entrenamientomod$CLASE)

####gravedad 
entrenamientomod$GRAVEDAD <-case_when( entrenamientomod$GRAVEDAD=="HERIDO" ~ "HERIDO",
                                       entrenamientomod$GRAVEDAD=="MUERTO" ~ "MUERTO",
                                       entrenamientomod$GRAVEDAD=="SOLO DAÃ'OS" ~ "SOLO_DANOS",
                                       TRUE ~ "raro")
entrenamientomod$GRAVEDAD<-as.factor(entrenamientomod$GRAVEDAD)

#####tramo_de_via
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
entrenamientomod$DISENO<-as.factor(entrenamientomod$DISENO)

##### comuna
entrenamientomod$COMUNA <-case_when( entrenamientomod$COMUNA=="Aranjuez" ~ "Aranjuez",
                                     entrenamientomod$COMUNA=="BelÃ©n" ~ "Belen",
                                     entrenamientomod$COMUNA=="Buenos Aires" ~ "Buenos Aires",
                                     entrenamientomod$COMUNA=="Castilla" ~ "Castilla",
                                     entrenamientomod$COMUNA=="Corregimiento de Altavista" ~ "Corregimiento de Altavista",
                                     entrenamientomod$COMUNA=="Corregimiento de San Antonio de Prado" ~ "Corregimiento de San Antonio de Prado",
                                     entrenamientomod$COMUNA=="Corregimiento de San CristÃ³bal" ~ "san cristobal",
                                     entrenamientomod$COMUNA=="Corregimiento de Santa Elena" ~ "Corregimiento de Santa Elena",
                                     entrenamientomod$COMUNA=="Doce de Octubre" ~ "Doce de Octubre",
                                     entrenamientomod$COMUNA=="El Poblado" ~ "El Poblado",
                                     entrenamientomod$COMUNA=="La AmÃ©rica" ~ "La America",
                                     entrenamientomod$COMUNA=="La Candelaria" ~ "La Candelaria",
                                     entrenamientomod$COMUNA=="Laureles Estadio" ~ "Laureles",
                                     entrenamientomod$COMUNA=="Manrique" ~ "Manrique",
                                     entrenamientomod$COMUNA=="Popular" ~ "Popular",
                                     entrenamientomod$COMUNA=="Robledo" ~ "Robledo",
                                     entrenamientomod$COMUNA=="San Javier" ~ "San Javier",
                                     entrenamientomod$COMUNA=="Santa Cruz" ~ "Santa Cruz",
                                     entrenamientomod$COMUNA=="Villa Hermosa" ~ "Villa Hermosa",
                                     entrenamientomod$COMUNA=="Corregimiento de San SebastiÃ¡n de Palmitas" ~ "palmitas",
                                     entrenamientomod$COMUNA=="Altavista" ~ "Altavista",
                                     entrenamientomod$COMUNA=="AndalucÃ???a" ~ "Andalucia",
                                     entrenamientomod$COMUNA=="Bolivariana" ~ "Bolivariana",
                                     entrenamientomod$COMUNA=="Boston" ~ "Boston",
                                     entrenamientomod$COMUNA=="Cabecera San Antonio de Prado" ~ "Corregimiento de San Antonio de Prado",
                                     entrenamientomod$COMUNA=="Calasanz" ~ "Calasanz",
                                     entrenamientomod$COMUNA=="Calle Nueva" ~ "Calle Nueva",
                                     entrenamientomod$COMUNA=="Campo Amor" ~ "Campo Amor",
                                     entrenamientomod$COMUNA=="Campo ValdÃ©s No. 1" ~ "Campo valdes",
                                     entrenamientomod$COMUNA=="Campo ValdÃ©s No. 2" ~ "Campo valdes",
                                     entrenamientomod$COMUNA=="Caribe" ~ "Caribe",
                                     entrenamientomod$COMUNA=="Cerro Nutibara" ~ "Cerro Nutibara",
                                     entrenamientomod$COMUNA=="CorazÃ³n de JesÃºs" ~ "Corazon de Jesus",
                                     entrenamientomod$COMUNA=="Cristo Rey" ~ "Cristo Rey",
                                     entrenamientomod$COMUNA=="El Chagualo" ~ "El Chagualo",
                                     entrenamientomod$COMUNA=="El Raizal" ~ "El Raizal",
                                     entrenamientomod$COMUNA=="FÃ¡tima" ~ "fatima",
                                     entrenamientomod$COMUNA=="Girardot" ~ "Girardot",
                                     entrenamientomod$COMUNA=="Guayaquil" ~ "Guayaquil",
                                     entrenamientomod$COMUNA=="HÃ©ctor Abad GÃ³mez" ~ "Hecthor Abad Gomez",
                                     entrenamientomod$COMUNA=="JesÃºs Nazareno" ~ "Jesus Nazareno",
                                     entrenamientomod$COMUNA=="La Alpujarra" ~ "La Alpujarra",
                                     entrenamientomod$COMUNA=="La Floresta" ~ "La Floresta",
                                     entrenamientomod$COMUNA=="La Rosa" ~ "La Rosa",
                                     entrenamientomod$COMUNA=="Las Esmeraldas" ~ "Las Esmeraldas",
                                     entrenamientomod$COMUNA=="Las Palmas" ~ "Las Palmas",
                                     entrenamientomod$COMUNA=="Laureles" ~ "Laureles",
                                     entrenamientomod$COMUNA=="Los Conquistadores" ~ "Los Conquistadores",
                                     entrenamientomod$COMUNA=="Los Mangos" ~ "Los Mangos",
                                     entrenamientomod$COMUNA=="Manila" ~ "Manila",
                                     entrenamientomod$COMUNA=="Moravia" ~ "Moravia",
                                     entrenamientomod$COMUNA=="Naranjal" ~ "Naranjal",
                                     entrenamientomod$COMUNA=="Patio Bonito" ~ "Patio Bonito",
                                     entrenamientomod$COMUNA=="Perpetuo Socorro" ~ "Perpetuo Socorro",
                                     entrenamientomod$COMUNA=="Rosales" ~ "Rosales",
                                     entrenamientomod$COMUNA=="Suramericana" ~ "Suramericana",
                                     entrenamientomod$COMUNA=="Toscana" ~ "Toscana",
                                     entrenamientomod$COMUNA=="Veinte de Julio" ~ "Veinte de Julio",
                                     TRUE ~ "Otros")
entrenamientomod$COMUNA<-as.factor(entrenamientomod$COMUNA)

############ acomodo tabla para clust

ftable(entrenamientomod$COMUNA,entrenamientomod$GRAVEDAD)
a<-data.frame(ftable(entrenamientomod$COMUNA,entrenamientomod$GRAVEDAD))

a2<-spread(a,Var2,Freq)
entrenamientomod<-entrenamiento[,-c(1,2,3,4,5,6,8,10,11,12,13,20,21,22,23,24)]

####datimp
a1<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$GRAVEDAD))
a2<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$CLASE))
a3<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$MES))
a4<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$DIA_NOMBRE))
a5<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$DISENO))
a6<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$DIA))
####datspread
a1t<-spread(a1,Var2,Freq)
a2t<-spread(a2,Var2,Freq)
a3t<-spread(a3,Var2,Freq)
a4t<-spread(a4,Var2,Freq)
a5t<-spread(a5,Var2,Freq)
a6t<-spread(a6,Var2,Freq)

base_lista1<-merge(a1t,a2t,by="Var1",all.x = TRUE)
base_lista2<-merge(base_lista1,a3t,by="Var1",all.x = TRUE)
base_lista3<-merge(base_lista2,a4t,by="Var1",all.x = TRUE)
base_lista4<-merge(base_lista3,a5t,by="Var1",all.x = TRUE)
base_final<-merge(base_lista4,a6t,by="Var1",all.x = TRUE)

### numero de cluster
base_final<-base_final[-c(1,2,3,4),]
base_final->seg
seg<-na.omit(seg)
seg<-seg[,-c(1)]
seg<-na.omit(seg)

set.seed(20200604)
wss<-(nrow(seg)-1)*sum(apply(seg,2,var))
for (i in 1:35) wss[i] <- sum(kmeans(seg,centers = i)$withinss)
plot(1:35, wss, type="b", xlab="#decluster",ylab="sumcuadgrup")

fit1<-kmeans(seg,5)
car_fit1_mean<-aggregate(seg, by = list(fit1$cluster), FUN = mean)
car_fit1_sd<-aggregate(seg, by = list(fit1$cluster), FUN = sd)

#### asignacion de datos al cluster
seg1<-data.frame(seg,fit1$cluster)

###### graficos iniciales
par(mar=c(5,4,6,2))
height<-table(fit1$cluster)
mp<-barplot(height,main = "distribucion barrios")
text(mp,height,labels=format(height,2),pos=1,cex = 0.9)

#plotcluster(seg1,fit1$cluster)
id_cluster<-as.data.frame(fit1$cluster)

Base<-cbind(base_final,id_cluster)
Base<-Base[,c(1,74)]
names (Base)[1] <- "BARRIO"
names (Base)[2] <- "CLUSTER"


###### PROMEDIO DE ACCIDENTES AÑO BARRIO

totalano<-data.frame(ftable(entrenamientomod$BARRIO,entrenamientomod$PERIODO))
names (totalano)[1] <- "BARRIO"
names (totalano)[2] <- "PERIODO"
names (totalano)[3] <- "FRECUENCIA"

prom_barrio<-data.frame(aggregate(x = totalano$FRECUENCIA, by = list(totalano$BARRIO), FUN = "mean"))
names (prom_barrio)[1] <- "BARRIO"
names (prom_barrio)[2] <- "PROMEDIO"

####### UNION DE BASE CLUSTER - PROMEDIO ACCIDENTES
base1 <- join(Base, prom_barrio, by="BARRIO")




#### PROMEDIO ACCIDENTES POR CLUSTER

Base$CLUSTER <- as.factor(Base$CLUSTER)
entrenam <- join(entrenamientomod, Base, by="BARRIO")

totalcluster <-data.frame(ftable(entrenam$CLUSTER,entrenam$PERIODO))
names (totalcluster)[1] <- "CLUSTER"
names (totalcluster)[2] <- "PERIODO"
names (totalcluster)[3] <- "FRECUENCIA"

prom_cluster<-data.frame(aggregate(x = totalcluster$FRECUENCIA, by = list(totalcluster$CLUSTER), FUN = "mean"))
names (prom_cluster)[1] <- "CLUSTER"
names (prom_cluster)[2] <- "PROMEDIO"

####### UNION DE BASE CLUSTER - PROMEDIO ACCIDENTES
base2 <- join(base1, prom_cluster, by="CLUSTER")


setwd("C:/Users/dayan/Desktop/NUEVA APP") 
write.csv(base2, file="Barrio_Cluster.csv")
