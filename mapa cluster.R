library(rgdal)
library(leaflet)
library(plyr)


barrios_med  <- shapefile("Barrio_Vereda.shp",encoding="UTF-8",use_iconv=TRUE)
datos_barrios <- barrios_med@data
names (datos_barrios)[3] <- "BARRIO"
datos_barrios$BARRIO <- iconv(datos_barrios$BARRIO, to="ASCII//TRANSLIT")


barrios_categorias <- read.csv("Barrio_Cluster.csv", header = TRUE , sep = ",")

barrios_categorias<- barrios_categorias[,c(2,3,4,5)]
names (barrios_categorias)[1] = "BARRIO"
names (barrios_categorias)[2] = "CATEGORIA"
names (barrios_categorias)[3] = "PROM_BARRIO"
names (barrios_categorias)[4] = "PROM_CLUSTER"

barrios_categorias$BARRIO <- iconv(barrios_categorias$BARRIO, to="ASCII//TRANSLIT")
datos_listos <- join(datos_barrios, barrios_categorias)




datos_listos$CATEGORIA <- gsub(1,"orange", datos_listos$CATEGORIA )
datos_listos$CATEGORIA <- gsub(2,"red", datos_listos$CATEGORIA )
datos_listos$CATEGORIA <- gsub(3,"green", datos_listos$CATEGORIA )
datos_listos$CATEGORIA <- gsub(4,"yellow", datos_listos$CATEGORIA )
datos_listos$CATEGORIA <- gsub(5,"blue", datos_listos$CATEGORIA )


state_popup <- paste0("<strong>Barrio: </strong>", 
                      datos_listos$BARRIO,
                      "<br><strong>Promedio Anual Accidentes Barrio: </strong>", 
                      datos_listos$PROM_BARRIO,
                      "<br><strong>Promedio Anual Accidentes Cluster: </strong>", 
                      datos_listos$PROM_CLUSTER
                      )


barrios_med@data$NOMBRE <- iconv(barrios_med@data$NOMBRE, to="ASCII//TRANSLIT")
barrios_med@data <- datos_listos
m=leaflet(barrios_med) 
m=addTiles(m)
m=addPolygons(m,popup=state_popup,color=barrios_med@data$CATEGORIA)
m







barrios_med  <- shapefile("Barrio_Vereda.shp",encoding="UTF-8",use_iconv=TRUE)
datos_barrios <- barrios_med@data
names (datos_barrios)[3] <- "BARRIO"
datos_barrios$BARRIO <- iconv(datos_barrios$BARRIO, to="ASCII//TRANSLIT")
barrios_categorias <- read.csv("Categorias.csv", header = TRUE , sep = ";")
barrios_categorias$BARRIO <- iconv(barrios_categorias$BARRIO, to="ASCII//TRANSLIT")
datos_listos <- join(datos_barrios, barrios_categorias)






