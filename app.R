library(shiny)
library(dplyr)
library(rgdal)
library(leaflet)
library(plyr)
library(lubridate)
library(DT)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(raster)
library(sp)
library(ggfortify)
library(plotly)
library(randomForest)
library(reshape2)
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


datos2014<-read.csv("Accidentalidad_georreferenciada_2014.csv")
datos2015<-read.csv("Accidentalidad_georreferenciada_2015.csv")
datos2016<-read.csv("Accidentalidad_georreferenciada_2016.csv")
datos2017<-read.csv("Accidentalidad_georreferenciada_2017.csv")
datos2018<-read.csv("Accidentalidad_georreferenciada_2018.csv")
randomresult<-read.csv("resultados1.csv")
fechaEspecial <- read.csv("Fechas_Especiales.csv")
youtube_video <- '<iframe width="560" height="315" src="https://www.youtube.com/embed/7ZlOC666G_8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
imagen<- "https://i.servimg.com/u/f64/13/84/08/18/logo_f10.jpg"
imagen2<-"https://sites.google.com/site/mosaigrupo/_/rsrc/1264045902848/config/app/images/customLogo/customLogo.gif?revision=1"


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





server <- function(input, output) {
 
############ DEFINIMOS LA TABLA A TOMAR PARA LA INFORMACION 
  totaldata<-reactive({
    
    d<-rbind(datos2014,datos2015,datos2016,datos2017)
    
    d$BARRIO=iconv(d$BARRIO,"UTF-8","ISO_8859-1")
    d$BARRIO<-as.factor(d$BARRIO)
    
    d$CLASE=iconv(d$CLASE,"UTF-8","ISO_8859-1")
    d$CLASE<-as.factor(d$CLASE)
    d$CLASE <- gsub("Caida de Ocupante", "Caida Ocupante",d$CLASE)
    d$CLASE <- gsub("Caída de Ocupante", "Caida Ocupante", d$CLASE)
    d$CLASE <- gsub("Caída Ocupante", "Caida Ocupante", d$CLASE)
    d$CLASE <- gsub("Choque y Atropello", "Otro", d$CLASE)
    d$CLASE <- gsub("Incendio", "Otro", d$CLASE)
    d$CLASE <- gsub("Choque ", "Choque", d$CLASE)
    

    
    d$CLASE <-case_when( d$CLASE=="Caida Ocupante" ~ "Caida Ocupante",
                         d$CLASE=="Choque" ~ "Choque",
                         d$CLASE=="Atropello" ~ "Atropello",
                         d$CLASE=="Volcamiento" ~ "Volcamiento",
                         d$CLASE=="Otro" ~ "Otro",
                         TRUE ~ "Otro")
    
    
    d$CLASE<-as.factor(d$CLASE)
    
    
    d$TIPO_GEOCOD=iconv(d$TIPO_GEOCOD,"UTF-8","ISO_8859-1")
    d$TIPO_GEOCOD<-as.factor(d$TIPO_GEOCOD)
    
    d$GRAVEDAD=iconv(d$GRAVEDAD,"UTF-8","ISO_8859-1")
    d$GRAVEDAD<-as.factor(d$GRAVEDAD)
    
    
    d$COMUNA=iconv(d$COMUNA,"UTF-8","ISO_8859-1")
    d$COMUNA<-as.factor(d$COMUNA)
    
    
    
    d$DISENO=iconv(d$DISENO,"UTF-8","ISO_8859-1")
    d$DISENO<-as.factor(d$DISENO)
    
    d$MES<-as.factor(d$MES)
    d$DIA<-as.factor(d$DIA)
    d$PERIODO<-as.factor(d$PERIODO)
    
    d$CANTIDAD<-(c(1))
    
    d<-d[,c(7,8,9,13,14,15,16,17,18,19,25,5)]
    
    d$DIA_NOMBRE <-case_when( d$DIA_NOMBRE=="LUNES    " ~ "2.LUNES",
                              d$DIA_NOMBRE=="MARTES   " ~ "3.MARTES",
                              d$DIA_NOMBRE=="MIÃ‰RCOLES" ~ "4.MIERCOLES",
                              d$DIA_NOMBRE=="JUEVES   " ~ "5.JUEVES",
                              d$DIA_NOMBRE=="VIERNES  " ~ "6.VIERNES",
                              d$DIA_NOMBRE=="SÃ\u0081BADO   " ~ "7.SABADO",
                              d$DIA_NOMBRE=="DOMINGO  " ~ "1.DOMINGO",
                              TRUE ~ "raro")
    d$DIA_NOMBRE<-as.factor(d$DIA_NOMBRE)
    d$FECHA<-as.Date(d$FECHA)
    
    d
  })
  
  
  
  
  ########## DEFINIMOS EL PRIMER FILTRO DE CLASE
  output$CLASE <- renderUI({
    selectInput ("CLASE_ACCIDENTE", 
                 "CLASE ACCIDENTE:", 
                 sort(unique(totaldata()[,3]), decreasing = FALSE),
                 selected="Choque",
                 multiple =FALSE)
  })
  
  
  
  
  ########## SACAMOS UN SUMMARY INICIAL 
  output$summary <- renderPrint({
    dataset <- totaldata()[,c(1,2,3,4,5,6,7,8,9,10)]
    summary(dataset)
  })
  
  
  ######## DEFINIMOS LA TABLA HISTORICA DE INFORMACIN
  
  tabla<-reactive({
    filtro <- filter(totaldata(), FECHA >= input$dates[1]  & FECHA <= input$dates[2])
    filtro
    ## subset(totaldata(), PERIODO == input$Anio)
    
  })
  
  tablasum<-reactive({
    
    filter(tabla(), CLASE == input$CLASE_ACCIDENTE)
  })
  
  
  ###### IMPRIMIR TABLA FINAL 
  
  output$view <- renderTable({
    dataset <- tablasum()
    dataset [1:500,1:10]
  })
  
  
  
  
  ######  TS
  
  output$TS <- renderPlotly({
    aggregate(CANTIDAD~PERIODO*MES,data=totaldata(),FUN=sum)%>%
      plot_ly(x = ~MES,
              y = ~CANTIDAD,
              type = "scatter" ,mode = "lines+markers",
              split = ~PERIODO,
              line=list(width=2))%>%
      layout(title='Accidentes por Año y Mes',
             xaxis=list(title="Mes"),
             yaxis=list(title="# Accidentes"))
    
  })
  
  
  
  
  #### GRAFICA BARRAS ACCIDENTES VS AÑO
  
  
  output$ggploacc <- renderPlotly({
    
    qplot(CLASE,data=tabla(),
          main="Tipo de accidentes",
          ylab = "Numero eventos", 
          xlab="Tipo de accidentes") + geom_bar(aes(fill=CLASE),colour="black",    # Black outline for all
                                                position=position_dodge(),show.legend = FALSE)+
      stat_count(geom = "text",aes(label=..count..,group=CLASE,vjust=-0.8))
  })
  
  
  
  
  ##### GRAFICA BARRAS DIAS VS CLASE
  
  output$diavsclas <- renderPlotly({
    
    ggplot(tabla(),aes(x=DIA_NOMBRE,fill=CLASE))+geom_bar()+
      stat_count(geom = "text",aes(label=..count..,group=CLASE,vjust=0.8))+
      ggtitle("Tipo de accidente según el dia")
    
  })
  
  
  ##### GRAFICA BARRAS GRAVEDAD VS CLASE
  
  output$GRAVvsclas <- renderPlotly({
    
    ggplot(tabla(),aes(x=CLASE,fill=GRAVEDAD))+geom_bar()+
      stat_count(geom = "text",aes(label=..count..,group=CLASE,vjust=0.8))+
      ggtitle("Tipo de accidente según gravedad")
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ################# ###################################################FILTROS Y MODELO RANDOM FOREST ################
  


    resulfiltr<-reactive({
      resultados1<-data.frame(randomresult,2018)
      names(resultados1)[10] <- "PERIODO"
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
      resultados2<-unite(resultados1, FECHA,c(10,11,2),  sep = "-", remove = FALSE)
      resultados2$FECHA<- as.Date(resultados2$FECHA)
      resultados2$prediccion1 <- ceiling(resultados2$prediccion1)
      resultados2$semana<- week(resultados2$FECHA)
      final <-  filter(resultados2, FECHA >= input$dates2[1]  & FECHA <= input$dates2[2])
      final
 
    })


    
  
  output$mensual <- renderTable({
    
    mensual<- dcast(resulfiltr(),MES_NUMERO + MES~CLASE,sum,value.var="prediccion1")
    mensual$MES_NUMERO <- as.numeric(mensual$MES_NUMERO)
    
    mensual <-mensual[order(mensual$MES_NUMERO),]
    
    mensual[,2:dim(mensual)[2]]
  })
  
  output$semanal <- renderTable({
    
    semanal<- dcast(resulfiltr(),semana~CLASE,sum,value.var="prediccion1")
    
    semanal$semana <- round(as.integer(semanal$semana), digits = 0)
    
    semanal <-semanal[order(semanal$semana),]
    semanal
  })
  
  output$diario <- renderTable({
    
    diario<- dcast(resulfiltr(),MES_NUMERO +DIA  + MES ~CLASE,sum,value.var="prediccion1")
    diario$MES_NUMERO <- as.numeric(diario$MES_NUMERO)
    diario$DIA <- as.numeric(diario$DIA)
    
    diario <-diario[order(diario$MES_NUMERO,diario$ DIA),]
    
    diario2<-unite(diario, DIA_MES,c(2,3),  sep = "/", remove = TRUE)
    
    diario2[,2:dim(diario2)[2]]
  })
  
  
  
  

  
  
  
 
  
  
  #### AGREGAR MAPA CON CLUSTERS
  output$mapa <- renderLeaflet({
    
    
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
    
  })
  
  
  

  
}








ui <- fluidPage(
  

navbarPage(title = 'Accidentalidad Medellin',
           theme=shinytheme("flatly"),

  
  
  tabPanel(title = "VIDEO PROMOCIONAL",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
        
           fluidRow(align='center',h3("VIDEO PROMOCIONAL APP ACCIDENTABILIDAD"),br(),
                    box(align='center', HTML(youtube_video),width=700))
           ),
  
  
  
  tabPanel(title = "VISUALIZACIÓN",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","INFORMACIÓN HISTORICA ACCIDENTES"), align = "center"),
           
           
           sidebarLayout(
             
             sidebarPanel(
               
               uiOutput("CLASE"),
               helpText("NOTA: El filtro clase accidente solo interactuctua con la información TABLA
                          HISTORICA"),
               dateRangeInput("dates",
                              "Rango fechas",
                              start = "2014-01-01",
                              end = "2014-12-31",
                              min = "2014-01-01",
                              max = "2017-12-31",
                              separator = " - ")
             ),
             
             
             mainPanel(tabsetPanel(type = "pills",
                                   
                                   tabPanel("GRAFICAS DESCRIPTIVAS",
                                            verbatimTextOutput("summary"),
                                            plotlyOutput("ggploacc"),
                                            plotlyOutput("diavsclas"),
                                            plotlyOutput("GRAVvsclas"),
                                            plotlyOutput("TS")
                                            
                                   ),
                                   tabPanel("TABLAS HISTORICAS",
                                            tableOutput("view"))
                                   
                                   
             ))
             
             
           )
  ),
  
 
  tabPanel(title = "PREDICCIÓN",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","PREDICCIÓN ACCIDENTES"), align = "center"),
           
           
           sidebarLayout(
             
             sidebarPanel(
               
               dateRangeInput("dates2",
                              "Rango fecha predicción",
                              start = "2018-01-01",
                              end = "2018-01-15",
                              min = "2018-01-01",
                              max = "2018-12-31",
                              separator = " - ")
             ),
             
             
             mainPanel(tabsetPanel(type = "pills",
                                   
                                   tabPanel("PREDICCIÓN MENSUAL",
                                            tableOutput("mensual"),
                                  a(p(style = "color:navy; font-family:'serif'","La predicción que se presenta en 
                                  la tabla, esta basada en un modelo predictivo bajo la metodología de random forest, 
                                  el cual representa un MSE mas favorable en comparación a otras metodologías de prueba 
                                  como lo fueron, SMV, Redes neuronales y un modelo GLM (poisson).
                                  El modelo se entreno con los años de accidentabilidad del 2014 al 2017, tomando las 
                                  variables de, clase de accidente, día de la semana, mes del accidente, diseño de la 
                                  via y fechas especiales.
                                  Al final se obtiene un modelo con un MSE de 19.3.
                                  Para ver mas información detallada del modelo y cómo se comparó con las demás 
                                  metodologías se puede revisar el reporte técnico."))),
                                  
                                  tabPanel("PREDICCIÓN SEMANAL",
                                           tableOutput("semanal"),
                                           a(p(style = "color:navy; font-family:'serif'","La predicción que se presenta en 
                                  la tabla, esta basada en un modelo predictivo bajo la metodología de random forest, 
                                  el cual representa un MSE mas favorable en comparación a otras metodologías de prueba 
                                  como lo fueron, SMV, Redes neuronales y un modelo GLM (poisson).
                                  El modelo se entreno con los años de accidentabilidad del 2014 al 2017, tomando las 
                                  variables de, clase de accidente, día de la semana, mes del accidente, diseño de la 
                                  via y fechas especiales.
                                  Al final se obtiene un modelo con un MSE de 19.3.
                                  Para ver mas información detallada del modelo y cómo se comparó con las demás 
                                  metodologías se puede revisar el reporte técnico."))),
                                  
                                  
                                  tabPanel("PREDICCIÓN DIARIA",
                                           tableOutput("diario"),
                                           a(p(style = "color:navy; font-family:'serif'","La predicción que se presenta en 
                                  la tabla, esta basada en un modelo predictivo bajo la metodología de random forest, 
                                  el cual representa un MSE mas favorable en comparación a otras metodologías de prueba 
                                  como lo fueron, SMV, Redes neuronales y un modelo GLM (poisson).
                                  El modelo se entreno con los años de accidentabilidad del 2014 al 2017, tomando las 
                                  variables de, clase de accidente, día de la semana, mes del accidente, diseño de la 
                                  via y fechas especiales.
                                  Al final se obtiene un modelo con un MSE de 19.3.
                                  Para ver mas información detallada del modelo y cómo se comparó con las demás 
                                  metodologías se puede revisar el reporte técnico.")))
                                  
                                   
                                   
             ))
             
             
           )
  ),
  
  
  
  
  
  
  
  tabPanel(title = "CLUSTERIZACIÓN BARRIOS",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","AGRUPACIÓN DE BARRIOS"), align = "center"),
           mainPanel(tabsetPanel(type = "pills",
                                 
                                 tabPanel("MAPA",
                                          leafletOutput("mapa"),
                              a(p(style = "color:navy; font-family:'serif'","El presente mapa representa una 
                                  agrupación de barrios de la ciudad de Medellín, dada agrupación se realizó después 
                                  de hacer un modelo de Clusterización k-means. Las variables que se consideraron 
                                  para construir la segmentación fueron, el día de la semana y el mes donde se 
                                  presenta el suceso, la clase de accidente, la gravedad del accidente, el diseño 
                                  de la vía y por último la comuna. Después de realizar las debidas validaciones de 
                                  esta segmentación se decidió tomar un total de 5 clúster donde se verán reflejadas 
                                  las características de cada uno de los k-cluster. para más información revisar 
                                  reporte técnico."))
                                          
                                 )
                                 
                                 
           ))
           )
  
  
)
)


shinyApp(ui = ui, server = server)