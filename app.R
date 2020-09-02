library(shiny)
library(dplyr)
library(rgdal)
library(leaflet)
library(party)
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



datos2014<-read.csv("Accidentalidad_georreferenciada_2014.csv")
datos2015<-read.csv("Accidentalidad_georreferenciada_2015.csv")
datos2016<-read.csv("Accidentalidad_georreferenciada_2016.csv")
datos2017<-read.csv("Accidentalidad_georreferenciada_2017.csv")
datos2018<-read.csv("Accidentalidad_georreferenciada_2018.csv")
youtube_video <- '<iframe width="600" height="350" src="https://www.youtube.com/embed/41jmGq7ALMY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
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
    
    d<-rbind(datos2014,datos2015,datos2016,datos2017,datos2018)
    d$CLASE <-case_when( d$CLASE=="Atropello" ~ "Atropello",
                         d$CLASE=="CaÃ???da de Ocupante" ~ "Caida de Ocupante",
                         d$CLASE=="Choque" ~ "Choque",
                         d$CLASE=="Incendio" ~ "Incendio",
                         d$CLASE=="Otro" ~ "Otros",
                         d$CLASE=="Volcamiento" ~ "Volcamiento",
                         d$CLASE=="Caida de Ocupante" ~ "Caida de Ocupante",
                         d$CLASE=="Caida Ocupante" ~ "Caida de Ocupante",
                         d$CLASE=="CaÃ???da Ocupante" ~ "Caida de Ocupante",
                         d$CLASE=="Choque " ~ "Choque",
                         d$CLASE=="Choque y Atropello" ~ "Choque con Atropello",
                         TRUE ~ "Otros")
    d$CLASE<-as.factor(d$CLASE)
    d$TIPO_GEOCOD<-as.factor(d$TIPO_GEOCOD)
    d$GRAVEDAD <-case_when( d$GRAVEDAD=="HERIDO" ~ "HERIDO",
                            d$GRAVEDAD=="MUERTO" ~ "MUERTO",
                            d$GRAVEDAD=="SOLO DAÃ‘OS" ~ "SOLO_DANOS",
                                           TRUE ~ "raro")
    d$GRAVEDAD<-as.factor(d$GRAVEDAD)
    d$BARRIO<-as.factor(d$BARRIO)
    d$COMUNA <-case_when( d$COMUNA=="Aranjuez" ~ "Aranjuez",
                                         d$COMUNA=="BelÃ©n" ~ "Belen",
                                         d$COMUNA=="Buenos Aires" ~ "Buenos Aires",
                                         d$COMUNA=="Castilla" ~ "Castilla",
                                         d$COMUNA=="Corregimiento de Altavista" ~ "Corregimiento de Altavista",
                                         d$COMUNA=="Corregimiento de San Antonio de Prado" ~ "Corregimiento de San Antonio de Prado",
                                         d$COMUNA=="Corregimiento de San CristÃ³bal" ~ "san cristobal",
                                         d$COMUNA=="Corregimiento de Santa Elena" ~ "Corregimiento de Santa Elena",
                                         d$COMUNA=="Doce de Octubre" ~ "Doce de Octubre",
                                         d$COMUNA=="El Poblado" ~ "El Poblado",
                                         d$COMUNA=="La AmÃ©rica" ~ "La America",
                                         d$COMUNA=="La Candelaria" ~ "La Candelaria",
                                         d$COMUNA=="Laureles Estadio" ~ "Laureles",
                                         d$COMUNA=="Manrique" ~ "Manrique",
                                         d$COMUNA=="Popular" ~ "Popular",
                                         d$COMUNA=="Robledo" ~ "Robledo",
                                         d$COMUNA=="San Javier" ~ "San Javier",
                                         d$COMUNA=="Santa Cruz" ~ "Santa Cruz",
                                         d$COMUNA=="Villa Hermosa" ~ "Villa Hermosa",
                                         d$COMUNA=="Corregimiento de San SebastiÃ¡n de Palmitas" ~ "palmitas",
                                         d$COMUNA=="Altavista" ~ "Altavista",
                                         d$COMUNA=="AndalucÃ???a" ~ "Andalucia",
                                         d$COMUNA=="Bolivariana" ~ "Bolivariana",
                                         d$COMUNA=="Boston" ~ "Boston",
                                         d$COMUNA=="Cabecera San Antonio de Prado" ~ "Corregimiento de San Antonio de Prado",
                                         d$COMUNA=="Calasanz" ~ "Calasanz",
                                         d$COMUNA=="Calle Nueva" ~ "Calle Nueva",
                                         d$COMUNA=="Campo Amor" ~ "Campo Amor",
                                         d$COMUNA=="Campo ValdÃ©s No. 1" ~ "Campo valdes",
                                         d$COMUNA=="Campo ValdÃ©s No. 2" ~ "Campo valdes",
                                         d$COMUNA=="Caribe" ~ "Caribe",
                                         d$COMUNA=="Cerro Nutibara" ~ "Cerro Nutibara",
                                         d$COMUNA=="CorazÃ³n de JesÃºs" ~ "Corazon de Jesus",
                                         d$COMUNA=="Cristo Rey" ~ "Cristo Rey",
                                         d$COMUNA=="El Chagualo" ~ "El Chagualo",
                                         d$COMUNA=="El Raizal" ~ "El Raizal",
                                         d$COMUNA=="FÃ¡tima" ~ "fatima",
                                         d$COMUNA=="Girardot" ~ "Girardot",
                                         d$COMUNA=="Guayaquil" ~ "Guayaquil",
                                         d$COMUNA=="HÃ©ctor Abad GÃ³mez" ~ "Hecthor Abad Gomez",
                                         d$COMUNA=="JesÃºs Nazareno" ~ "Jesus Nazareno",
                                         d$COMUNA=="La Alpujarra" ~ "La Alpujarra",
                                         d$COMUNA=="La Floresta" ~ "La Floresta",
                                         d$COMUNA=="La Rosa" ~ "La Rosa",
                                         d$COMUNA=="Las Esmeraldas" ~ "Las Esmeraldas",
                                         d$COMUNA=="Las Palmas" ~ "Las Palmas",
                                         d$COMUNA=="Laureles" ~ "Laureles",
                                         d$COMUNA=="Los Conquistadores" ~ "Los Conquistadores",
                                         d$COMUNA=="Los Mangos" ~ "Los Mangos",
                                         d$COMUNA=="Manila" ~ "Manila",
                                         d$COMUNA=="Moravia" ~ "Moravia",
                                         d$COMUNA=="Naranjal" ~ "Naranjal",
                                         d$COMUNA=="Patio Bonito" ~ "Patio Bonito",
                                         d$COMUNA=="Perpetuo Socorro" ~ "Perpetuo Socorro",
                                         d$COMUNA=="Rosales" ~ "Rosales",
                                         d$COMUNA=="Suramericana" ~ "Suramericana",
                                         d$COMUNA=="Toscana" ~ "Toscana",
                                         d$COMUNA=="Veinte de Julio" ~ "Veinte de Julio",
                                         TRUE ~ "Otros")
    d$COMUNA<-as.factor(d$COMUNA)
    d$DISENO <-case_when( d$DISENO=="Ciclo Ruta" ~ "Ciclo Ruta",
                          d$DISENO=="Glorieta" ~ "Glorieta",
                          d$DISENO=="Choque" ~ "Choque",
                          d$DISENO=="Interseccion" ~ "Interseccion",
                          d$DISENO=="Lote o Predio" ~ "Lote o Predio",
                          d$DISENO=="Paso a Nivel" ~ "Paso a Nivel",
                          d$DISENO=="Paso Elevado" ~ "Paso Elevado",
                          d$DISENO=="Paso Inferior" ~ "Paso Inferior",
                          d$DISENO=="Puente" ~ "Puente",
                          d$DISENO=="Tramo de via" ~ "Tramo de via",
                          d$DISENO=="Tunel" ~ "Tunel",
                          d$DISENO=="Via peatonal" ~ "Via peatonal",
                          TRUE ~ "Otros")
    d$DISENO<-as.factor(d$DISENO)
    d$MES<-as.factor(d$MES)
    d$DIA<-as.factor(d$DIA)
    d$PERIODO<-as.factor(d$PERIODO)
    d$CANTIDAD<-(c(1))
    d<-d[,c(7,8,9,13,14,15,16,17,18,19,25)]
    d$DIA_NOMBRE <-case_when( d$DIA_NOMBRE=="LUNES    " ~ "LUNES",
                              d$DIA_NOMBRE=="MARTES   " ~ "MARTES",
                              d$DIA_NOMBRE=="MIÃ‰RCOLES" ~ "MIERCOLES",
                              d$DIA_NOMBRE=="JUEVES   " ~ "JUEVES",
                              d$DIA_NOMBRE=="VIERNES  " ~ "VIERNES",
                              d$DIA_NOMBRE=="SÃ\u0081BADO   " ~ "SABADO",
                              d$DIA_NOMBRE=="DOMINGO  " ~ "DOMINGO",
                              TRUE ~ "raro")
    d$DIA_NOMBRE<-as.factor(d$DIA_NOMBRE)
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
  
######## DEFINIMOS EL SEGUNDO FILTRO DE PERIODO
  output$periodo <- renderUI({
    selectInput ("Anio", 
                 "AÑO:", 
                 sort(unique(totaldata()[,2]), decreasing = FALSE),
                 selected="2014",
                 multiple =FALSE)
  })
  

 ########## SACAMOS UN SUMMARY INICIAL 
  output$summary <- renderPrint({
    dataset <- totaldata()
    summary(dataset)
  })
  
  
######## DEFINIMOS LA TABLA HISTORICA DE INFORMACIN
  
  tabla<-reactive({
 subset(totaldata(), PERIODO == input$Anio)
 
  })
  
 ###### IMPRIMIR TABLA FINAL 
  
  output$view <- renderTable({
    dataset <- tabla()
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
           tabPanel(title = "VISUALIZACIÓN",
                    tags$div(class="landing-wrapper",
                             
                             # child element 1: images
                             
                             tags$div(class="landing-block background-content",
                                      
                                      img(src=imagen2, width = "200", height = "100"),
                                      img(src=imagen, width = "200", height = "100", align = "right")
                             )),
                    a(h1(style = "color:black; font-family:'fantasy'","INFORMACIÓN HISTORICA ACCIDENTES"), align = "center"),
  
  
  sidebarLayout(
    sidebarPanel(uiOutput("CLASE"),
                 uiOutput("periodo")
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
  tabPanel(title = "VIDEO PROMOCIONAL",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","LINK VIDEO YOUTUBE"), align = "center"),
           fluidRow(align='center',h3("Video App Accidentalidad Medellin"),br(),
                    box(align='center', HTML(youtube_video),width=700))
           ),
  
  
 
  tabPanel(title = "PREDICCIÓN",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","PREDICCIÓN ACCIDENTABILIDAD DÍA, SEMANA Y MESES"), align = "center")),
  
  
  
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
           ),
  
  
  
  
  
  tabPanel(title = "REPORTE TÉCNICO",
           tags$div(class="landing-wrapper",
                    
                    # child element 1: images
                    tags$div(class="landing-block background-content",
                             
                             img(src=imagen2, width = "200", height = "100"),
                             img(src=imagen, width = "200", height = "100", align = "right")
                    )),
           a(h1(style = "color:black; font-family:'fantasy'","INFORMACIÓN TECNICA DESARROLLADA"), align = "center"))
  
  
)
)


shinyApp(ui = ui, server = server)