library(h2o)
########## modelo predictivo tipo de accidente
######datosentrenamiento
D2014<-read.csv(file.choose(""),sep=",",header = TRUE)
D2015<-read.csv(file.choose(""),sep=",",header = TRUE)
D2016<-read.csv(file.choose(""),sep=",",header = TRUE)
D2017<-read.csv(file.choose(""),sep=",",header = TRUE)
###### concatenación de los datos
entrenamiento<-rbind(D2014,D2015,D2016,D2017)
######datos validación
validación<-read.csv(file.choose(""),sep=",",header = TRUE)
validacion<-as.data.frame(validación)
validacion$MES<-as.factor(validacion$MES)
###### modelo
######selecciono variables
entrenamientomod<-entrenamiento[,-c(1,2,3,4,5,7,8,10,11,12,13,20,21,22,23,24)]
##### defino al variable mes como factor debido a que originalmente se entiende como numerica
entrenamientomod$MES<-as.factor(entrenamientomod$MES)
#### metodologia
h2o.init(nthreads = -1)
mod<-h2o.deeplearning(y = 'GRAVEDAD',
                      training_frame = as.h2o(entrenamientomod),
                      activation = 'Rectifier',
                      hidden = c(5, 5),
                      epochs = 100,
                      train_samples_per_iteration = -2)
####predicción
prob_pred <- h2o.predict(mod, newdata = as.h2o(validacion))
y_pred <- as.vector(prob_pred$predict)
