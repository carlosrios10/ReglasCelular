getwd()
#Limpio memoria
rm(list = ls())
library(arules)
source("AllClasses.R")
source("ClasificadorReglas.R")

#Cargo el conjunto de datos limpio (generado en la etapa de procesamiento)
archivo<-"20140318211813-497933c89e49ff1b_Clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")
#Convierto los datos en transacciones y obtengo el train and test
celularTransaction<- as(datosCel, "transactions")
countTrain<-floor(length(celularTransaction)*0.7)
celularTrain<-celularTransaction[1:countTrain,]
celularTest<-celularTransaction[(countTrain+1):length(celularTransaction),]

classLabels <- itemLabels(celularTransaction)[itemInfo(celularTransaction)$variables=="WIFI.STATE"]

modelo<-ClasificadorReglas(classLabels,celularTrain,parameter=list(supp=0.03, conf=0.8, minlen=2))
pred <- predict(modelo, celularTest)
acc <- accuracy(pred, celularTest, modelo)
confusion(pred2, celularTest, modelo)

