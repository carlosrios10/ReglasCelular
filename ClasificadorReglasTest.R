setwd("C:/Users/Usuarioç/Desktop/carlos/datos-paralela-distribuida/proyecto-reglas")
#Limpio memoria
rm(list = ls())
library(arules)
source("AllClasses.R")
source("ClasificadorReglas.R")

#Cargo el conjunto de datos limpio (generado en la etapa de procesamiento)
archivo<-"datos2/_20140318211813-497933c89e49ff1b_clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")

#Convierto los datos en transacciones y obtengo el train and test
celularTransaction<- as(datosCel, "transactions")
countTrain<-floor(length(celularTransaction)*0.7)
celularTrain<-celularTransaction[1:countTrain,]
celularTest<-celularTransaction[(countTrain+1):length(celularTransaction),]

classLabels <- itemLabels(celularTransaction)[itemInfo(celularTransaction)$variables=="WIFI.IACCESS"]
modelo<-ClasificadorReglas(classLabels,celularTrain,parameter=list(supp=0.1, conf=0.9, minlen=2))

length(modelo@rules)
pred <- predict(modelo, celularTest)
acc <- accuracy(pred$res, celularTest, modelo)
coverage(celularTest,pred$res)
confusion(pred$res, celularTest, modelo)

### Genero varias modelos con distintas confianza ###
confidence<-c(0.7,0.75,0.8,0.85,0.9)
resultado<-data.frame(confidence,
                      totalRules=vector(mode="integer",length=length(confidence)),
                      PruningTotalRules=vector(mode="integer",length=length(confidence)),
                      accuracy=vector(mode="double",length=length(confidence)),
                      coverage=vector(mode="double",length=length(confidence)))
for(i in 1:length(confidence)) {
    modelo<-ClasificadorReglas(classLabels,celularTrain,parameter=list(supp=0.1, conf=confidence[i], minlen=2))
    pred <- predict(modelo, celularTest)
    acc<- accuracy(pred$res, celularTest, modelo)
    cov<-coverage(celularTest,pred$res)
    resultado$accuracy[i]<-acc
    resultado$coverage[i]<-cov
    resultado$totalRules[i]<-modelo@totalRules
    resultado$PruningTotalRules[i]<-length(modelo@rules)
}
resultado<-round(resultado,digits=3)

#####Graficos de los resultados######
plot(resultado$confidence,resultado$accuracy, type="o",col="blue", 
     xlim=c(min(resultado$confidence),1),ylim=c(0,1),axes=FALSE, ann=FALSE)
axis(2,las=1, at=seq(0,by=0.5),cex=0.8)
axis(1, at=resultado$confidence)
box()
lines(resultado$confidence,resultado$coverage, type="o",pch=22, lty=2,  col="red")
title(main="Precison y cobertura del modelo", font.main=4)
title(xlab="Confianza")
title(ylab="Valores")
legend("topright", pch=21:22, lty=1:2,cex=0.8, col = c("blue", "red"), legend = c("Precision", "Cobertura"))

