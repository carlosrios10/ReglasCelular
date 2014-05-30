setwd("C:/Users/Usuarioç/Desktop/carlos/datos-paralela-distribuida/proyecto-reglas")
#Limpio memoria
rm(list = ls())
library(arules)
source("AllClasses.R")
source("ClasificadorReglas.R")

#Cargo el conjunto de datos limpio (generado en la etapa de procesamiento)
archivo<-"20140318211813-497933c89e49ff1b_Clean2.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")
sum(datosCel=="-",na.rm=T)

#Convierto los datos en transacciones y obtengo el train and test
celularTransaction<- as(datosCel, "transactions")
countTrain<-floor(length(celularTransaction)*0.7)
celularTrain<-celularTransaction[1:countTrain,]
celularTest<-celularTransaction[(countTrain+1):length(celularTransaction),]

classLabels <- itemLabels(celularTransaction)[itemInfo(celularTransaction)$variables=="WIFI.IACCESS"]



modelo<-ClasificadorReglas(classLabels,celularTrain,parameter=list(supp=0.1, conf=0.8, minlen=2))
#write(modelo@rules, file="modelo-reglas.csv",sep=";",col.names=NA)
inspect(head(modelo@rules[1]))
allRules<-modelo@rules
modelo@rules<-rulesPruebas
pred <- predict(modelo, celularTest)
acc <- accuracy(pred$res, celularTest, modelo)
coverage(celularTest,pred$res)
confusion(pred$res, celularTest, modelo)
pred$match<-pred$res==unlist(as(celularTest[,classLabels], "list"))

erroPred<-pred[!pred$match,]
erroPred<-na.omit(erroPred)
errorRules<-modelo@rules[which(is.element(labels(modelo@rules),erroPred$reglasElegidas) )]
inspect(errorRules)

goodPred<-pred[pred$match,]
goodPred<-na.omit(goodPred)
goodRules<-modelo@rules[which(is.element(labels(modelo@rules),goodPred$reglasElegidas) )]
inspect(goodRules)
1/7
noMatachRules<- allRules[which(!is.element(allRules,goodRules))]

quality(allRules)<-cbind(quality(allRules),confTest = interestMeasure(x=allRules,method="confidence",transactions=celularTest,reuse=F))
quality(allRules)<-cbind(quality(allRules),coverageTest = interestMeasure(x=allRules,method="coverage",transactions=celularTest,reuse=F))

inspect(allRules)
rulesPruebas<-allRules[which(quality(allRules)$confTest>=0.85)]
rulesPruebas2<-allRules[which(quality(allRules)$confTest<0.85)]
inspect(rulesPruebas2)
allRules<-addInterestMeasure(allRules,data=celularTrain)
quality<-quality(allRules)
> acc
[1] 0.8547565
> coverage(celularTest,pred$res)
[1] 0.9504844

plot(quality$gini,quality$confTest)
table(quality$isClosed)
cor<-cor(x=quality)
plot(cor)
sacale<-scale(quality)
