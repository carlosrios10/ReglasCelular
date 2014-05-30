getwd()
#Limpio memoria
rm(list = ls())
library("rpart")
source("funciones.R")
archivo<-"20140318211813-497933c89e49ff1b_Clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")


countTrain<-floor(nrow(datosCel)*0.7)
celularTrain<-datosCel[1:countTrain,]
celularTest<-datosCel[(countTrain+1):nrow(datosCel),]

mycontrol = rpart.control(cp = 0, xval = 10)
modelo <- rpart(WIFI.STATE ~ .,data=celularTrain, control = mycontrol,method="class")
which.min(modelo$cptable[,"xerror"])
print(modelo)

modelo <- tree(WIFI.STATE ~ .,data=celularTrain)
seq<-prune.tree(modelo)
plot(seq)
cv<-cv.tree(modelo)
plot(cv)
#Elegirmos el mejor cp y realizamos el pruning
# Regla 1-SD
cpM<-calcularMejorCp(modelo)
cpM[1]
prunedModel = prune(modelo,cp=cpM[1])

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(prunedModel, uniform = T, compress = T,margin = 0.1, branch = 0.3)
text(prunedModel, use.n = T, digits = 3, cex = 0.6)

#calculamos la precision
pred<-predict(prunedModel, type="class", newdata=celularTest)
model.table = table(celularTest$WIFI.STATE,pred)
model.table
sum(diag(model.table))/sum(model.table)

#hacemos la curva ROC
prob<-predict(prunedModel, type="prob", newdata=celularTest)[,2]
pred = prediction(prob,celularTest$WIFI.STATE)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="ROC:  Classification Trees on Adult Dataset")
abline(a=0,b=1)

perf = performance(pred,"auc")@y.values[[1]]




