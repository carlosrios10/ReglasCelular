getwd()
#Limpio memoria
rm(list = ls())
library("rpart")
archivo<-"20140318211813-497933c89e49ff1b_Clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")


countTrain<-floor(nrow(datosCel)*0.7)
celularTrain<-datosCel[1:countTrain,]
celularTest<-datosCel[(countTrain+1):nrow(datosCel),]
modelo <- rpart(WIFI.STATE ~ .,data=celularTrain, parms=list(split='gini'), control = mycontrol)
modelo$cptable

pred<-predict(modelo, type="class", newdata=celularTest)
prob<-predict(modelo, type="prob", newdata=celularTest)
perf <- performance(pred)
class(pred)
t<-table(pred,celularTest$WIFI.STATE)
table(celularTest$WIFI.STATE)
sum(diag(t))/sum(t)
plot(as.party(modelo), type="simple")    
prediction(pred, celularTest$WIFI.STATE, label.ordering = NULL)
data(ROCR.simple)
ROCR.simple$predictions

source("http://scg.sdsu.edu/wp-content/uploads/2013/09/dataprep.r")
library(ROCR)
mycontrol = rpart.control(cp = 0, xval = 10)
fittree = rpart(income~., method = "class",data = data$train, control = mycontrol)
fittree$cptable
which(min(fittree$cptable[,4])==fittree$cptable[,4])
cptarg = sqrt(fittree$cptable[7,1]*fittree$cptable[8,1])
prunedtree = prune(fittree,cp=cptarg)
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(prunedtree, uniform = T, compress = T, 
     margin = 0.1, branch = 0.3)
text(prunedtree, use.n = T, digits = 3, cex = 0.6)
fit.preds = predict(prunedtree,newdata=data$val,type="class")
fit.table = table(data$val$income,fit.preds)
fit.table
sum(diag(fit.table))/sum(fit.table)
fit.pr = predict(prunedtree,newdata=data$val,type="prob")[,2]
fit.pred = prediction(fit.pr,data$val$income)
fit.perf = performance(fit.pred,"tpr","fpr")
plot(fit.perf,lwd=2,col="blue",main="ROC:  Classification Trees on Adult Dataset")
abline(a=0,b=1)
