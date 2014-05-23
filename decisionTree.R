

archivo<-"20140318211813-497933c89e49ff1b_Clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")

plot(as.party(modelo), type="simple")
countTrain<-floor(nrow(datosCel)*0.7)
celularTrain<-datosCel[1:countTrain,]
celularTest<-datosCel[(countTrain+1):nrow(datosCel),]
modelo <- rpart(WIFI.STATE ~ .,data=celularTrain, parms=list(split='gini'))
pred<-predict(modelo, type="class", newdata=celularTest)
prob<-predict(modelo, type="prob", newdata=celularTest)
perf <- performance(pred)
class(pred)
t<-table(pred,celularTest$WIFI.STATE)
table(celularTest$WIFI.STATE)
sum(diag(t))/sum(t)
    
prediction(pred, celularTest$WIFI.STATE, label.ordering = NULL)
data(ROCR.simple)
ROCR.simple$predictions
