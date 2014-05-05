setwd("C:/Users/Usuarioç/Desktop/carlos/datos-paralela-distribuida/CSVs/")
getwd()
library(Hmisc)
library(arules)
source("funciones.R")

soporte<-0.3
confianza<-0.8
archivo<-"20140318211813-497933c89e49ff1b"
#archivo<-"20140318211729-9e4f9ff1072ab8d6"
#archivo<-"20140318211740-48febd56a2d553fe"
#archivo<-"20140318211758-666d4bbefa5393d5"
#archivo<-"20140318211813-497933c89e49ff1b"
clases=c("numeric","numeric","numeric","numeric","numeric","numeric","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character", "character","character","character")

datosCel=read.csv(paste(archivo,".csv",sep=""), header = TRUE, sep = ";",quote = "\"",dec = ".", fill = TRUE,comment.char = "",colClasses =clases)
#Realizo un profiling del archivo para ver la calidad de los datos.
profilign<-describe(datosCel[1])
names(profilign)
porcentajes<-datosCel$BATTERY.PCT
porcentajes<-as.numeric(gsub("%", "", porcentajes))/100
summary(porcentajes)
hist(porcentajes)

#creo una variable con los dias de la semana
dias<-extraerDiasDelaSemana(datosCel)
datosCel$DAY_WEEK<-as.factor(dias)
#creo la variale HOUR_DISC
horasDisc<-discretizarHora(datosCel)
datosCel$HOUR_DISC<-as.factor(horasDisc)
#creo la variable 
bbPower2<-crearBatteryPower2(datosCel)
datosCel$BATTERY.POWER2<-as.factor(bbPower2)

#Los valores perdidos lo reemplazo por la constante NA
datosCel[is.na(datosCel)]<-NA #NA_character_
datosCel[datosCel==""]<-NA #NA_character_

#preparo el dataset para aplicar reglas de asociacion
#elimino los atributos que no aportan informacion
datosCel[["YEAR"]] <- NULL
datosCel[["MONTH"]] <- NULL
datosCel[["DAY"]] <- NULL
datosCel[["HOUR"]] <- NULL
datosCel[["MINUTE"]] <- NULL
datosCel[["SECOND"]] <- NULL
datosCel[["WIFI.Quitapenas"]] <- NULL
datosCel[["BATTERY.PCT"]] <- NULL
datosCel[["LOCATION.LONG"]] <- NULL
datosCel[["LOCATION.LAT"]] <- NULL
datosCel[["LOCATION.ALT"]] <- NULL
datosCel[["WIFI.IP"]] <- NULL
datosCel[["WIFI.MAC"]] <- NULL
datosCel[["WIFI.NetID"]] <- NULL
datosCel[["WIFI.BSSID"]] <- NULL
datosCel[["BATTERY.POWER"]] <- NULL
datosCel[["BLUETOOTH.NAME"]] <- NULL
datosCel[["BLUETOOTH.ADDRESS"]] <- NULL
#El paquete arules necesita que los datos sean factor(categorias)
#transformamos al data set como transacciones
aframe2 <- as.data.frame(lapply(datosCel, factor)) 
celular<- as(aframe2, "transactions")
summary(celular)
countTrain<-floor(length(celular)*0.7)
countTest<-length(celular)-countTrain
c<-celular[1:countTrain]
celularTest<-celular[8669:length(celular)]
celular<-c
#Para ver los items mas importantes, muestro aquellos que estan en almenos el 30% de las transacciones
itemFrequencyPlot(celular, support = soporte, cex.names=0.7,horiz =FALSE)

#genero reglas
reglas <-apriori(celular,parameter=list(support=soporte, confidence=confianza,minlen=2))
#agrego un columna para saber si el itemset es cerrado, sirve para eliminar reglas redundantes
quality(reglas) <- cbind(quality(reglas),isClosed = is.closed(generatingItemsets(reglas)))
quality(reglas) <- cbind(quality(reglas),phi = interestMeasure(reglas, method = "phi", 
                                                          transactions = celular))

subreglasClosed<-subset(reglas,isClosed==TRUE & lift>1.5)
summary(subreglasClosed)

subreglasBatteryPower<-subset(subreglasClosed, subset =rhs %pin% "WIFI.IACCESS=0")
plot(subreglasBatteryPower, method="grouped", control=list(k=50))
#Criterio 1: reglas teniendo en cuenta el lift.
inspect(head(sort(subreglasClosed,by="lift",decreasing=TRUE),n=100))
#Criterio 2: reglas teniendo en cuenta la confianza.
inspect(head(sort(subreglasClosed,by="confidence",decreasing=TRUE),n=100))
#Criterio 3: reglas teniendo en cuenta el soporte.
inspect(head(sort(subreglasClosed,by="support",decreasing=TRUE),n=100))
#Criterio 4: reglas que involucren capacidad de la bateria.
subreglasBatteryPower<-subset(subreglasClosed, subset = rhs %pin% "BATTERY.POWER2=medio")
inspect(head(sort(subreglasBatteryPower,by="confidence",decreasing=TRUE),n=50))

#guardo las reglas en formato csv.
nombreArchivo<-paste("reglas_",archivo,"_S_",soporte,"C_",confianza,".csv",sep="")
quality(subreglasClosed)$lift<-round(quality(subreglasClosed)$lift, digits = 3)
quality(subreglasClosed)$support<-round(quality(subreglasClosed)$support, digits = 3)
quality(subreglasClosed)$confidence<-round(quality(subreglasClosed)$confidence, digits = 3)
write(subreglasClosed, file=nombreArchivo,sep=";",col.names=NA)

###Aplicacion de analisis de correspondencia#######
library(ca)
table(datosCel[,c("BATTERY.POWER2","DAY_WEEK")])
chisq.test(table(datosCel[,c("BATTERY.POWER2","DAY_WEEK")]))
c<-ca(table(datosCel[,c("BATTERY.POWER2","DAY_WEEK")]))
summary(c)
plot(c,arrows=c(T,F))

2.2e-16<0.05
################# testing de reglas ################
vec<-vector(length=530)
for(i in 1:length(subreglasClosed)){
    vec[i]<-calcularPresicionRegla(regla=reglas[i],transacciones=celularTest)    
}
hist(vec)
quality(subreglasClosed) <- cbind(quality(subreglasClosed),precision = vec)
inspect(head(sort(subreglasClosed,by="lift",decreasing=TRUE),n=100))




transaccionesLhs<-subset(celularTest, subset =items %ain% c("BLUETOOTH.TSTATE=0","RINGER.STATE=1","WIFI.SSID=-"))
transaccionesLhsRhs<-subset(transaccionesLhs, subset =items %ain% c("BATTERY.PLGUSB=0"))
length(transaccionesLhsRhs)/length(transaccionesLhs)
inspect(transaccionesLhs[1])
