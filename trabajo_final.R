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
#creo la variable 
# fecha<-agregarFecha(datosCel)
# datosCel$FECHA<-fecha
# hist(fecha,breaks="years")
# table(fecha)
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
####
#El paquete arules necesita que los datos sean factor(categorias)
#transformamos al data set como transacciones
datosCel <- as.data.frame(lapply(datosCel, factor)) 
countTrain<-floor(nrow(datosCel)*0.7)
countTest<-nrow(datosCel)-countTrain
celularTrain<-datosCel[1:countTrain,]
celularTest<-datosCel[(countTrain+1):nrow(datosCel),]
celularTransaction<- as(celularTrain, "transactions")
#Para ver los items mas importantes, muestro aquellos que estan en almenos el 30% de las transacciones
itemFrequencyPlot(celularTransaction, support = soporte, cex.names=0.7,horiz =FALSE)

#genero reglas
reglas <-apriori(celularTransaction,parameter=list(support=soporte, confidence=confianza,minlen=2))
#agrego un columna para saber si el itemset es cerrado, sirve para eliminar reglas redundantes
quality(reglas) <- cbind(quality(reglas),isClosed = is.closed(generatingItemsets(reglas)))
quality(reglas) <- cbind(quality(reglas),phi = interestMeasure(reglas, method = "phi",transactions = celularTransaction))

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
celularTestTransaction<- as(celularTest, "transactions")
quality(subreglasClosed) <- cbind(quality(subreglasClosed),confTest = interestMeasure(subreglasClosed, method = "confidence",transactions = celularTestTransaction,reuse=FALSE))
quality(subreglasClosed) <- cbind(quality(subreglasClosed),sopLhsTest = interestMeasure(subreglasClosed, method = "coverage",transactions = celularTestTransaction,reuse=FALSE))
hist(quality(subreglasClosed)$confTest)
frecuenciaDelConsecuente(subreglasClosed)
############## modelo para predecir ###############
subreglasModelo<-subset(subreglasClosed, subset =rhs %in% "RINGER.STATE=1")
inspect(subreglasModelo)
reglaModelo<-subreglasModelo[82]
claseOriginal<-celularTest["RINGER.STATE"]
for(i in 1:length(subreglasModelo) ){
    claseOriginal <- cbind(claseOriginal,clase_regla=calcularPresicionModeloRINGERSTATE1(subreglasModelo[i],celularTestTransaction))    
}

precisiones<-vector(length=length(subreglasModelo))
for(i in 1:ncol(claseOriginal) ){
    col<-i+1
    precisiones[i]<-sum(claseOriginal[1]==claseOriginal[i],na.rm=TRUE)/nrow(claseOriginal)
    
}

sum(claseOriginal[1]==claseOriginal[2],na.rm=TRUE)/nrow(claseOriginal)
quality(subreglasModelo) <- cbind(quality(subreglasModelo),precision=precisiones[2:84])

plot(size(subreglasModelo),quality(subreglasModelo)$precision)
DT = data.table(x=c("b","b","b","a","a"),v=rnorm(5))
tables()
setkey(DT,x)
DT["b",]
grpsize = ceiling(1e7/26^2)

tt=system.time( DF <- data.frame(
     x=rep(LETTERS,each=26*grpsize),
     y=rep(letters,each=grpsize),
    v=runif(grpsize*26^2),
     stringsAsFactors=FALSE)
     )
dim(DF)
tt=system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])
DT = as.data.table(DF)
system.time(setkey(DT,x,y))
ss=system.time(ans2 <- DT[J("R","h")]) 
system.time(ans1 <- DT[x=="R" & y=="h",])
system.time(ans2 <- DF[DF$x=="R" & DF$y=="h",])
