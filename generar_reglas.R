#En esta seccion se generan las reglas de asociacion utilizando el algoritmo apriori.
#Limpio memoria
rm(list = ls())
#Cargo librerias(si es la primera vez se debe instalar el paquete-ventana "Packages")
library(arules)
source("funciones.R")

#Cargo el conjunto de datos limpio (generado en la etapa de procesamiento)
archivo<-"20140318211813-497933c89e49ff1b_Clean.csv"
#Cargo el archivo en memoria (colClasses=factor significa que las columnas se convierten en categorias)
datosCel=read.csv(archivo, header = TRUE, sep = ",",colClasses="factor")
#Convierto los datos en transacciones
celularTransaction<- as(datosCel, "transactions")
#Defino soporte y confianza
soporte<-0.3
confianza<-0.8
#Para ver los items mas importantes, muestro aquellos que superan el minsup
itemFrequencyPlot(celularTransaction, support = soporte, cex.names=0.7,horiz =FALSE)

#genero reglas
reglas <-apriori(celularTransaction,parameter=list(support=soporte, confidence=confianza,minlen=2))
#agrego un columna para saber si el itemset es cerrado, sirve para eliminar reglas redundantes
quality(reglas) <- cbind(quality(reglas),isClosed = is.closed(generatingItemsets(reglas)))
#Agrego la medida phi (se pueden agregar todas las que se deseen - ver ayuda)
quality(reglas) <- cbind(quality(reglas),phi = interestMeasure(reglas, method = "phi",transactions = celularTransaction))

#Obtengo aquellas reglas que provienen de un itemset cerrado y aquellas que tienen un lift>1.5
subreglasClosed<-subset(reglas,isClosed==TRUE & lift>1.5)

#Busqueda de reglas
#si quiero ver las 10 primeras reglas ordenadas segun el lift
inspect(head(sort(subreglasClosed,by="lift",decreasing=TRUE),n=10))
#Si quiero buscar reglas segun algun criterio, por ejemplo reglas que tengan en el consecuente BATTERY.PLGUSB=1 
subreglas<-subset(subreglasClosed, subset =rhs %in% "BATTERY.PLGUSB=1")
inspect(subreglas)
#si quier ver los distintos consecuentes de las reglas
frecuenciaDelConsecuente(subreglasClosed)

#guardo las reglas en formato csv.
nombreArchivo<-paste("reglas_20140318211813-497933c89e49ff1b_S_",soporte,"C_",confianza,".csv",sep="")
# redondeo a tres decimales 
quality(subreglasClosed)$lift<-round(quality(subreglasClosed)$lift, digits = 3)
quality(subreglasClosed)$support<-round(quality(subreglasClosed)$support, digits = 3)
quality(subreglasClosed)$confidence<-round(quality(subreglasClosed)$confidence, digits = 3)
quality(subreglasClosed)$phi<-round(quality(subreglasClosed)$phi, digits = 3)
write(subreglasClosed, file=nombreArchivo,sep=";",col.names=NA)
