#En esta seccion se realiza carga, limpieza y agregado de variables, con el objetivo de obtener un archivo
#limpio listo para aplicarle algun algoritmo.
#Limpio memoria
rm(list = ls())
#Cargo la funciones
source("funciones.R")

#Carga del archivo
archivo<-"20140318211813-497933c89e49ff1b.csv"
#Se definien los tipos de datos para las variables del conjunto de datos.
clases=c("numeric","numeric","numeric","numeric","numeric","numeric","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character","character","character","character",
         "character","character","character","character", "character","character","character")
#Cargo el archivo en memoria
datosCel=read.csv(archivo, header = TRUE, sep = ";",colClasses =clases)
#creo una variable con los dias de la semana
dias<-extraerDiasDelaSemana(datosCel)
datosCel$DAY_WEEK<-as.factor(dias)
#creo la variale HOUR_DISC
horasDisc<-discretizarHora(datosCel)
datosCel$HOUR_DISC<-as.factor(horasDisc)
#creo la variable bbPower2
bbPower2<-crearBatteryPower2(datosCel)
datosCel$BATTERY.POWER2<-as.factor(bbPower2)
#Los valores perdidos lo reemplazo por la constante NA
datosCel[is.na(datosCel)]<-NA 
datosCel[datosCel==""]<-NA 
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
#Guardo el archivo
write.csv(datosCel, file="20140318211813-497933c89e49ff1b_Clean.csv", row.names=FALSE)


