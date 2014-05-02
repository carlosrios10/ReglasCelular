discretizarHora<-function(datosCel)
{
    hourDisc<- array(0,dim=c(length(datosCel$HOUR),0))
    diaActual=datosCel$DAY[1]
    cambioDia=FALSE
    horaActual=datosCel$HOUR[1]
    cambiaHora=FALSE
    esAM=TRUE
    for(i in 1:length(datosCel$HOUR)){
        dia=datosCel$DAY[i]
        if(dia!=diaActual){
            diaActual=dia
            esAM=TRUE
            cambiaHora=FALSE
            horaActual=datosCel$HOUR[i]
        }
        hora=datosCel$HOUR[i]
        if(hora!=horaActual){
            horaActual=hora
            cambiaHora=TRUE
        }
        
        if(hora==12&&cambiaHora){
            esAM=FALSE
        }
        
        if(hora==12&&esAM)
            hourDisc[i]="noche"
        
        if(hora>=1&&hora<=5&&esAM)
            hourDisc[i]="noche"
        
        if(hora>=6&&hora<12&&esAM)
            hourDisc[i]="maniana"
        
        if(hora==12&&!esAM)
            hourDisc[i]="maniana"
        
        if(hora>=1&&hora<=7&&!esAM)
            hourDisc[i]="tarde"
        
        if(hora>=8&&hora<=11&&!esAM)
            hourDisc[i]="noche"
        
    }
    return(hourDisc)
}

extraerDiasDelaSemana<-function(datosCel)
{
    diasDelaSemana<- array(0,dim=c(length(datosCel$DAY),0))
    for(i in 1:length(datosCel$DAY)){
        fechaVector<-as.character(c(datosCel$YEAR[i],datosCel$MONTH[i],datosCel$DAY[i]))
        fechaStr<- as.Date(paste(fechaVector,collapse="-"), "%Y-%m-%d");
        diasDelaSemana[i]=weekdays(fechaStr)
        
    }
    return(diasDelaSemana)
    
}
crearBatteryPower2<-function(datosCel){
    porcentajes<-datosCel$BATTERY.PCT
    porcentajes<-as.numeric(gsub("%", "", porcentajes))
    vec<-vector(length=length(porcentajes))
    vec[porcentajes>=0&porcentajes<=20]<-"bajo"
    vec[porcentajes>=21&porcentajes<=80]<-"medio"
    vec[porcentajes>=81&porcentajes<=100]<-"alto"
    return (vec)
}
calcularPresicionRegla<-function(regla,transacciones){
    antecedente<-lhs(regla)
    antecedente<-as(antecedente,"list")
    consecuente<-rhs(regla)
    consecuente<-as(consecuente,"list")
    transaccionesLhs<-subset(transacciones, subset =items %ain% antecedente[[1]])
    transaccionesLhsRhs<-subset(transaccionesLhs, subset =items %ain% consecuente[[1]])
    length(transaccionesLhsRhs)/length(transaccionesLhs)
    
}