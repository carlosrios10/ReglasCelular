setwd("C:/Users/Usuario�/Desktop/carlos/datos-paralela-distribuida/proyecto-reglas")
library("cluster")
operaciones<-read.csv("operaciones.csv")
Train_data<- operaciones[,2:617]
d<-daisy(Train_data)
v<-vector(mode="integer",length=14)
for(num_of_cluster in 1:length(v)) {
centroid <- pam(d,(num_of_cluster+1),diss=T)
s <- silhouette(centroid)
mean<-mean(s[,"sil_width"])
v[num_of_cluster]<-mean
}
plot(c(2:15),v);
plot(silhouette(pam(d,2,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,3,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,4,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,5,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,6,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,7,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,8,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,9,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,10,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,11,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,12,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,13,diss=T)),main="Siluetas - PAM")
plot(silhouette(pam(d,14,diss=T)),main="Siluetas - PAM")

plot(silhouette(pam_3, d),main="Siluetas - PAM")


cc<-pam(d,7,diss=T)
final<-data.frame(operaciones$AltaRelaciones, cc$clustering)
split(final$operaciones.AltaRelaciones,f=final$cc.clustering)
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:5) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


d<-adist(operaciones$AltaRelaciones)
#------------------------Cluster-------------------------------------

clusdata<-operaciones[,2:617]
clusdata <- na.omit(clusdata) # listwise deletion of missing
d<-daisy(clusdata, metric = c("euclidean"),stand=TRUE)
d<-dist(clusdata, method="binary")

hc_complete <- hclust(d, method="complete")
hc_ward <- hclust(d, method="ward")
hc_single <- hclust(d, method="single")
hc_average <- hclust(d, method="average")

pam_2 <- pam(d, 2)
pam_3 <- pam(d, 3)

plot(hc_complete,hang=-1,labels=FALSE,main="Dendrograma - Método Encadenamiento Completo")
plot(hc_ward,hang=-1,labels=FALSE,main="Dendrograma - Método Ward")
plot(hc_single,hang=-1,labels=FALSE,main="Dendrograma - Método Encadenamiento Simple")
plot(hc_average,hang=-1,labels=FALSE,main="Dendrograma - Método Encadenamiento Promedio")


plot(silhouette(cutree(hc_complete,2), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")
plot(silhouette(cutree(hc_complete,3), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")
plot(silhouette(cutree(hc_complete,4), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")
plot(silhouette(cutree(hc_complete,5), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")
plot(silhouette(cutree(hc_complete,6), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")
plot(silhouette(cutree(hc_complete,7), d),main="Siluetas - Agrupamiento jerarquico - Encadenemiento completo")

plot(silhouette(cutree(hc_ward,2), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,3), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,4), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,5), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,6), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,7), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,8), d),main="Siluetas - Agrupamiento jerarquico - Ward")
plot(silhouette(cutree(hc_ward,9), d),main="Siluetas - Agrupamiento jerarquico - Ward")


plot(silhouette(pam_2, d),main="Siluetas - PAM")
plot(silhouette(pam_3, d),main="Siluetas - PAM")


#se elije  metodo ward 5 clusters 
sil=silhouette(cutree(hc_ward,5),d)
#selecciona a todos los casos mal agrupados y reasigna
sil[sil[,3]<0,]
sil[sil[,3]<0,1]=sil[sil[,3]<0,2]
clus=sil[,1]
plot(silhouette(clus, d),main="Siluetas - Reasignación")

clusdata$cluster=clus

dataset$cluster=rep(NA,1000)

dataset[which(dataset$song_id %in% row.names(clusdata)),c("cluster")]=clusdata$cluster

clus1=clusdata[clusdata$cluster==1,]
clus2=clusdata[clusdata$cluster==2,]
clus3=clusdata[clusdata$cluster==3,]
clus4=clusdata[clusdata$cluster==4,]
clus5=clusdata[clusdata$cluster==5,]