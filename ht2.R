install.packages("cluster")
install.packages("e1071")
install.packages("mclust")
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")


library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering


setwd("~/U/2019/1er/MineriaDeDatos/MD-HT2")
movies <- read.csv("tmdb-movies.csv")

movies$id <- NULL
movies$imdb_id <- NULL
movies$original_title <- NULL
movies$cast <- NULL
movies$homepage <- NULL
movies$tagline <- NULL
movies$keywords <- NULL
movies$overview <- NULL
movies$release_date <- NULL
movies$director <- NULL
movies$genres <- NULL
movies$production_companies <- NULL

head(movies)

wss <- (nrow(movies)-1)*sum(apply(movies,2,var))

for (i in 2:10) wss[i] <- sum(kmeans(movies[,1:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# grupos
group <- 2

# K-Means
datos<-movies
moviesCompleto<-movies[complete.cases(movies),]
km<-kmeans(movies, group)
datos$grupo<-km$cluster
plotcluster(movies,km$cluster) #grafica la ubicación de los clusters

<<<<<<< HEAD
km

datos$grupo

=======
#Clustering jerárquico
hc<-hclust(dist(movies)) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=group) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=group) #corta el dendograma, determinando el grupo de cada fila
datos$gruposHC<-groups
g1HC<-datos[datos$gruposHC==1,]
g2HC<-datos[datos$gruposHC==2,]
>>>>>>> 3040e2cca113c552fde0b6026d86d4c17319b1c0

#Fuzzy C-Means
fcm<-cmeans(movies,2)
datos$FCGrupos<-fcm$cluster
datos<-cbind(datos,fcm$membership)

#Mixture of gaussians
mc<-Mclust(movies,2)
plot(mc, what = "classification", main="MClust Classification")
datos$mxGau<-mc$classification
g1MC<-datos[datos$mxGau==1,]
g2MC<-datos[datos$mxGau==2,]

#Método de la silueta para las k-medias
silkm<-silhouette(km$cluster,dist(movies))
mean(silkm[,2]) #0.55, no es la mejor partición pero no está mal

#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(movies))
mean(silch[,2]) #0.51, no es la mejor partición pero no está mal

#Método de la silueta para fuzzy cmeans
silfcm<-silhouette(fcm$cluster,dist(movies))
mean(silfcm[,2]) #0.54, no es la mejor partición pero no está mal

#Método de la silueta para mixture of gaussians
silmg<-silhouette(mc$classification,dist(movies))
mean(silmg[,2]) #0.50, no es la mejor partición pero no está mal
