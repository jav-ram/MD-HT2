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

for (i in 2:10) 
  wss[i] <- sum(kmeans(movies[,1:4], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# grupos
group <- 2

# K-Means
datos<-movies
moviesCompleto<-movies[complete.cases(movies),]
km<-kmeans(movies, group)
datos$grupo<-km$cluster

km

datos$grupo


plotcluster(movies,km$cluster) #grafica la ubicación de los clusters

