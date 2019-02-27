setwd("~/Documents/uvg/mineria/hoja2")
javier<-read.csv("tmdb-movies-1.csv")
head(javier)
#to install packages
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
#to load packages
library(factoextra)
library(NbClust)
#get elbow graph
fviz_nbclust(javier, kmeans, method = "wss") +geom_vline(xintercept = 4, linetype = 2)+labs(subtitle = "Elbow method")