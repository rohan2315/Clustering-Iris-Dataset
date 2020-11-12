install.packages("fpc")
install.packages("corrplot")

library(ggplot2)
library(corrplot)
library(tidymodels)
library(tidyverse)
library(cluster) 
library(fpc)

## Iris dataset and its characteristics

irisdata <- read_csv("Rohan/The_Spark_Foundation/Iris.csv")
irisdata$Id <- NULL
irisdata<- irisdata[c(5,1,2,3,4)]
irisdata<- as.data.frame(irisdata)
irisdata$Species<- as.factor(irisdata$Species)
summary(irisdata)

pairs(irisdata[,2:5],col=irisdata[,1],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(irisdata$Species)),fill=c(1,2,3))

## K-means Clustering is used with unlabeled data, but in this case, we have a labeled dataset so we have to use the iris data without the Species column.

ggplot(irisdata, aes(PetalLengthCm, PetalWidthCm)) + geom_point(aes(col=Species), size=4)

## Comparing the predicted clusters with the original data

set.seed(240)
iris2 <- irisdata
iris2$Species <- NULL

(kmeans.result <- kmeans(iris2, 3, nstart = 20))

#Comparing the predicted clusters with the original data

ktab<- table(irisdata$Species, kmeans.result$cluster)
ktab

plot(iris2[c("SepalLengthCm", "SepalWidthCm")], col = kmeans.result$cluster)

## Plot of the clusters

data_for_clustering <- irisdata[,2:5]
clusters_iris <- kmeans(data_for_clustering, 3, nstart = 20)

clusplot(data_for_clustering, clusters_iris$cluster, color = TRUE, shade = TRUE)

## We find the exact number of clusters using the elbow method

tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  kmeans.result <- kmeans(iris2[,1:4], center=i, nstart=20)
  tot.withinss[i] <- kmeans.result$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)