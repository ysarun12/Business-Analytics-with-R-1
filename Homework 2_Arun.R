
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--------Question 1---------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Loading the dataset
library(readr)
dungaree <- read_csv("C:/Users/Arun/Desktop/BA with R/Homework 2/dungaree.csv")
View(dungaree)

##Checking for outliers or missing values
summary(dungaree)
Mydata <- dungaree[2:5]
library(tidyr)
library(ggplot2)
Mydata %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value))+facet_wrap(~ key, scales = "free") +geom_histogram(bins = 10)

##Remving the unusually small values in fashion variable
newdata <- Mydata[ which(Mydata$FASHION >= 20), ]

##K - Means Clustering
install.packages("NbClust")
library(NbClust)
set.seed(42)
devAskNewPage(ask=TRUE)
dungaree.df.norm <- sapply(newdata, scale)
fit.km <- kmeans(dungaree.df.norm, 10, nstart=25)
fit.km$size

set.seed(42)
devAskNewPage(ask=TRUE)
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=10, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

##Wss plot
wssplot <- function(data, nc=15, seed=1234){wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:nc){
set.seed(seed)
wss[i] <- sum(kmeans(data, centers=i)$withinss)}
plot(1:nc, wss, type="b", xlab="Number of Clusters",
ylab="Within groups sum of squares")}
wssplot(dungaree.df.norm)


##Running k means clustering with 6 clusters again
set.seed(42)
devAskNewPage(ask=TRUE)
nc <- NbClust(dungaree.df.norm, min.nc=2, max.nc=6, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

##Checking properties of clusters formed
aggregate(pharma_data.norm, by=list(cluster=clusters), mean)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--------Question 2---------------
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hierarchical Cluster Analysis
install.packages("NbClust")
library(NbClust)
devAskNewPage(ask=TRUE)

##Loading the dataset
library(readr)
pharmaceuticals <- read_csv("C:/Users/Arun/Desktop/BA with R/Homework 2/Pharmaceuticals.csv")
View(pharmaceuticals)

##Using only numerical values for clustering
pharma_data <- pharmaceuticals[3:11]
pharma_data.norm <- sapply(pharma_data, scale)
nc <- NbClust(pharma_data.norm, distance="euclidean", min.nc=2, max.nc=10, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")

clusters <- cutree(fit.average, k=5)
table(clusters)
aggregate(pharma_data.norm, by=list(cluster=clusters), median)
rect.hclust(fit.average, k=5)

###K-means for the same data

library(NbClust)
set.seed(42)
devAskNewPage(ask=TRUE)
nckm <- NbClust(Pharma_data.norm, min.nc=2, max.nc=10, method="kmeans")
table(nckm$Best.n[1,])
barplot(table(nckm$Best.n[1,]), xlab="Number of Clusters", ylab="Number of criteria", main="Number of clusters chosen by criteria")




