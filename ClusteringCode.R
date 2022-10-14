# Assignment 2: Cluster Analysis of Housing Data

library(ggplot2)
library(gridExtra)
library(reshape)
library(factoextra)
library(knitr)
library(mclust)
library(caret)
library(NbClust)
library(ggmap)

setwd("C:/Users/aa0843/Documents/MSDS 411/Assignment 2")

alldata = read.csv("Melbourne_housing_FULL.csv",header=TRUE)
houses = alldata[complete.cases(alldata),]
# convert Distance to numeric & YearBuilt to Age
houses$Distance <- as.numeric(houses$Distance) 
houses$Age <- 2021 - houses$YearBuilt
print(str(houses))

workdata = houses[,c("Rooms","Price","Distance","Bedroom2",
                     "Bathroom","Car","Landsize","BuildingArea","Age")]
print(str(workdata))

####################### EDA ####################### 
summary(alldata)
summary(workdata)

kable(table(houses$Suburb))

# Numeric variables: histogram of each
plot1 <- ggplot(workdata, aes(x=Rooms)) + 
  geom_histogram(fill="steelblue4", color="white", binwidth=1) +
  scale_x_continuous(breaks=seq(0,12,1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot2 <- ggplot(workdata, aes(x=Price)) + 
  geom_histogram(fill="steelblue4", color="white") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot3 <- ggplot(workdata, aes(x=Distance)) + 
  geom_histogram(fill="steelblue4", color="white") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot4 <- ggplot(workdata, aes(x=Bedroom2)) + 
  geom_histogram(fill="steelblue4", color="white", binwidth=1) +
  scale_x_continuous(breaks=seq(0,12,1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot5 <- ggplot(workdata, aes(x=Bathroom)) + 
  geom_histogram(fill="steelblue4", color="white", binwidth=1) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot6 <- ggplot(workdata, aes(x=Car)) + 
  geom_histogram(fill="steelblue4", color="white", binwidth=1) +
  scale_x_continuous(breaks=seq(0,12,1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot7 <- ggplot(workdata, aes(x=Landsize)) + 
  geom_histogram(fill="steelblue4", color="white") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot8 <- ggplot(workdata, aes(x=BuildingArea)) + 
  geom_histogram(fill="steelblue4", color="white") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
plot9 <- ggplot(workdata, aes(x=Age)) + 
  geom_histogram(fill="steelblue4", color="white") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3)

# Numeric variables: boxplot of each to see outliers
meltData <- melt(workdata)
ggplot(meltData, aes(factor(variable), value)) + 
  geom_boxplot(fill="steelblue4") + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  facet_wrap(~variable, scale="free")

par(mfrow=c(1,1))




####################### Standardized Data (mean 0, std 1), Outliers Retained ####################### 
# scale/standardize data (mean 0, sd 1)
workstd <- scale(workdata)

# k-means clustering
# Determine number of k clusters - option 1
fviz_nbclust(workstd, kmeans, method = "wss")
# suggests 4

# Determine number of k clusters - option 2
rng<-2:20 #K from 2 to 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(as.matrix(workstd),centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

# Determine number of k clusters - option 3
# NbClust package provides 30 indices for determining the number of clusters and proposes to user the best 
# clustering scheme from the different results obtained by varying all combinations of number of clusters, 
# distance measures, and clustering methods.

# NbClust(data=workstd, distance="euclidean", min.nc=2, max.nc=8, method="kmeans", index="all", alphaBeale=0.1)
# suggests 2 or 4 clusters


# 2 clusters
km.std2<-kmeans(workstd, 2, nstart=50) 
kmstd2clust <- km.std2$cluster
round(km.std2$centers,2)
round(km.std2$betweenss/km.std2$totss,2)
# km.std2$cluster # if you just want cluster vector to append to data set
# visualize clusters
par(mfrow=c(1,1))
plot(workstd, col=(km.std2$cluster), main="K-Means Clustering Results with K=2", xlab=" ", pch=20,cex=2)
fviz_cluster(km.std2, data = workstd)

# 3 clusters
km.std3<-kmeans(workstd, 3, nstart=50)
kmstd3clust <- km.std3$cluster
round(km.std3$centers,2)
round(km.std3$betweenss/km.std3$totss,2)
plot(workstd, col=(km.std3$cluster), main="K-Means Clustering Results with K=3", xlab=" ", pch=20,cex=2)
fviz_cluster(km.std3, data = workstd)

newwork <- houses
newwork <-cbind.data.frame(newwork,kmstd3clust)
table(newwork$kmstd3clust)
round(aggregate(newwork, by=list(cluster = kmstd3clust), mean),1)

# 4 clusters
km.std4<-kmeans(workstd, 4, nstart=50) 
kmstd4clust <- km.std4$cluster
round(km.std4$centers,2)
round(km.std4$betweenss/km.std4$totss,2)
plot(workstd, col=(km.std4$cluster), main="K-Means Clustering Results with K=4", xlab=" ", pch=20,cex=2)
fviz_cluster(km.std4, data = workstd)

newwork <-cbind.data.frame(newwork,kmstd4clust)
table(newwork$kmstd4clust) # very few obs in group 2
round(aggregate(newwork, by=list(cluster = kmstd4clust), mean),1)

# 5 clusters
km.std5<-kmeans(workstd, 5, nstart=50) 
kmstd5clust <- km.std5$cluster
round(km.std5$centers,2)
round(km.std5$betweenss/km.std5$totss,2)
plot(workstd, col=(km.std5$cluster), main="K-Means Clustering Results with K=5", xlab=" ", pch=20,cex=2)
fviz_cluster(km.std5, data = workstd)

newwork <-cbind.data.frame(newwork,kmstd5clust)
table(newwork$kmstd5clust) # very few obs in group 2
round(aggregate(newwork, by=list(cluster = kmstd5clust), mean),1)

# 6 clusters
km.std6<-kmeans(workstd, 6, nstart=50) 
kmstd6clust <- km.std6$cluster
round(km.std6$centers,2)
round(km.std6$betweenss/km.std6$totss,2)
plot(workstd, col=(km.std6$cluster), main="K-Means Clustering Results with K=6", xlab=" ", pch=20,cex=2)
fviz_cluster(km.std6, data = workstd)



# hierarchical clustering
hc.complete<-hclust(dist(workstd), method="complete") 
hc.average<-hclust(dist(workstd), method="average")
hc.single<-hclust(dist(workstd), method="single") 
hc.ward<-hclust(dist(workstd), method="ward.D") 

# plot dendrograms
plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.ward,main="Ward Linkage", xlab=" ",sub=" ",cex=0.9)

# Ward looks best - cut at 4 clusters
hcward4clust<-cutree(hc.ward, 4)
newwork<-cbind.data.frame(newwork, hcward4clust)
table(newwork$hcward4) # more substantial obs in each group even with 4 clusters
round(aggregate(newwork, by=list(cluster = hcward4clust), mean),1)



####################### Normalized Data (scaled from 0 to 1), Outliers Retained ####################### 
# Normalize data
process <- preProcess(workdata, method=c("range"))
worknorm <- predict(process, workdata)

# alternative method
# norm_minmax <- function(x){
#   (x- min(x)) /(max(x)-min(x))
# }
# worknorm <- as.data.frame(lapply(workdata, norm_minmax))
# summary(worknorm)

# k-means clustering
# Determine number of k clusters
fviz_nbclust(worknorm, kmeans, method = "wss")
# suggests 3-4


# 2 clusters
km.norm2<-kmeans(worknorm, 2, nstart=50) 
kmnorm2clust <- km.norm2$cluster
round(km.norm2$centers,2)
round(km.norm2$betweenss/km.norm2$totss,2)
# km.norm2$cluster # if you just want cluster vector to append to data set
# visualize clusters
par(mfrow=c(1,1))
plot(worknorm, col=(km.norm2$cluster), main="K-Means Clustering Results with K=2", xlab=" ", pch=20,cex=2)
fviz_cluster(km.norm2, data = worknorm)

# 3 clusters
km.norm3<-kmeans(worknorm, 3, nstart=50)
kmnorm3clust <- km.norm3$cluster
round(km.norm3$centers,2)
round(km.norm3$betweenss/km.norm3$totss,2)
plot(worknorm, col=(km.norm3$cluster), main="K-Means Clustering Results with K=3", xlab=" ", pch=20,cex=2)
fviz_cluster(km.norm3, data = worknorm)

newwork <-cbind.data.frame(newwork,kmnorm3clust)
table(newwork$kmnorm3clust)
round(aggregate(newwork, by=list(cluster = kmnorm3clust), mean),1)

# 4 clusters
km.norm4<-kmeans(worknorm, 4, nstart=50) 
kmnorm4clust <- km.norm4$cluster
round(km.norm4$centers,2)
round(km.norm4$betweenss/km.norm4$totss,2)
plot(worknorm, col=(km.norm4$cluster), main="K-Means Clustering Results with K=4", xlab=" ", pch=20,cex=2)
fviz_cluster(km.norm4, data = worknorm)

newwork <-cbind.data.frame(newwork,kmnorm4clust)
table(newwork$kmnorm4clust) # very few obs in group 2
round(aggregate(newwork, by=list(cluster = kmnorm4clust), mean),1)

# 5 clusters
km.norm5<-kmeans(worknorm, 5, nstart=50) 
kmnorm5clust <- km.norm5$cluster
round(km.norm5$centers,2)
round(km.norm5$betweenss/km.norm5$totss,2)
plot(worknorm, col=(km.norm5$cluster), main="K-Means Clustering Results with K=5", xlab=" ", pch=20,cex=2)
fviz_cluster(km.norm5, data = worknorm)

newwork <-cbind.data.frame(newwork,kmnorm5clust)
table(newwork$kmnorm5clust) # very few obs in group 2
round(aggregate(newwork, by=list(cluster = kmnorm5clust), mean),1)



# hierarchical clustering
hc.complete<-hclust(dist(worknorm), method="complete") 
hc.average<-hclust(dist(worknorm), method="average")
hc.single<-hclust(dist(worknorm), method="single") 
hc.ward<-hclust(dist(worknorm), method="ward.D") 

# plot dendrograms
plot(hc.complete,main="Complete Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.average,main="Average Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.single,main="Single Linkage", xlab=" ",sub=" ",cex=0.9)
plot(hc.ward,main="Ward Linkage", xlab=" ",sub=" ",cex=0.9)

# Ward looks best - cut at 4 clusters
hcwardn4clust<-cutree(hc.ward, 4)
newwork<-cbind.data.frame(newwork, hcwardn4clust)
table(newwork$hcwardn4clust) # more substantial obs in each group even with 4 clusters
round(aggregate(newwork, by=list(cluster = hcwardn4clust), mean),1)



####################### Normalized Data (scaled from 0 to 1) & Outliers Removed ####################### 
# extreme outliers
workdataout <- subset(workdata, !(Age > 221 | Price > 4000000 | BuildingArea>800 | Landsize>6000) )
# normalize
process <- preProcess(workdataout, method=c("range"))
worknormout <- predict(process, workdataout)

# k-means clustering
# Determine number of k clusters
fviz_nbclust(worknormout, kmeans, method = "wss")
# suggests 3-4


# 3 clusters
km.normout3<-kmeans(worknormout, 3, nstart=50)
kmnormout3clust <- km.normout3$cluster
round(km.normout3$centers,2)
round(km.normout3$betweenss/km.normout3$totss,2)
plot(worknormout, col=(km.normout3$cluster), main="K-Means Clustering Results with K=3", xlab=" ", pch=20,cex=2)
fviz_cluster(km.normout3, data = worknormout)

newworkout <- subset(houses, !(Age > 221 | Price > 4000000 | BuildingArea>800 | Landsize>6000) )
newworkout <-cbind.data.frame(newworkout,kmnormout3clust)
table(newworkout$kmnormout3clust)
round(aggregate(newworkout, by=list(cluster = kmnormout3clust), mean),1)

# 4 clusters
km.normout4<-kmeans(worknormout, 4, nstart=50) 
kmnormout4clust <- km.normout4$cluster
round(km.normout4$centers,2)
round(km.normout4$betweenss/km.normout4$totss,2)
plot(worknormout, col=(km.normout4$cluster), main="K-Means Clustering Results with K=4", xlab=" ", pch=20,cex=2)
fviz_cluster(km.normout4, data = worknormout)

newworkout <-cbind.data.frame(newworkout,kmnormout4clust)
table(newworkout$kmnormout4clust)
round(aggregate(newworkout, by=list(cluster = kmnormout4clust), mean),1)

# 5 clusters
km.normout5<-kmeans(worknormout, 5, nstart=50) 
kmnormout5clust <- km.normout5$cluster
round(km.normout5$centers,2)
round(km.normout5$betweenss/km.normout5$totss,2)
plot(worknormout, col=(km.normout5$cluster), main="K-Means Clustering Results with K=5", xlab=" ", pch=20,cex=2)
fviz_cluster(km.normout5, data = worknormout)

newworkout <-cbind.data.frame(newworkout,kmnormout5clust)
table(newworkout$kmnormout5clust)
round(aggregate(newworkout, by=list(cluster = kmnormout5clust), mean),1)

# hierarchical clustering
hc.ward<-hclust(dist(worknormout), method="ward.D") 

# plot dendrograms
plot(hc.ward,main="Ward Linkage", xlab=" ",sub=" ",cex=0.9)

# Ward looks best - cut at 4 clusters
hcwardout4clust<-cutree(hc.ward, 4)
newworkout <-cbind.data.frame(newworkout,hcwardout4clust)
table(newworkout$hcwardout4clust)
round(aggregate(newworkout, by=list(cluster = hcwardout4clust), mean),1)

# try 7
hcwardout7clust<-cutree(hc.ward, 7)
newworkout <-cbind.data.frame(newworkout,hcwardout7clust)
table(newworkout$hcwardout7clust)
round(aggregate(newworkout, by=list(cluster = hcwardout7clust), mean),1)



####################### PCA before Clustering  ####################### 
# PCA then 4 kmeans clusters
binarypca <- princomp(worknorm, cor = T) 
binarypca$loadings 

km.normpca4 <- kmeans(binarypca$scores, 4)
plot(worknorm, col=(km.normpca4$cluster), main="K-Means Clustering Results with PCA & K=4", xlab=" ", pch=20,cex=2)
fviz_cluster(km.normpca4, data = worknorm)

kmnormpca4clust <- km.normpca4$cluster
round(km.normpca4$centers,2)
round(km.normpca4$betweenss/km.normpca4$totss,2)
# Lower SS then normal k-means without PCA. WIll not proceed with this

km.normpca5 <- kmeans(binarypca$scores, 5)
plot(worknorm, col=(km.normpca5$cluster), main="K-Means Clustering Results with PCA & K=5", xlab=" ", pch=20,cex=2)
fviz_cluster(km.normpca5, data = worknorm)

kmnormpca5clust <- km.normpca5$cluster
round(km.normpca5$centers,2)
round(km.normpca5$betweenss/km.normpca4$totss,2)


####################### Compare cluster methods  ####################### 
# Map of clusters
register_google(key="AIzaSyCn0QWET4S6HeYrtPe1W49JgnrukG7ESSk") # please do not share
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmstd3clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmstd4clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmstd5clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(hcward4clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmnorm3clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmnorm4clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(kmnorm5clust))
qmplot(Longtitude, Lattitude, data = newwork,  color = factor(hcwardn4clust))
qmplot(Longtitude, Lattitude, data = newworkout,  color = factor(kmnormout3clust))
qmplot(Longtitude, Lattitude, data = newworkout,  color = factor(kmnormout4clust))
qmplot(Longtitude, Lattitude, data = newworkout,  color = factor(kmnormout5clust))
qmplot(Longtitude, Lattitude, data = newworkout,  color = factor(hcwardout4clust))
qmplot(Longtitude, Lattitude, data = newworkout,  color = factor(hcwardout7clust))

# Means by cluster
round(aggregate(newwork, by=list(cluster = kmstd3clust), mean),1)
round(aggregate(newwork, by=list(cluster = kmstd4clust), mean),1) # only 12 obs in group 1
round(aggregate(newwork, by=list(cluster = kmstd5clust), mean),1) 
round(aggregate(newwork, by=list(cluster = hcward4clust), mean),1)
round(aggregate(newwork, by=list(cluster = kmnorm3clust), mean),1)
clust4 <- round(aggregate(newwork, by=list(cluster = kmnorm4clust), mean),1)
clust4
write.csv(clust4, "clust4.csv")
finalclust <- round(aggregate(newwork, by=list(cluster = kmnorm5clust), mean),1)
finalclust
write.csv(finalclust, "finalclust.csv")
round(aggregate(newwork, by=list(cluster = hcwardn4clust), mean),1)
round(aggregate(newworkout, by=list(cluster = kmnormout3clust), mean),1)
round(aggregate(newworkout, by=list(cluster = kmnormout4clust), mean),1)
round(aggregate(newworkout, by=list(cluster = kmnormout5clust), mean),1)
round(aggregate(newworkout, by=list(cluster = hcwardout4clust), mean),1)
round(aggregate(newworkout, by=list(cluster = hcwardout7clust), mean),1)
# Hierarchical ones don't make as much sense/bit harder to interpret

# SS - proportion of variability explained
round(km.std3$betweenss/km.std3$totss,2)
round(km.norm3$betweenss/km.norm3$totss,2)
round(km.normout3$betweenss/km.normout3$totss,2)
round(km.std4$betweenss/km.std4$totss,2)
round(km.norm4$betweenss/km.norm4$totss,2)
round(km.normout4$betweenss/km.normout3$totss,2)
round(km.normpca4$betweenss/km.normpca4$totss,2)
round(km.std5$betweenss/km.std5$totss,2)
round(km.norm5$betweenss/km.norm5$totss,2)
round(km.normout5$betweenss/km.normout5$totss,2)
round(km.normpca5$betweenss/km.normpca5$totss,2)
# Highest SS explained is using normal adjustment with outliers kept in

# ARI to compare similarities between some methods
adjustedRandIndex(kmstd4clust, hcward4clust) # only 32% of obs in the same clusters
adjustedRandIndex(kmnorm4clust, hcwardn4clust) # 49% same
adjustedRandIndex(kmstd3clust, kmnorm3clust) # 48% same
adjustedRandIndex(kmstd4clust, kmnorm4clust) # 50% same

# Distribution of clusters by house type
addmargins(table(newwork$Type, newwork$kmstd3clust))
addmargins(table(newwork$Type, newwork$kmstd4clust))
addmargins(table(newwork$Type, newwork$kmstd5clust))
addmargins(table(newwork$Type, newwork$hcward4clust))
addmargins(table(newwork$Type, newwork$kmnorm3clust))
addmargins(table(newwork$Type, newwork$kmnorm4clust))
clusttype_cnt <- addmargins(table(newwork$Type, newwork$kmnorm5clust))
clusttype_cnt
write.csv(clusttype_cnt, "clusttype_cnt.csv")
addmargins(table(newwork$Type, newwork$hcwardn4clust))
addmargins(table(newworkout$Type, newworkout$kmnormout3clust))
addmargins(table(newworkout$Type, newworkout$kmnormout4clust))
addmargins(table(newworkout$Type, newworkout$kmnormout5clust))
addmargins(table(newworkout$Type, newworkout$hcwardout4clust))
addmargins(table(newworkout$Type, newworkout$hcwardout7clust))

# Distribution of clusters by suburb
clustsub_cnt <- addmargins(table(newwork$Suburb, newwork$kmnorm5clust))
clustsub_cnt
write.csv(clustsub_cnt, "clustsub_cnt.csv")


