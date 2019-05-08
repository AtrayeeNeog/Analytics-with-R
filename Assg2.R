pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)
library(dplyr)
library(cluster)
student<- read.csv("C:/Users/bahubali/Documents/2nd semester/Visual Analytics/Assignments/clustering.csv")
summary(student)
#K-means with K=2
cluster1<-kmeans(student,centers = 2)
student$cluster<-as.factor(cluster1$cluster)
plot(student,col=cluster1$cluster)
points(cluster1$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=3
cluster2<-kmeans(student,centers = 3)
student$cluster1<-as.factor(cluster2$cluster)
plot(student,col=cluster2$cluster)
points(cluster2$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=4
cluster3<-kmeans(student,centers = 4)
student$cluster2<-as.factor(cluster3$cluster)
plot(student,col=cluster3$cluster)
points(cluster3$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=5
cluster4<-kmeans(student,centers = 5)
student$cluster3<-as.factor(cluster4$cluster)
plot(student,col=cluster4$cluster)
points(cluster4$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=6
cluster5<-kmeans(student,centers = 6)
student$cluster4<-as.factor(cluster5$cluster)
plot(student,col=cluster5$cluster)
points(cluster5$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=7
cluster6<-kmeans(student,centers = 7)
student$cluster5<-as.factor(cluster6$cluster)
plot(student,col=cluster6$cluster)
points(cluster6$centers, col = 1:2, pch = 8, cex=2)
#K-means with K=8
cluster7<-kmeans(student,centers = 8)
student$cluster6<-as.factor(cluster7$cluster)
plot(student,col=cluster7$cluster)
points(cluster7$centers, col = 1:2, pch = 8, cex=2)

# Silhouette method
fviz_nbclust(student, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Hierachical Clustering

dm <- tribble(~p1,~p2,~p3,~p4,~p5,
              0.00, 0.02, 0.90, 0.36, 0.53,
              0.02, 0.00, 0.65, 0.15, 0.24,
              0.90, 0.65, 0.00, 0.59, 0.45,
              0.36, 0.15, 0.59, 0.00, 0.56,
              0.53, 0.24, 0.45, 0.56, 0.00) %>% as.matrix()
rownames(dm) <- letters[1:5]
colnames(dm) <- letters[1:5]
dm
dist_mat <- dist(dm, method = 'euclidean')
# Single Linkage
hclust_single <- hclust(dist_mat, method = 'single')
plot(hclust_single)
# Complete Linkage
hclust_complete <- hclust(dist_mat, method = 'complete')
plot(hclust_complete)

#k-means for given set of points
dat <- tibble(
     x = c(2.0, 2.0, 2.0, 2.5, 2.5, 3.0, 4.0, 4.0, 4.5, 4.5, 4.5 , 4.5),
     y = c(1.0, 1.5, 2.0, 1.0, 2.0, 4.0, 1.0, 2.5, 1.0, 1.5, 2.5 , 3.0)
   )
c1<- dat[1,]  
c2<- dat[2,]
c3<- dat[3,]

result<-kmeans(dat, rbind(c1,c2,c3))
plot(dat,col=result$cluster)

