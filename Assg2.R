pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)
library(dplyr)
library(cluster)
student<- read.csv("C:/Users/bahubali/Documents/2nd semester/Visual Analytics/Assignments/clustering.csv")
summary(student)

c1<- matrix(0,nrow=nrow(data),ncol=7)
for (n in c(2:8)) {
  c1[,n-1]<-kmeans(student,n)[1]$cluster
  c1
}
oldpar<- par(mfrow=c(3,3))
for(n1 in c(1:7)){
  plot(student, main= n1+1, col=c1[,n1])
}


# Silhouette method
sil<-vector(mode = "list",length = 7)
for(s in c(2:8)){
  sil[s-1]<-mean(silhouette(pam(student,s))[,3])
}
plot(c(2:8),sil,type="l",lwd=3)
lines(c(2:8),sil,type="p",cex=2,col=c("blue","red","blue","blue","blue","blue"), pch = 19)
# fviz_nbclust(student, kmeans, k.max=8, method = "silhouette")+
#   labs(subtitle = "Silhouette method")

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
plot(hclust_single,hang =-1,main = "Single Linkage" )
# Complete Linkage
hclust_complete <- hclust(dist_mat, method = 'complete')
plot(hclust_complete,hang =-1,main="Complete Linkage")

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

