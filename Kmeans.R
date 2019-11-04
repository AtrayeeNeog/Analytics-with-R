dat <- tibble(x = c(2.0, 2.0, 2.0, 2.5, 2.5, 3.0, 4.0, 4.0, 4.5, 4.5, 4.5 , 4.5),y = c(1.0, 1.5, 2.0, 1.0, 2.0, 4.0, 1.0, 2.5, 1.0, 1.5, 2.5 , 3.0))
xy = c(2.0, 2.0, 2.0, 2.5, 2.5, 3.0, 4.0, 4.0, 4.5, 4.5, 4.5, 4.5, 1.0, 1.5, 2.0, 1.0, 2.0, 4.0, 1.0, 2.5, 1.0, 1.5, 2.5, 3.0)
xystart = c(2.0, 2.0, 2.0, 1.0, 1.5, 2.0)
# Solution of task 1...

datmatrix <- matrix(xy, ncol=2, nrow=12)
startmatrix <- matrix(xystart, ncol=2, nrow = 3)


clusters1 <- kmeans(datmatrix, centers = startmatrix, iter.max=1, algorithm="MacQueen")
plot(datmatrix, col=clusters1$cluster, cex=2, pch = 20, xlab="x", ylab="y", main="iteration 1")
points(clusters1$centers, col=1:5, pch=10, cex=2)
centers1 <-matrix(clusters1$centers,nrow=3, ncol=2, dimnames=list(c("1","2","3"),c("x","y")))


clusters2 <- kmeans(datmatrix, centers = startmatrix, iter.max=2, algorithm="MacQueen")
plot(datmatrix, col=clusters2$cluster, cex=2, pch = 20, xlab="x", ylab="y", main="iteration 2")
points(clusters2$centers, col=1:3, pch=10, cex=2)
centers2 <-matrix(clusters2$centers,nrow=3, ncol=2)


clusters3 <- kmeans(datmatrix, centers = startmatrix, iter.max=3, algorithm="MacQueen")
plot(datmatrix, col=clusters3$cluster, cex=2, pch = 20, xlab="x", ylab="y", main="iteration 3")
points(clusters3$centers, col=1:3, pch=10, cex=2)
centers3 <-matrix(clusters3$centers,nrow=3, ncol=2)


centers <- rbind(centers1, centers2, centers3)
centers

dat$dist1_iter1=sqrt((dat$x-centers[1,1])^2+(dat$y-centers[1,2])^2)
dat$dist2_iter1=sqrt((dat$x-centers[2,1])^2+(dat$y-centers[2,2])^2)
dat$dist3_iter1=sqrt((dat$x-centers[3,1])^2+(dat$y-centers[3,2])^2)
dat$dist1_iter2=sqrt((dat$x-centers[4,1])^2+(dat$y-centers[4,2])^2)
dat$dist2_iter2=sqrt((dat$x-centers[5,2])^2+(dat$y-centers[5,2])^2)
dat$dist3_iter2=sqrt((dat$x-centers[6,1])^2+(dat$y-centers[6,2])^2)

dat