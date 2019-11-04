library(maps)
library(dplyr)
library(dbplyr)
library(fpc)
library(dbscan)
library(ggplot2)
library(factoextra)
library("tidyverse")
#a)
#Data Retrieval of cities > 50k population
data("world.cities")
cities <- world.cities %>% filter(pop>=50000)
summary(cities)

#DBscan used to find clusters
clusters <- dbscan(select(cities, lat, long), eps = 0.15, minPts = 8) 
                   
cities$cluster <- clusters$cluster
groups  <- cities %>% filter(cluster != 0)
noise  <- cities %>% filter(cluster == 0)
summary(groups)

ggplot(cities, aes(x = long, y = lat)) + 
  geom_point(aes(fill = "grey"), noise) +
  geom_point(aes(colour = as.factor(cluster)), groups, size = 4)

#No of clusters
no_of_clusters<-unique(groups$cluster)
length(no_of_clusters)

#No of core objects
length(unique(groups$name))

#No of noise objects
length(unique(noise$name))

plot(lat ~ long, data = cities, col = (clusters$cluster+ 1L), pch=19)
print(clusters)
groups$name
names<- cities %>% filter(cluster==1)
unique(names$country.etc)
most_clusters <-filter(cities, cities$cluster %in% 1:103)
#countries<- cities %>% filter(cluster==1 | cluster==7 | cluster==10 | cluster==17)
#unique(countries$country.etc)

#cluster1<-dbscan(select(cities, lat, long), eps = 0.15, minPts = 8)
 
filter(cities, cities$cluster %in% 1:103)
