library(stringr)
library(readr)
library(dplyr)
library(reldist)
library(ineq)


student <- read.csv("C:/Users/bahubali/Documents/2nd semester/Visual Analytics/studentalc.csv")
#student <- student %>%
  #mutate(alc_prob = ifelse(Dalc + Walc >= 6, "alc_p", "no_alc_p"))
alc_high <- ifelse(student$Dalc + student$Walc >= 6, "alc_p", "no_alc_p")
student<-data.frame(student, alc_high)
summary(student)
student$sex<-student %>% group_indices(sex)
student$famsize<-student %>% group_indices(famsize)
student$Pstatus<-student %>% group_indices(Pstatus)
student$Mjob<-student %>% group_indices(Mjob)
student$Fjob<-student %>% group_indices(Fjob)
student$reason<-student %>% group_indices(reason)
student$guardian<-student %>% group_indices(guardian)
student$schoolsup<-student %>% group_indices(schoolsup)
student$famsup<-student %>% group_indices(famsup)
student$paid<-student %>% group_indices(paid)
student$activities<-student %>% group_indices(activities)
student$nursery<-student %>% group_indices(nursery)
student$higher<-student %>% group_indices(higher)
student$internet<-student %>% group_indices(internet)
student$romantic<-student %>% group_indices(romantic)
student$alc_high<-student %>% group_indices(alc_high)
student$alc_prob<-student %>% group_indices(alc_prob)
student$absences<-student %>% group_indices(absences)

#Gini index for alc_prob:
gini_target<-ineq(student$alc_prob,type = "Gini")










