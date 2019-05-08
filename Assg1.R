library(RSQLite)
library(DBI)
library(dplyr)
mydb<-dbConnect(RSQLite::SQLite(), "C:\\Users\\bahubali\\Documents\\2nd semester\\Visual Analytics\\Assignments\\EuropeanSoccer.sqlite")
mydb

countries<-dbGetQuery(mydb,'SELECT * FROM Country')
head(countries)
leagues<-dbGetQuery(mydb,'SELECT * FROM League')
head(leagues)
match<-dbGetQuery(mydb,'SELECT * FROM Match')
head(match)
goalsE<-data.frame(dbGetQuery(mydb,'SELECT country_id, date,
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id=1729 '))
goalsS<- data.frame(dbGetQuery(mydb,'SELECT country_id, date, home_team_goal, 
                          away_team_goal 
                               FROM Match WHERE country_id=21518'))
goalsG<-data.frame(dbGetQuery(mydb,'SELECT country_id, date,
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 7809'))
goalsI<-data.frame(dbGetQuery(mydb,'SELECT country_id, date,
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 10257 '))
goalsB<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 1 '))
goalsF<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 4769 '))
goalsN<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id=  13274 '))
goalsPl<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id=  15722 '))
goalsPo<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 17642 '))
goalsSc<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id= 19694 '))
goalsSz<-data.frame(dbGetQuery(mydb,'SELECT country_id, 
                   home_team_goal, away_team_goal FROM Match 
                   WHERE country_id=  24558 '))


sumE<-sum(rowsum(colSums(goalsE[2:3]), group =c(1,2)))
sumS<-sum(rowsum(colSums(goalsS[2:3]), group =c(1,2)))
sumG<-sum(rowsum(colSums(goalsG[2:3]), group =c(1,2)))
sumI<-sum(rowsum(colSums(goalsI[2:3]), group =c(1,2)))

sumE/NROW(goalsE)
sumS/NROW(goalsS)
sumG/NROW(goalsG)
sumI/NROW(goalsI)

barplot(c(sumE/NROW(goalsE),
          sumS/NROW(goalsS),
          sumG/NROW(goalsG),
          sumI/NROW(goalsI)), col=c('red','blue','green','yellow'), 
        names.arg= c('England','Spain','Germany','Italy'))

avgE<- mean(c(goalsE$home_team_goal,goalsE$away_team_goal))
avgS<- mean(c(goalsS$home_team_goal,goalsS$away_team_goal))
avgG<- mean(c(goalsG$home_team_goal,goalsG$away_team_goal))
avgI<- mean(c(goalsI$home_team_goal,goalsI$away_team_goal))
barplot(c(avgE,avgS,avgG,avgI), col=c('red','blue','green','yellow'), 
        names.arg= c('England','Spain','Germany','Italy'), 
        main = 'Average of Goals per Match' )


avgP<-mean(c(goalsE$home_team_goal,goalsE$away_team_goal,
             goalsS$home_team_goal,goalsS$away_team_goal,
             goalsG$home_team_goal,goalsG$away_team_goal,
             goalsI$home_team_goal,goalsI$away_team_goal))

avgU<-mean(c(goalsB$home_team_goal,goalsB$away_team_goal,
             goalsF$home_team_goal,goalsF$away_team_goal,
             goalsN$home_team_goal,goalsN$away_team_goal,
             goalsPl$home_team_goal,goalsPl$away_team_goal,
             goalsPo$home_team_goal,goalsPo$away_team_goal,
             goalsSc$home_team_goal,goalsSc$away_team_goal,
             goalsSz$home_team_goal,goalsSz$away_team_goal))

barplot(c(avgP, avgU), col=c('red','gray'), 
        names.arg= c('Popular Leagues','Others'),
        main = 'Comparison between Average Goals between the Popular 
        Four and the Others' )

medP<-median(c(goalsE$home_team_goal,goalsE$away_team_goal,
             goalsS$home_team_goal,goalsS$away_team_goal,
             goalsG$home_team_goal,goalsG$away_team_goal,
             goalsI$home_team_goal,goalsI$away_team_goal))
medU<-median(c(goalsB$home_team_goal,goalsB$away_team_goal,
             goalsF$home_team_goal,goalsF$away_team_goal,
             goalsN$home_team_goal,goalsN$away_team_goal,
             goalsPl$home_team_goal,goalsPl$away_team_goal,
             goalsPo$home_team_goal,goalsPo$away_team_goal,
             goalsSc$home_team_goal,goalsSc$away_team_goal,
             goalsSz$home_team_goal,goalsSz$away_team_goal))
barplot(c(medP, medU), col=c('red','gray'), 
        names.arg= c('Popular Leagues','Others'),
        main = 'Comparison between Median of Goals between the Popular 
        Four and the Others' )
sdP<-sd(c(goalsE$home_team_goal,goalsE$away_team_goal,
               goalsS$home_team_goal,goalsS$away_team_goal,
               goalsG$home_team_goal,goalsG$away_team_goal,
               goalsI$home_team_goal,goalsI$away_team_goal))
sdU<-sd(c(goalsB$home_team_goal,goalsB$away_team_goal,
               goalsF$home_team_goal,goalsF$away_team_goal,
               goalsN$home_team_goal,goalsN$away_team_goal,
               goalsPl$home_team_goal,goalsPl$away_team_goal,
               goalsPo$home_team_goal,goalsPo$away_team_goal,
               goalsSc$home_team_goal,goalsSc$away_team_goal,
               goalsSz$home_team_goal,goalsSz$away_team_goal))
barplot(c(sdP, sdU), col=c('red','gray'), 
        names.arg= c('Popular Leagues','Others'),
        main = 'Comparison between the Standard Deviation of Goals between the Popular 
        Four and the Others' )
varP<-var(c(goalsE$home_team_goal,goalsE$away_team_goal,
          goalsS$home_team_goal,goalsS$away_team_goal,
          goalsG$home_team_goal,goalsG$away_team_goal,
          goalsI$home_team_goal,goalsI$away_team_goal))
varU<-var(c(goalsB$home_team_goal,goalsB$away_team_goal,
          goalsF$home_team_goal,goalsF$away_team_goal,
          goalsN$home_team_goal,goalsN$away_team_goal,
          goalsPl$home_team_goal,goalsPl$away_team_goal,
          goalsPo$home_team_goal,goalsPo$away_team_goal,
          goalsSc$home_team_goal,goalsSc$away_team_goal,
          goalsSz$home_team_goal,goalsSz$away_team_goal))
barplot(c(varP, varU), col=c('red','gray'), 
        names.arg= c('Popular Leagues','Others'),
        main = 'Comparison between Variance of Goals between the Popular 
        Four and the Others' )
rangeP<-range(c(goalsE$home_team_goal,goalsE$away_team_goal,
            goalsS$home_team_goal,goalsS$away_team_goal,
            goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal))
rangeU<-range(c(goalsB$home_team_goal,goalsB$away_team_goal,
            goalsF$home_team_goal,goalsF$away_team_goal,
            goalsN$home_team_goal,goalsN$away_team_goal,
            goalsPl$home_team_goal,goalsPl$away_team_goal,
            goalsPo$home_team_goal,goalsPo$away_team_goal,
            goalsSc$home_team_goal,goalsSc$away_team_goal,
            goalsSz$home_team_goal,goalsSz$away_team_goal))

barplot(c(rangeP,rangeU), col=c('red','gray'))

iqrP<-IQR(c(goalsE$home_team_goal,goalsE$away_team_goal,
                goalsS$home_team_goal,goalsS$away_team_goal,
                goalsG$home_team_goal,goalsG$away_team_goal,
                goalsI$home_team_goal,goalsI$away_team_goal))
iqrU<-IQR(c(goalsB$home_team_goal,goalsB$away_team_goal,
                goalsF$home_team_goal,goalsF$away_team_goal,
                goalsN$home_team_goal,goalsN$away_team_goal,
                goalsPl$home_team_goal,goalsPl$away_team_goal,
                goalsPo$home_team_goal,goalsPo$away_team_goal,
                goalsSc$home_team_goal,goalsSc$away_team_goal,
                goalsSz$home_team_goal,goalsSz$away_team_goal))
iqrP
iqrU
barplot(c(iqrP,iqrU),col=c('red','gray'))

goalsH <-(c(goalsE$home_team_goal,goalsB$home_team_goal,goalsF$home_team_goal,
            goalsG$home_team_goal,goalsI$home_team_goal,goalsN$home_team_goal,
            goalsPl$home_team_goal,goalsPo$home_team_goal,goalsS$home_team_goal,
            goalsSc$home_team_goal,goalsSz$home_team_goal))
goalsA <-(c(goalsE$away_team_goal,goalsB$away_team_goal,goalsF$away_team_goal,
                goalsG$away_team_goal,goalsI$away_team_goal,goalsN$away_team_goal,
                goalsPl$away_team_goal,goalsPo$away_team_goal,goalsS$away_team_goal,
                goalsSc$away_team_goal,goalsSz$away_team_goal))

boxplot(goalsH, goalsA)

summer1<- filter(match, match$date>as.Date("2008-05-15") & match$date<as.Date("2008-09-21"))
summer2<- filter(match, match$date>as.Date("2009-05-15") & match$date<as.Date("2009-09-21"))
summer3<- filter(match, match$date>as.Date("2010-05-15") & match$date<as.Date("2010-09-21"))
summer4<- filter(match, match$date>as.Date("2011-05-15") & match$date<as.Date("2011-09-21"))
summer5<- filter(match, match$date>as.Date("2012-05-15") & match$date<as.Date("2012-09-21"))
summer6<- filter(match, match$date>as.Date("2013-05-15") & match$date<as.Date("2013-09-21"))
summer7<- filter(match, match$date>as.Date("2014-05-15") & match$date<as.Date("2014-09-21"))
summer8<- filter(match, match$date>as.Date("2015-05-15") & match$date<as.Date("2015-09-21"))



#summerG <- c(summer1$home_team_goal,summer1$away_team_goal,summer2$home_team_goal,summer2$away_team_goal,
               #summer3$home_team_goal,summer3$away_team_goal,summer4$home_team_goal,summer4$away_team_goal,
               #summer5$home_team_goal,summer5$away_team_goal,summer6$home_team_goal,summer6$away_team_goal,
   #summer7$home_team_goal,summer7$away_team_goal,summer8$home_team_goal,summer8$away_team_goal)
summerG1 <- (sum(summer1$home_team_goal,summer1$away_team_goal)/nrow(summer1))
summerG2 <- (sum(summer2$home_team_goal,summer2$away_team_goal)/nrow(summer2))
summerG3 <- (sum(summer3$home_team_goal,summer3$away_team_goal)/nrow(summer3))
summerG4 <- (sum(summer4$home_team_goal,summer4$away_team_goal)/nrow(summer4))
summerG5 <- (sum(summer5$home_team_goal,summer5$away_team_goal)/nrow(summer5))
summerG6 <- (sum(summer6$home_team_goal,summer6$away_team_goal)/nrow(summer6))
summerG7 <- (sum(summer7$home_team_goal,summer7$away_team_goal)/nrow(summer7))
summerG8 <- (sum(summer8$home_team_goal,summer8$away_team_goal)/nrow(summer8))

summerG <- c(summerG1,summerG2,summerG3,summerG4,summerG5,summerG6,summerG7,summerG8)
year1 <- c(summer1$date,summer2$date,summer3$date,summer4$date,summer5$date,summer6$date,
           summer7$date,summer8$date)
year<- substring(year1,1,4)
uyear<- unique(year,incomparables = FALSE)



rest1 <- filter(match, (match$date!=summer1$date))
rest2 <- filter(match, (match$date!=summer2$date))
rest3 <- filter(match, (match$date!=summer3$date))
rest4 <- filter(match, (match$date!=summer4$date))
rest5 <- filter(match, (match$date!=summer5$date))                
rest6 <- filter(match, (match$date!=summer6$date))
rest7 <- filter(match, (match$date!=summer7$date))
rest8 <- filter(match, (match$date!=summer8$date))

restG1 <- (sum(rest1$home_team_goal,rest1$away_team_goal)/nrow(rest1))
restG2 <- (sum(rest2$home_team_goal,rest2$away_team_goal)/nrow(rest2))
restG3 <- (sum(rest3$home_team_goal,rest3$away_team_goal)/nrow(rest3))
restG4 <- (sum(rest4$home_team_goal,rest4$away_team_goal)/nrow(rest4))
restG5 <- (sum(rest5$home_team_goal,rest5$away_team_goal)/nrow(rest5))
restG6 <- (sum(rest6$home_team_goal,rest6$away_team_goal)/nrow(rest6))
restG7 <- (sum(rest7$home_team_goal,rest7$away_team_goal)/nrow(rest7))
restG8 <- (sum(rest8$home_team_goal,rest8$away_team_goal)/nrow(rest8))

restG <- c(restG1,restG2,restG3,restG4,restG5,restG6,restG7,restG8)
restG

plot(uyear,summerG, type='l', ylab='Average Goals',xlab='Year',col='blue')
lines(uyear, restG, col='red')


avg1<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2008-01-01")& match$date<=as.Date("2008-12-31"))
avg2<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2009-01-01")& match$date<=as.Date("2009-12-31"))
avg3<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2010-01-01")& match$date<=as.Date("2010-12-31"))
avg4<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2011-01-01")& match$date<=as.Date("2011-12-31"))
avg5<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2012-01-01")& match$date<=as.Date("2012-12-31"))
avg6<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2013-01-01")& match$date<=as.Date("2013-12-31"))
avg7<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2014-01-01")& match$date<=as.Date("2014-12-31"))
avg8<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2015-01-01")& match$date<=as.Date("2015-12-31"))
avg9<- mean(goalsE$home_team_goal,goalsE$away_team_goal,goalsG$home_team_goal,goalsG$away_team_goal,
            goalsI$home_team_goal,goalsI$away_team_goal,goalsS$home_team_goal,goalsS$away_team_goal, 
            match$date>=as.Date("2016-01-01")& match$date<=as.Date("2016-12-31"))
plot(avg1,uyear,col='red',type='l')
        lines(avg2,uyear,col='black',type='l')
        lines(avg3,uyear,col='red',type='l')
        lines(av42,uyear,col='black',type='l')
        lines(avg5,uyear,col='red',type='l')
        lines(av6,uyear,col='black',type='l')
        lines(avg7,uyear,col='red',type='l')
        lines(avg8,uyear,col='black',type='l')
        lines(avg9,uyear,col='red',type='l')
        
dat<- qqplot(match$home_team_possession,normal)
