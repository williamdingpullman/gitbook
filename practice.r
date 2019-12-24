mydata = read.csv(url('https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv'))
mydata$result_new<-ifelse(mydata$HomeScore>mydata$VisitorScore,1,0)
summary(mydata$result_new)
mylogit1 = glm(result_new~1, family=binomial, data=mydata)
summary(mylogit1)


#mydata$sunnyyes<-ifelse(mydata$GameWeather=='Sunny',1,0)
mydata$sunnyyes <- sapply(mydata$GameWeather, function(x) ifelse(length(grep("sunny",x,ignore.case=TRUE))>0, 1, 0))
mylogit1 = glm(result_new~1, family=binomial, data=mydata)
summary(mylogit1)

mylogit12 = lmer(scorediff~1+(1| stadium_new ) ,  data=mydata)
summary(mylogit12)

mydata$scorediff<-mydata$HomeScore-mydata$VisitorScore
lm_results<-lm(mydata$scorediff~mydata$sunnyyes)
summary(lm_results)


mydata$stadium_new<- sapply(mydata$StadiumType, function(x) ifelse(length(grep("indoor",x,ignore.case=TRUE))>0, 1, 0))
library(lme4)
summary(lmer ( scorediff ~ sunnyyes + ( sunnyyes| StadiumType ) , data= mydata ))
