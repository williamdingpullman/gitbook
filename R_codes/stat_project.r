#https://fivethirtyeight.com/contributors/josh-hermsmeyer/
# https://github.com/ryurko/nflscrapR-data/blob/master/legacy_data/README.md

#mydata1 = read.csv('plays.txt')
#unique(mydata1$gameId)

#unique(mydata1$PassLength)
#table(mydata1$PassLength)
#table(mydata1$PassResult)
#table(mydata1$numberOfPassRushers)


##mydata3 = read.csv(url('https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/legacy_data/season_play_by_play/pbp_2017.csv'))
##write.csv(mydata3,'2017playbyplay.csv')

mydata3<-read.csv('2017playbyplay.csv')
nrow(mydata3)
table(mydata3$Passer)
table(mydata3$PlayType)

#mydata5<-mydata3[!duplicated(mydata3[,c('GameID','Passer')]),]
#unique(mydata3$GameID)
mydata6<-subset(mydata3,down==1)


mydata7<-subset(mydata6,PlayType=='Pass'|PlayType=='Run')
#table(mydata7$PlayType)
#table(droplevels(mydata7$PlayType))

mydata7$PlayType<-droplevels(mydata7$PlayType)
table(mydata7$PlayType)

#http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
mydata8<-mydata7[,c('Passer','PlayType','GameID','posteam','DefensiveTeam','Yards.Gained','FirstDown','TimeSecs')]
#results<-crosstab(mydata8, row.vars = "GameID", col.vars = "PlayType", type = "r")
#p1<-results$crosstab
#hist(p1[,1],20)



library(plyr)
count_vector<-count(mydata8, "GameID")

l_new<-length(count_vector$freq)
time<-c()
for(i in 1:l_new)
{time<-append(time,rep(1:count_vector$freq[i]))}
nrow(time)
mydata8$time<-time
mydata8$play_new<-ifelse(mydata8$PlayType=='Pass',1,0)

n_counting<-0  # help counting the number of pairs

## The following code collects all the rows of each pair. However, it is difficult to analyze data
# in such a format.

#empty_df = mydata8[FALSE,]
#for (i in 1:l_new) # level of different game
#{
#   for(j in 1:((count_vector$freq[i])-1)) # within the same game
#   {
#      if(i==1)
#      {row_id<-j}
#      else {row_id<-sum(count_vector$freq[1:(i-1)])+j}
#
#      #print(row_id)
#      if(as.character(mydata8[row_id,]$posteam)!=as.character(mydata8[row_id+1,]$posteam))
#      {
#        print("not same team")
#        if (nrow(empty_df)==0)
#           {empty_df<-mydata8[row_id:(row_id+1),]}
#        else
#           {
#             if(row.names(mydata8[row_id,])!=row.names(tail(empty_df,1)))
#               {empty_df<-rbind(empty_df,mydata8[row_id,])}
#             empty_df<-rbind(empty_df,mydata8[row_id+1,])
#           }
#       n_counting<-n_counting+1
#      }
#   }
#}


# The following code only collects the second row of the pair, but adds data of
### PT_L: type of play in the last first down from the other team
### TG_L: Yards.Gained in the last play
### FirstDown: did they get first down or not. Note that, if yes, it means it was a fumble.

PT_L="Pass"
TG_L=0
FD_L=0

pari_data= mydata8[1,]
pari_data<-cbind(pari_data,PT_L,TG_L,FD_L)
pari_data<-pari_data[FALSE,]

for (i in 1:l_new) # level of different game
{
  for(j in 1:((count_vector$freq[i])-1)) # within the same game
  {

    if(i==1)
    {row_id<-j}
    else {row_id<-sum(count_vector$freq[1:(i-1)])+j}

    print(row_id)
    if(as.character(mydata8[row_id,]$posteam)!=as.character(mydata8[row_id+1,]$posteam))
    {
      print("not same team")
      PT_L<-as.character(mydata8[row_id,]$PlayType)
      TG_L<-mydata8[row_id,]$Yards.Gained
      FD_L<-mydata8[row_id,]$FirstDown

      new_row<-cbind(mydata8[(row_id+1),],PT_L,TG_L,FD_L)
      pari_data<-rbind(pari_data,new_row)
     }

      n_counting<-n_counting+1
  }
}

pari_data$same<-ifelse(pari_data$PlayType==pari_data$PT_L,1,0)

#write.csv(pari_data,'pari_data.csv')

write.table(pari_data, file = "pari_data.csv",row.names=FALSE,na = "", sep=",")

pari_data2<-read.csv('pari_data.csv')




mylogit1 = glm(same~1, family=binomial, data=pari_data)
summary(mylogit1)

mylogit2 = glm(same~TG_L+FD_L, family=binomial, data=pari_data)
summary(mylogit2)

mylogit2 = glmer(same~play_new+TG_L+FD_L+(1|GameID), family= binomial("logit"), data=pari_data)
summary(mylogit2)






#Bill_1<- bild(play_new ~ TG_L+FD_L, data = mydata8, id="GameID",start = NULL, dependence = "MC1R")
#summary(Bill_1)

#locust2 <- bild(as.factor(PlayType) ~ time + I(time^2), data = mydata8,id="GameID",start = NULL, dependence = "MC2")
