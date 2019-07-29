library(dplyr)
library(ggplot2)

lax<-read.csv("http://math.roanoke.edu/childers/STAT304/laxdata.csv",header=T);
View(lax)

#This code gets the game date and year the game was played
date<-strsplit(as.character(lax$dbtime)," ")
date<-do.call(rbind,date)
lax$date<-date[,1]
year<-strsplit(lax$date,'/')
lax$year<-sapply(year, "[", 3)

#This transforms the data so every team is shooting on the same goal
for (i in 1:length(lax$e)){
  if(lax$home[i]=="Home"&lax$team[i]=="ROA"){
    lax$shotposx[i]<-1100-lax$posx[i];
    lax$shotposy[i]<-600-lax$posy[i];
  }
  else if(lax$home[i]=="Away"&lax$team[i]=="ROA"){
    lax$shotposx[i]<-lax$posx[i];
    lax$shotposy[i]<-lax$posy[i];
  }
  
  else if(lax$home[i]=="Away"&lax$team[i]=="AWAY"){
    lax$shotposx[i]<-1100-lax$posx[i];
    lax$shotposy[i]<-600-lax$posy[i];
  }
  else if(lax$home[i]=="Home"&lax$team[i]=="AWAY"){
    lax$shotposx[i]<-lax$posx[i];
    lax$shotposy[i]<-lax$posy[i];
  }
}
for(i in 1:length(lax$e)){
  if(lax$team[i]=="ROA"&lax$shotposx[i]<550){
    lax$shotposx[i]<-1100-lax$shotposx[i];
    lax$shotposy[i]<-600-lax$shotposy[i];
  }
}

for(i in 1:length(lax$e)){
  if(lax$team[i]=="AWAY"&lax$shotposx[i]<550){
    lax$shotposx[i]<-1100-lax$shotposx[i];
    lax$shotposy[i]<-600-lax$shotposy[i];
  }
}

#shot charts##
plot(lax$shotposx[which(lax$team=="ROA"&lax$e=="Goal")],lax$shotposy[which(lax$team=="ROA"&lax$e=="Goal")],main="Roanoke Shot Chart",xlim=c(700,965),ylim=c(175,425),col='red',pch=16)
points(lax$shotposx[which(lax$team=="ROA"&lax$e!="Goal")],lax$shotposy[which(lax$team=="ROA"&lax$e!="Goal")],col='blue')
points(950,300,cex=2.5,pch=16)

plot(lax$shotposx[which(lax$team=="AWAY"&lax$e=="Goal")],lax$shotposy[which(lax$team=="AWAY"&lax$e=="Goal")],main="Away Shot Chart",xlim=c(700,965),ylim=c(175,425),col='red',pch=16)
points(lax$shotposx[which(lax$team=="AWAY"&lax$e!="Goal")],lax$shotposy[which(lax$team=="AWAY"&lax$e!="Goal")],col='blue')
points(950,300,cex=2.5,pch=16)

plot(lax$shotposx[which(lax$e=="Goal")],lax$shotposy[which(lax$e=="Goal")],xlab="",ylab="",main="All Shot Chart",xlim=c(700,965),ylim=c(175,425),col='red',cex=1.5,cex.main=2.5,pch=20,cex.axis=1.5,cex.lab=2)
points(lax$shotposx[which(lax$e!="Goal")],lax$shotposy[which(lax$e!="Goal")],col='blue',cex=1.25)
points(950,300,cex=2.5,pch=16)
legend(x="topleft",pch=c(16,1),pt,cex=1.25,col=c("red","blue"),legend=c("Goal", "Miss"))
####
lax$e
#angle and dist
for (i in 1:length(lax$e)){
  lax$shotdist[i]<-sqrt((lax$shotposx[i]-950)^2+(lax$shotposy[i]-300)^2);
  lax$angle[i]<-atan((lax$shotposy[i]-300)/abs(lax$shotposx[i]-950))/pi*180;
}

#set lax angle and put in categories
A=20
B=-A
lax$angle
for (i in 1:length(lax$e)){
  if(lax$angle[i]<A&B<lax$angle[i]){lax$angleC[i]="S"}
  else{lax$angleC[i]="N"}
}

#check where players are shooting
table(lax$e,lax$angleC)


#Remove Halftime
lax<-lax %>% filter(e!="HALF")

#Make goal an indicator variable
lax$goal<-ifelse(lax$e=="Goal",1,0)

#Model with only distance
M = glm(lax$goal ~ lax$shotdist  , family=binomial)
summary(M)
#Plot of model
ggplot(lax, aes(x=shotdist, y=lax$goal)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#Model with angleC
M1 = glm(lax$goal ~ lax$shotdist +lax$angleC , family=binomial)
summary(M1)
#Plot of model with angle
ggplot(lax, aes(x=shotdist, y=lax$goal, color = angleC)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#picks out the dates
dates<-levels(as.factor(lax$date))
dates

#fitted values
lax$expected.goals<-fitted(M)
lax$expected.goals1<-fitted(M1)

#make a dataframe to look at expected goals and goals.
model.game<-data.frame("expected"=rep(0,length(dates)),"expected1"=rep(0,length(dates)),"game"=rep(0,length(dates)))

for( i in 1:length(dates) ){
  model.game$expected[i]<-lax  %>% filter(date == dates[i]) %>% select(expected.goals)%>%sum()
  model.game$expected1[i]<-lax  %>% filter(date == dates[i]) %>% select(expected.goals1)%>%sum()
  lax.game<-lax  %>% filter(date == dates[i]) 
  model.game$game[i]<-sum(lax.game$goal)
}
#add dates
model.game$dates<-dates
model.game

#We need to remove the games that were mistakenly entered (people playing with the site)
remove.dates<-which(model.game$game<5)
remove.dates
lax<-lax %>% filter(!date %in% dates[remove.dates])

#Redo the analysis
M = glm(lax$goal ~ lax$shotdist  , family=binomial)
summary(M)
lax$goal
ggplot(lax, aes(x=shotdist, y=lax$goal)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


M1 = glm(lax$goal ~ lax$shotdist +lax$angleC , family=binomial)
summary(M1)

ggplot(lax, aes(x=shotdist, y=lax$goal, color = angleC)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


dates<-levels(as.factor(lax$date))
dates


lax$expected.goals<-fitted(M)
lax$expected.goals1<-fitted(M1)

model.game<-data.frame("expected"=rep(0,length(dates)),"expected1"=rep(0,length(dates)),"game"=rep(0,length(dates)))

for( i in 1:length(dates) ){
  model.game$expected[i]<-lax  %>% filter(date == dates[i]) %>% select(expected.goals)%>%sum()
  model.game$expected1[i]<-lax  %>% filter(date == dates[i]) %>% select(expected.goals1)%>%sum()
  lax.game<-lax  %>% filter(date == dates[i]) 
  model.game$game[i]<-sum(lax.game$goal)
}
model.game$dates<-dates
model.game


lax.men <- lax %>% filter(gender == 'Men')
