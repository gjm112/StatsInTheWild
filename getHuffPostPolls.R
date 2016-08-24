library(RCurl)
library(rjson)
datList<-list()

x<-fromJSON(getURL("http://elections.huffingtonpost.com/pollster/api/polls.json"))
for (i in 1:10){
  datList[[i]]<-x[[i]]
}

for (n in 2:700){print(n)
x<-fromJSON(getURL(paste0("http://elections.huffingtonpost.com/pollster/api/polls.json?page=",n)))
for (i in 1:10){
datList[[10*(n-1)+i]]<-x[[i]]
}
}

#Remove the poll with id = 24610
for (i in 1:3000){
if (datList[[i]]$id %in% c(24610,24170,23677,22781,22776)){datList[[i]]<-NULL}
}


getCode<-function(x){
datList[[x]]$questions[[1]]$code
}

codes<-lapply(c(1:2600),getCode)

usIndex<-grep("16-US-Pres-GE TrumpvClinton",unlist(codes))
#usIndex<-setdiff(usIndex, c(608,619,630,645,661,666,670))

pollList<-list()
for (i in usIndex){print(i)
  n<-datList[[i]]$questions[[1]]$subpopulations[[1]]$observations
  if (is.null(n)){n<-NA}
  
  jjj<-length(datList[[i]]$questions[[1]]$subpopulations[[1]]$responses)
  tempList<-list()
  for (j in 1:jjj){
    tempList[[datList[[i]]$questions[[1]]$subpopulations[[1]]$responses[[j]]$choice]]<-datList[[i]]$questions[[1]]$subpopulations[[1]]$responses[[j]]$value
  }
  
  if ("Clinton"%in%names(tempList) & "Trump"%in%names(tempList)){
    
  pollList[[i]]<-
    data.frame(pollster = datList[[i]]$pollster,
             start = datList[[i]]$start_date,
             end = datList[[i]]$end_date,
             method = datList[[i]]$method,
             part = datList[[i]]$partisan,
             who = datList[[i]]$questions[[1]]$subpopulations[[1]]$name,
             n = n,
             clinton = unlist(tempList)[grep("Clinton",names(unlist(tempList)))],
             trump = unlist(tempList)[grep("Trump",names(unlist(tempList)))])
  
  }
  
}

polls<-do.call(rbind,pollList)
polls<-subset(polls,who%in%c("Registered Voters","Likely Voters"))
polls$end<-as.Date(polls$end)

plot(polls$start,polls$clinton,col="blue")
points(polls$start,polls$trump,col="red")

dates<-sort(unique(as.Date(polls$end)))[-1]

pList<-list()
for (i in 1:length(dates)){
temp<-polls[polls$end<=dates[i] & polls$end>=dates[i]-14,]
pList[[i]]<-data.frame(date=dates[i],clinton= mean(temp$clinton,weight=temp$n),trump = mean(temp$trump,weight=temp$n))
}

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/TrumpVsClintonNationalPolls.png",h=10,w=20,units="in",res=300)
pDat<-do.call(rbind,pList)
par(mfrow=c(2,1))

plot(pDat$date,pDat$clinton,type="l",ylim=c(33,55),col="white",lwd=3,xlab="Date",ylab="%",main="Trump vs Clinton Polls\n Weighted Moving Average\n Source: Huffington Post",sub="@statsinthewild -  August 24, 2016")
points(pDat$date,pDat$trump,type="l",ylim=c(33,51),col="white",lwd=3)
abline(h=c(35,40,45,50),lty=3,col=rgb(0.5,0.5,0.5,0.5))
abline(v=as.Date(c("2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01","2016-08-01","2016-09-01")),lty=3,col=rgb(0.5,0.5,0.5,0.5))


for (lw in 1:21){
pList<-list()
for (i in 1:length(dates)){
  temp<-polls[polls$end<=dates[i] & polls$end>=dates[i]-lw,]
  p<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  moeC<-1.96*sqrt(p*(1-p)/sum(temp$n))
  p<-sum(temp$trump/100*temp$n)/sum(temp$n)
  moeT<-1.96*sqrt(p*(1-p)/sum(temp$n))
  pList[[i]]<-data.frame(date=dates[i],clinton= mean(temp$clinton,weight=temp$n),trump = mean(temp$trump,weight=temp$n),moeC = moeC,moeT = moeT)
}
pDat<-do.call(rbind,pList)
if (lw==7){pDat7<-pDat}
if (lw==14){pDat14<-pDat}
if (lw==21){pDat21<-pDat}
points(pDat$date,pDat$clinton,type="l",ylim=c(0,100),col=rgb(0,0,lw/21,lw/30),lwd=lw/5)
points(pDat$date,pDat$trump,type="l",ylim=c(0,100),col=rgb(lw/21,0,0,lw/30),lwd=lw/5)
}

polygon(c(pDat7$date,pDat7$date[nrow(pDat7):1]),c(pDat7$clinton-100*pDat7$moeC,pDat7$clinton[nrow(pDat7):1]+100*pDat7$moeC[nrow(pDat7):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat7$date,pDat7$date[nrow(pDat7):1]),c(pDat7$trump-100*pDat7$moeT,pDat7$trump[nrow(pDat7):1]+100*pDat7$moeT[nrow(pDat7):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

polygon(c(pDat14$date,pDat14$date[nrow(pDat14):1]),c(pDat14$clinton-100*pDat14$moeC,pDat14$clinton[nrow(pDat14):1]+100*pDat14$moeC[nrow(pDat14):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat14$date,pDat14$date[nrow(pDat14):1]),c(pDat14$trump-100*pDat14$moeT,pDat14$trump[nrow(pDat14):1]+100*pDat14$moeT[nrow(pDat14):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

polygon(c(pDat21$date,pDat21$date[nrow(pDat21):1]),c(pDat21$clinton-100*pDat21$moeC,pDat21$clinton[nrow(pDat21):1]+100*pDat21$moeC[nrow(pDat21):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat21$date,pDat21$date[nrow(pDat21):1]),c(pDat21$trump-100*pDat21$moeT,pDat21$trump[nrow(pDat21):1]+100*pDat21$moeT[nrow(pDat21):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

polygon(as.numeric(as.Date(c("2016-05-01","2016-09-01","2016-09-01","2016-05-01"))),c(36,36,51,51),bor=rgb(0,0,0,0.5),lwd=3)

abline(v=as.Date(c("2016-07-18")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-07-18")),34.5,"RNC",srt=90)
abline(v=as.Date(c("2016-07-28")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-07-28")),34.5,"DNC",srt=90)
abline(v=as.Date(c("2016-05-26")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-05-26")),50,"Trump\n 1237 Delegates",srt=90)
abline(v=as.Date(c("2016-06-06")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-06-06")),50,"Clinton 2383\n Pledged Delegates",srt=90)
abline(v=as.Date(c("2016-03-01")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-03-01")),35,"Super\n Tuesday",srt=90)
abline(v=as.Date(c("2015-08-06")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-08-06")),40,"1st Rep Debate",srt=90)
abline(v=as.Date(c("2015-09-16")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-09-16")),40,"2nd Rep Debate",srt=90)
abline(v=as.Date(c("2015-10-28")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-10-28")),40,"3rd Rep Debate",srt=90)
abline(v=as.Date(c("2015-11-10")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-11-10")),40,"4th Rep Debate",srt=90)
abline(v=as.Date(c("2015-12-15")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-12-15")),40,"5th Rep Debate",srt=90)
abline(v=as.Date(c("2016-01-114")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-01-14")),40,"6th Rep Debate",srt=90)
abline(v=as.Date(c("2016-01-28")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-01-28")),40,"7th Rep Debate",srt=90)
abline(v=as.Date(c("2016-02-06")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-02-06")),40,"8th Rep Debate",srt=90)

abline(v=as.Date(c("2015-10-13")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-10-13")),40,"1st Dem Debate",srt=90)
abline(v=as.Date(c("2015-11-14")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-11-14")),40,"2nd Dem Debate",srt=90)
abline(v=as.Date(c("2015-12-19")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2015-12-19")),40,"3rd Dem Debate",srt=90)
abline(v=as.Date(c("2016-01-17")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-01-17")),40,"4th Dem Debate",srt=90)
abline(v=as.Date(c("2016-02-04")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-02-04")),40,"5th Dem Debate",srt=90)

pDat <- pDat[pDat>"2016-05-01",]
plot(pDat$date,pDat$clinton,type="l",ylim=c(36,51),col="white",lwd=3,xlab="Date",ylab="%",main="Trump vs Clinton Polls\n Weighted Moving Average\n Source: Huffington Post",sub="@statsinthewild -  August 24, 2016")
points(pDat$date,pDat$trump,type="l",ylim=c(33,51),col="white",lwd=3)
abline(h=c(35,40,45,50),lty=3,col=rgb(0.5,0.5,0.5,0.5))
abline(v=as.Date(c("2015-09-01","2015-10-01","2015-11-01","2015-12-01","2016-01-01","2016-02-01","2016-03-01","2016-04-01","2016-05-01","2016-06-01","2016-07-01","2016-08-01","2016-09-01")),lty=3,col=rgb(0.5,0.5,0.5,0.5))


for (lw in 1:21){
  pList<-list()
  for (i in 1:length(dates)){
    temp<-polls[polls$end<=dates[i] & polls$end>=dates[i]-lw,]
    p<-sum(temp$clinton/100*temp$n)/sum(temp$n)
    moeC<-1.96*sqrt(p*(1-p)/sum(temp$n))
    p<-sum(temp$trump/100*temp$n)/sum(temp$n)
    moeT<-1.96*sqrt(p*(1-p)/sum(temp$n))
    pList[[i]]<-data.frame(date=dates[i],clinton= mean(temp$clinton,weight=temp$n),trump = mean(temp$trump,weight=temp$n),moeC = moeC,moeT = moeT)
  }
  pDat<-do.call(rbind,pList)
  if (lw==7){pDat7<-pDat}
  if (lw==14){pDat14<-pDat}
  if (lw==21){pDat21<-pDat}
  points(pDat$date,pDat$clinton,type="l",ylim=c(0,100),col=rgb(0,0,lw/21,lw/30),lwd=lw/5)
  points(pDat$date,pDat$trump,type="l",ylim=c(0,100),col=rgb(lw/21,0,0,lw/30),lwd=lw/5)
}

polygon(c(pDat7$date,pDat7$date[nrow(pDat7):1]),c(pDat7$clinton-100*pDat7$moeC,pDat7$clinton[nrow(pDat7):1]+100*pDat7$moeC[nrow(pDat7):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat7$date,pDat7$date[nrow(pDat7):1]),c(pDat7$trump-100*pDat7$moeT,pDat7$trump[nrow(pDat7):1]+100*pDat7$moeT[nrow(pDat7):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

polygon(c(pDat14$date,pDat14$date[nrow(pDat14):1]),c(pDat14$clinton-100*pDat14$moeC,pDat14$clinton[nrow(pDat14):1]+100*pDat14$moeC[nrow(pDat14):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat14$date,pDat14$date[nrow(pDat14):1]),c(pDat14$trump-100*pDat14$moeT,pDat14$trump[nrow(pDat14):1]+100*pDat14$moeT[nrow(pDat14):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

polygon(c(pDat21$date,pDat21$date[nrow(pDat21):1]),c(pDat21$clinton-100*pDat21$moeC,pDat21$clinton[nrow(pDat21):1]+100*pDat21$moeC[nrow(pDat21):1]),col=rgb(0,0,1,0.25),border=rgb(0,0,1,0.05))
polygon(c(pDat21$date,pDat21$date[nrow(pDat21):1]),c(pDat21$trump-100*pDat21$moeT,pDat21$trump[nrow(pDat21):1]+100*pDat21$moeT[nrow(pDat21):1]),col=rgb(1,0,0,0.25),border=rgb(1,0,0,0.05))

abline(v=as.Date(c("2016-07-18")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-07-18")),38,"RNC",srt=90)
abline(v=as.Date(c("2016-07-28")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-07-28")),38,"DNC",srt=90)
abline(v=as.Date(c("2016-05-26")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-05-26")),38,"Trump 1237\n Delegates",srt=90)
abline(v=as.Date(c("2016-06-06")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-06-06")),45,"Clinton 2383 \n Pledged Delegates",srt=90)
abline(v=as.Date(c("2016-03-01")),lty=1,col=rgb(0.5,0.5,0.5,0.5))
text(as.Date(c("2016-03-01")),38,"Super Tuesday",srt=90)

dev.off()



#State polls
stateIndex<-grep("16-[A-z]{2}-Pres-GE TrumpvClinton",unlist(codes))
stateIndex<-setdiff(stateIndex,usIndex)


pollList<-list()
for (i in stateIndex){print(i)
  n<-datList[[i]]$questions[[1]]$subpopulations[[1]]$observations
  if (is.null(n)){n<-NA}
  
  state<-substring(datList[[i]]$questions[[1]]$code[[1]],4,5)
  
  jjj<-length(datList[[i]]$questions[[1]]$subpopulations[[1]]$responses)
  tempList<-list()
  for (j in 1:jjj){
    tempList[[datList[[i]]$questions[[1]]$subpopulations[[1]]$responses[[j]]$choice]]<-datList[[i]]$questions[[1]]$subpopulations[[1]]$responses[[j]]$value
  }
  
  if ("Clinton"%in%names(tempList) & "Trump"%in%names(tempList)){
    
    pollList[[i]]<-
      data.frame(pollster = datList[[i]]$pollster,
                 state = state,
                 start = datList[[i]]$start_date,
                 end = datList[[i]]$end_date,
                 method = datList[[i]]$method,
                 part = datList[[i]]$partisan,
                 who = datList[[i]]$questions[[1]]$subpopulations[[1]]$name,
                 n = n,
                 clinton = unlist(tempList)[grep("Clinton",names(unlist(tempList)))],
                 trump = unlist(tempList)[grep("Trump",names(unlist(tempList)))])
    
  }
  
}


polls<-do.call(rbind,pollList)
polls<-subset(polls,who%in%c("Registered Voters","Likely Voters"))
polls<-polls[polls$state!="US",]
polls$end<-as.Date(polls$end)

pList<-list()
states<-sort(names(table(polls$state)))
states <- setdiff(states,"US")

library(plotrix)
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/TrumpVsClintonStatePolls20160824.png",h=10,w=10,units="in",res=100)
plot(c(0.2,0.6),c(0.2,.6),col="white",xlab="Clinton %",ylab="Trump %",main="State polls\n August 24, 2016",sub="@statsinthewild",asp=1)


arrows(0.3,0.3,0.4,0.2,length=0.4,angle=-45,col="blue",lwd=3)
arrows(0.3,0.3,0.2,0.4,length=0.4,angle=-45,col="red",lwd=3)
arrows(0.3,0.3,0.25,0.25,length=0.4,angle=-45,col="gray",lwd=3)
arrows(0.3,0.3,0.35,0.35,length=0.4,angle=-45,col="gray",lwd=3)
text(0.35,0.25,"Clinton",pos=3,srt=-45)
text(0.25,0.35,"Trump",pos=3,srt=-45)
text(0.32,0.32,"Major\n Candidates",pos=3,srt=45)
text(0.27,0.27,"3rd Party",pos=3,srt=45)

polygon(c(0,1,1),c(0,0,1),col=rgb(0,0,1,0.2))
polygon(c(0,0,1),c(0,1,1),col=rgb(1,0,0,0.2))
abline(a=0,b=1)
abline(a=1,b=-1)
abline(a=0.75,b=-1)
abline(a=0.5,b=-1)

for (st in states){
  temp<-polls[polls$state==st,]
  
  pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
  pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
  sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))
  
  draw.circle(pClinton,pTrump,c(1,2)*sdT,border=rgb(0,0,0,0.5),col=rgb(2*(pTrump-0.2),0,2*(pClinton-0.2),0.1))
  
  
  text(pClinton,pTrump,st,col=rgb(0.9,0.9,0.9,0.9),cex=10*abs(pClinton-pTrump)+0.5)
}

for (st in states){
temp<-polls[polls$state==st,]
pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))

ccc<-rnorm(5000,pClinton,sdC)
ttt<-rnorm(5000,pTrump,sdT)
points(ccc,ttt,pch=16,cex=0.5,col=rgb(2*(pTrump-0.2),0,2*(pClinton-0.2),0.1))
}
abline(a=0,b=1)
abline(a=1,b=-1)
abline(a=0.75,b=-1)
abline(a=0.5,b=-1)

for (st in states){
  temp<-polls[polls$state==st,]
  pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
  pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
  sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))
  
  
  
  
  text(pClinton,pTrump,st,col=rgb(0.9,0.9,0.9,0.9),cex=10*abs(pClinton-pTrump)+0.5)
}

dev.off()


#Now try to do it over time
polls<-do.call(rbind,pollList)
dats<-c(as.Date("2016-06-01"):as.Date("2016-08-21"))

for (tim in dats){
polls<-do.call(rbind,pollList)
polls<-subset(polls,who%in%c("Registered Voters","Likely Voters"))
polls<-polls[polls$state!="US",]
polls$end<-as.Date(polls$end)

polls<-polls[polls$end<=tim,]

pList<-list()
states<-sort(names(table(as.character(polls$state))))
states <- setdiff(states,"US")

library(plotrix)
png(paste0("/Users/gregorymatthews/Dropbox/StatsInTheWild/TrumpVsClintonStatePolls20160824_",tim,".png"),h=10,w=10,units="in",res=100)
plot(c(0.2,0.6),c(0.2,.6),col="white",xlab="Clinton %",ylab="Trump %",main=paste("State polls\n",as.Date(tim,origin="1970-01-01"),sep=""),sub="@statsinthewild",asp=1)


arrows(0.3,0.3,0.4,0.2,length=0.4,angle=-45,col="blue",lwd=3)
arrows(0.3,0.3,0.2,0.4,length=0.4,angle=-45,col="red",lwd=3)
arrows(0.3,0.3,0.25,0.25,length=0.4,angle=-45,col="gray",lwd=3)
arrows(0.3,0.3,0.35,0.35,length=0.4,angle=-45,col="gray",lwd=3)
text(0.35,0.25,"Clinton",pos=3,srt=-45)
text(0.25,0.35,"Trump",pos=3,srt=-45)
text(0.32,0.32,"Major\n Candidates",pos=3,srt=45)
text(0.27,0.27,"3rd Party",pos=3,srt=45)

polygon(c(0,1,1),c(0,0,1),col=rgb(0,0,1,0.2))
polygon(c(0,0,1),c(0,1,1),col=rgb(1,0,0,0.2))
abline(a=0,b=1)
abline(a=1,b=-1)
abline(a=0.75,b=-1)
abline(a=0.5,b=-1)

for (st in states){
  temp<-polls[polls$state==st,]
  
  pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
  pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
  sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))
  
  draw.circle(pClinton,pTrump,c(1,2)*sdT,border=rgb(0,0,0,0.5),col=rgb(2*(pTrump-0.2),0,2*(pClinton-0.2),0.1))
  
  
  text(pClinton,pTrump,st,col=rgb(0.9,0.9,0.9,0.9),cex=10*abs(pClinton-pTrump)+0.5)
}

for (st in states){
  temp<-polls[polls$state==st,]
  pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
  pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
  sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))
  
  ccc<-rnorm(5000,pClinton,sdC)
  ttt<-rnorm(5000,pTrump,sdT)
  points(ccc,ttt,pch=16,cex=0.5,col=rgb(2*(pTrump-0.2),0,2*(pClinton-0.2),0.1))
}
abline(a=0,b=1)
abline(a=1,b=-1)
abline(a=0.75,b=-1)
abline(a=0.5,b=-1)

for (st in states){
  temp<-polls[polls$state==st,]
  pClinton<-sum(temp$clinton/100*temp$n)/sum(temp$n)
  sdC<-sqrt(pClinton*(1-pClinton)/sum(temp$n))
  pTrump<-sum(temp$trump/100*temp$n)/sum(temp$n)
  sdT<-sqrt(pTrump*(1-pTrump)/sum(temp$n))
  
  
  
  
  text(pClinton,pTrump,st,col=rgb(0.9,0.9,0.9,0.9),cex=10*abs(pClinton-pTrump)+0.5)
}

dev.off()

}
