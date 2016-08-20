library(RCurl)
library(rjson)
datList<-list()

x<-fromJSON(getURL("http://elections.huffingtonpost.com/pollster/api/polls.json"))
for (i in 1:10){
  datList[[i]]<-x[[i]]
}

for (n in 1001:10000){print(n)
x<-fromJSON(getURL(paste0("http://elections.huffingtonpost.com/pollster/api/polls.json?page=",n)))
for (i in 1:10){
datList[[10*(n-1)+i]]<-x[[i]]
}
}


getCode<-function(x){
datList[[x]]$questions[[1]]$code
}

codes<-lapply(c(1:10000),getCode)

usIndex<-grep("16-US-Pres-GE TrumpvClinton",unlist(codes))
usIndex<-setdiff(usIndex, c(608,619,630,645,661,666,670))

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
plot(pDat$date,pDat$clinton,type="l",ylim=c(33,51),col="white",lwd=3,xlab="Date",ylab="%",main="Trump vs Clinton Polls\n Weighted Moving Average\n Source: Huffington Post",sub="@statsinthewild")
points(pDat$date,pDat$trump,type="l",ylim=c(33,51),col="white",lwd=3)
abline(h=c(35,40,45,50),lty=3,col=rgb(0.5,0.5,0.5,0.5))
abline(v=as.Date(c("2016-06-01","2016-07-01","2016-08-01")),lty=3,col=rgb(0.5,0.5,0.5,0.5))

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

polygon(c(pDat7$date,pDat7$date[53:1]),c(pDat7$clinton-100*pDat7$moeC,pDat7$clinton[53:1]+100*pDat7$moeC[53:1]),col=rgb(0,0,1,0.5),border=rgb(0,0,1,0.1))
polygon(c(pDat7$date,pDat7$date[53:1]),c(pDat7$trump-100*pDat7$moeT,pDat7$trump[53:1]+100*pDat7$moeT[53:1]),col=rgb(1,0,0,0.5),border=rgb(1,0,0,0.1))

polygon(c(pDat14$date,pDat14$date[53:1]),c(pDat14$clinton-100*pDat14$moeC,pDat14$clinton[53:1]+100*pDat14$moeC[53:1]),col=rgb(0,0,1,0.5),border=rgb(0,0,1,0.1))
polygon(c(pDat14$date,pDat14$date[53:1]),c(pDat14$trump-100*pDat14$moeT,pDat14$trump[53:1]+100*pDat14$moeT[53:1]),col=rgb(1,0,0,0.5),border=rgb(1,0,0,0.1))

polygon(c(pDat21$date,pDat21$date[53:1]),c(pDat21$clinton-100*pDat21$moeC,pDat21$clinton[53:1]+100*pDat21$moeC[53:1]),col=rgb(0,0,1,0.5),border=rgb(0,0,1,0.1))
polygon(c(pDat21$date,pDat21$date[53:1]),c(pDat21$trump-100*pDat21$moeT,pDat21$trump[53:1]+100*pDat21$moeT[53:1]),col=rgb(1,0,0,0.5),border=rgb(1,0,0,0.1))

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

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/TrumpVsClintonStatePolls.png",h=10,w=10,units="in",res=300)
plot(c(0.2,0.6),c(0.2,.55),col="white",xlab="Clinton %",ylab="Trump %",main="State polls",sub="@statsinthewild")
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


