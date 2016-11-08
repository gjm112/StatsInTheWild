stateProbs <- read.csv("/Users/gregorymatthews/Dropbox/StatsInTheWild/UpshotStateElectionProbs_Nov5.csv")
stateProbs <- stateProbs[1:56,1:8]

stateProbs[,3:8]<-apply(stateProbs[,3:8],2,as.character)

cleanit<-function(x){
  x[x==">99% Dem."]<-99.9
  x[x==">99% Rep."]<-100-99.9
  x<-gsub("%","",x)
  x<-gsub("Dem.","",x)
  x<-gsub(" ","",x)
  
  repInd <- grep("Rep",x)
  x[grep("Rep",x)] <- gsub("Rep.","",x[grep("Rep",x)])
  x <- as.numeric(x)
  x[repInd]<-100-x[repInd]
  x<-x/100
  return(x)
}

stateProbs[,3:8]<-apply(stateProbs[,3:8],2,cleanit)

#Dim the dem win the state
stateProbs$Results <- c(rep(1,15),c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0),rep(0,14))
stateProbs$Results[stateProbs$State == "Florida"]<-1
stateProbs$Results[stateProbs$State == "Ohio"]<-1
stateProbs$Results[stateProbs$State == "North Carolina"]<-1
stateProbs$Results[stateProbs$State == "Pennsylvania"]<-1
stateProbs$Results[stateProbs$State == "Nevada"]<-1

x<-stateProbs[,3]

logloss<-function(x){
  ll<-(-(stateProbs$Results*log(x) + (1-stateProbs$Results)*log(1-x)))
  ll<-ll[-grep("CD",stateProbs$State)]
  out <- mean(ll)
  return(out)
}

brier<-function(x){
  ll<- (stateProbs$Results-x)^2
  ll<-ll[-grep("CD",stateProbs$State)]
  out <- mean(ll)
  return(out)
}

apply(stateProbs[3:8],2,logloss)
apply(stateProbs[3:8],2,brier)
library(ROCR)

col<-c("red","orange","gold","darkgreen","blue","purple")
pred<-prediction(stateProbs[-grep("CD",stateProbs$State),3],stateProbs$Results[-grep("CD",stateProbs$State)])
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,type="l",col=col[1])

for (i in 4:8){
  pred<-prediction(stateProbs[-grep("CD",stateProbs$State),i],stateProbs$Results[-grep("CD",stateProbs$State)])
  perf<-performance(pred,measure="tpr",x.measure="fpr")
  points(perf@x.values[[1]],perf@y.values[[1]],col=col[i],type="l")
}

auc<-rep(NA,6)
for (q in 3:8){
  pred<-prediction(stateProbs[-grep("CD",stateProbs$State),q],stateProbs$Results[-grep("CD",stateProbs$State)])
  perf<-performance(pred,measure="auc")
  auc[q-2]<-perf@y.values[[1]]
}

################################
#How many do we expect to get wrong?
################################
stateProbs[-grep("CD",stateProbs$State),3]
datList<-list()
for (source in names(stateProbs)[3:8]){
  set.seed(1234)
  conList<-list()
  nsim<-5000
  for (i in 1:nsim){print(i)
    sim<-rbinom(51,1,stateProbs[-grep("CD",stateProbs$State),source])
    pred<-(stateProbs[-grep("CD",stateProbs$State),source]>.50)+0
    conList[[i]]<-c(table(sim,pred)[1,2],table(sim,pred)[2,1])
  }
  #predicted R goes D, predicted D goes R
  check<-as.data.frame(do.call(rbind,conList))
  check$summ<-apply(check,1,sum)
  datList[[source]]<-check
}
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/ExpectedMisses.png",res=300,units="in",w=10,h=10)
par(mfrow=c(2,3))
for (j in 1:6){
  barplot(table(factor(datList[[j]]$summ,levels=c(0:12))),xlim=c(0,15),main=names(datList)[j],xlab="Incorrectly Predicted States",ylim=c(0,2000))
}
dev.off()
#predicted R goes D, predicted D goes R
for (j in 1:6){
  print(apply(datList[[j]],2,mean))
}

for (j in 1:6){
  print(apply(datList[[j]],2,median))
}



################################
#Correlated elections
################################
electHist<-read.csv("/Users/gregorymatthews/Dropbox/StatsInTheWild/US Presidential Results & PVIs by State 1828-2012 - 2-Party US Pres Results & PVIs.csv")
state<-as.character(electHist[-c(1),1])
#demVote<-electHist[-1,seq(2,30,4)]
demVote<-electHist[-1,seq(2,45,4)]
demVote<-apply(demVote,2,as.character)
demVote<-gsub("%","",demVote)
demVote<-t(apply(demVote,2,as.numeric))[,-51]
demVote

repVote<-electHist[-1,seq(3,45,4)]
repVote<-apply(repVote,2,as.character)
repVote<-gsub("%","",repVote)
repVote<-t(apply(repVote,2,as.numeric))[,-51]
repVote

#dat$demPrice[rownames(dat)%in%c("HI","MD","VT","CA","MA","NY","RI","DE","IL","NJ","CT","OR","VA","NM","MN")]<-1
#dat$demPrice[rownames(dat)%in%c("NE","WY","WV","OK","ID","KY","AL","ND","SD","AR","NE","KS","TN","MT","LA","UT","AK","MS","TX","SC")]<-0

colnames(demVote)<-row.names(dat)
rho<-cor((demVote[,1:50]/100))
library(corrplot)
colnames(rho)<-row.names(dat)
rownames(rho)<-row.names(dat)
corrplot(rho,method="ellipse",order="AOE")

rhoSwing<-cor((demVote[,row.names(dat)%in%c("OH","PA","FL","NC","NH","IA","AZ","MI","PA","CO","NV")]/100))
corrplot(rhoSwing,method="ellipse",order="AOE")

stateProbs[[""]]<-0.5
stateProbs[["Naive"]]<-c(rep(1,15),rep(0.5,20),rep(0,21))
stateProbs$State[1:15]
stateProbs$State[-c(1:35)]
predList<-list()
for (source in names(stateProbs)[3:8]){
  library(MASS)
  Sigma<-rho
  for (i in 1:50){
    for (j in 1:50){
      vari<-(stateProbs[[source]][-c(1,grep("CD",stateProbs$State))][i]*(1-stateProbs[[source]][-c(1,grep("CD",stateProbs$State))][i]))
      varj<-(stateProbs[[source]][-c(1,grep("CD",stateProbs$State))][j]*(1-stateProbs[[source]][-c(1,grep("CD",stateProbs$State))][j]))
      Sigma[i,j]<-rho[i,j]*sqrt(vari*varj)
    }
  }
  
  #add electoral vote values to the states
  #dat$EV <- c(3,9,11,6,55,9,7,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
  
  #Simulate the election nsim times 
  nsim<-10000
  set.seed(1234)
  EVsimList<-list()
  for (i in 1:nsim){print(i)
    sim<-(mvrnorm(1,stateProbs[[source]][-c(1,grep("CD",stateProbs$State))],Sigma)>0.5)+0
    EVsimList[[i]]<-sim%*%c(stateProbs$E.V.[-c(1,grep("CD",stateProbs$State))])+3  #Plus 3 for Washington DC
    if (source != "DK"){EVsimList[[i]]<-EVsimList[[i]]+sum(rbinom(5,1,stateProbs[[source]][grep("CD",stateProbs$State)]))}
    if (source != "DK"){EVsimList[[i]]<-EVsimList[[i]]+sum(rbinom(5,1,stateProbs[['NYT']][grep("CD",stateProbs$State)]))}
  }
  predList[[source]]<-unlist(EVsimList)
  
}

col<-c("red","orange","gold","darkgreen","blue","purple","black","gray")
a<-density(predList[[1]])
plot(a$x,a$y,type="l",col=col[1],xlim=c(0,538),ylim=c(0,0.03),lwd=3,xlab="Electoral Votes",ylab="Density")
for (i in c(2:6,8)){
  a<-density(predList[[i]])
  points(a$x,a$y,type="l",col=col[i],lwd=3)
}
legend(0,0.02,names(stateProbs)[3:8],lty=1,col=col,lwd=3)
abline(v=270)
cbind(names(stateProbs)[3:8],
      c(mean(predList[[1]]>270),
        mean(predList[[2]]>270),
        mean(predList[[3]]>270),
        mean(predList[[4]]>270),
        mean(predList[[5]]>270),
        mean(predList[[6]]>270)))

hist(predList[[2]])

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/ComparingPredictionsElection2016.png",res=300,units="in",w=15,h=10)
box<-do.call(cbind,predList)
boxplot(box,col=col,ylim=c(0,538),border=rgb(0.5,0.5,0.5,0.5),ylab="Electoral Votes")
legend(4,200,names(stateProbs)[3:8],lty=1,col=col,lwd=3)
abline(h=270,lwd=3,col=rgb(0.5,0.5,0.5,0.5))
abline(h=seq(0,550,50),lty=3,col=rgb(0.5,0.5,0.5,0.5))
text(c(1:6),apply(box,2,median),apply(box,2,median),cex=2)
text(c(1:6),apply(box,2,quantile,0.75),apply(box,2,quantile,0.75))
text(c(1:6),apply(box,2,quantile,0.25),apply(box,2,quantile,0.25))
text(c(1:6),apply(box,2,min),apply(box,2,min))
text(c(1:6),apply(box,2,max),apply(box,2,max))
dev.off()

#How many red bars and blue bars? 
redN<-sum(as.numeric(names(table(unlist(EVsimList))))<270)
blueN<-sum(as.numeric(names(table(unlist(EVsimList))))>270)

