library(RCurl)
date<-"20160909"
repPrice<-demPrice<-list()

#Vector of all states
stateAbbr<-c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA",
"ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
"SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

#Get the most recent closing prices for all the available state markets
for (st in stateAbbr){print(st)
  x <- getURL(paste0("https://www.predictit.org/api/marketdata/ticker/DEM.",st,".USPREZ16"))
  if (x == "null"){
    demPrice[[st]]<-NA
    repPrice[[st]]<-NA
  }
    if (x != "null"){
      temp<-strsplit(x,",")[[1]]
      test<-read.csv(textConnection(temp),sep=":",header=FALSE)
      names(test)<-c("var","value")
      repID<-which(as.character(test[,2])==paste0("GOP.",st,".USPREZ16"))+2
      demID<-which(as.character(test[,2])==paste0("DEM.",st,".USPREZ16"))+2
      
      demPrice[[st]]<-as.numeric(gsub("}","",as.character(test[demID,"value"])))
      repPrice[[st]]<-as.numeric(gsub("}]","",as.character(test[repID,"value"])))
      
      #if (as.character(test[14,"value"])=="Republican"){
      #  demPrice[[st]]<-as.numeric(gsub("}]","",as.character(test[37,"value"])))
      #  repPrice[[st]]<-as.numeric(gsub("}","",as.character(test[22,"value"])))
      #}
      }
}

unlist(demPrice)

#When there is no marker available set a state to 0 or 1 based on last election.  
for (st in c("AK"   ,   "WY")){
  demPrice[[st]]<-0;repPrice[[st]]<-1
}

for (st in c( "VT")){
  demPrice[[st]]<-1;repPrice[[st]]<-0
}


#Create a data frame with the prices for each state and normalize so that the probs sum to 1.  
dat<-data.frame(demPrice=unlist(demPrice),repPrice=unlist(repPrice))
dat<-as.data.frame(t(apply(dat,1,function(x){x/sum(x)})))

#add electoral vote values to the states
dat$EV <- c(3,9,11,6,55,9,7,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)

#Simulate the election nsim times 
nsim<-100000
EVsimList<-list()
for (i in 1:nsim){print(i)
sim<-rbinom(50,1,dat$demPrice)
EVsimList[[i]]<-sim%*%dat$EV + 3 #Plus 3 for Washington DC
}

#How many red bars and blue bars? 
redN<-sum(as.numeric(names(table(unlist(EVsimList))))<270)
blueN<-sum(as.numeric(names(table(unlist(EVsimList))))>270)


#Make the plot 
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/PredictItPresident_20160909_DC.png",h=10,w=10,res=300,units="in")
plot(table(unlist(EVsimList)),xlim=c(150,440),ylim=c(0,0.015*100000),col=c(rep("red",redN),"black",rep("blue",blueN)),yaxt='n',ylab="Probability",xlab="Electoral Votes",main="Electoral Vote Projections Based \n on Predictit.org State Markets \n September 9, 2016",sub="@statsinthewild")
#abline(v=270,lwd=1,col=rgb(0,0,0,0.1))
polygon(c(270,500,500,270),c(0,0,4000,4000),col=rgb(0,0,1,0.5))
polygon(c(0,270,270,0),c(0,0,4000,4000),col=rgb(1,0,0,0.5))
axis(2,seq(0,2000,250),seq(0,2000,250)/nsim)
text(388,1250,paste0(100*sum(unlist(EVsimList)>=270)/nsim,"%"),cex=3)
text(188,1250,paste0(100*sum(unlist(EVsimList)<=268)/nsim,"%"),cex=3)
text(270,1250,paste0(100*sum(unlist(EVsimList)==269)/nsim,"%"),cex=3)
dev.off()

save.image(paste0("/Users/gregorymatthews/Dropbox/StatsInTheWild/PredictIt_",date,".RData"))

#Dem win prob
sum(unlist(EVsimList)>=270)/nsim
#time Prob
sum(unlist(EVsimList)==269)/nsim
#rep win prob
sum(unlist(EVsimList)<=268)/nsim


#Over time
date<-as.Date(c("2016-07-30","2016-08-01","2016-08-02","2016-08-08","2016-08-09","2016-08-10","2016-08-11","2016-08-12","2016-08-13","2016-08-14","2016-08-15","2016-08-16","2016-08-17","2016-08-18","2016-08-19","2016-08-20","2016-08-21","2016-08-22","2016-08-23","2016-08-24","2016-08-25","2016-08-26","2016-08-27","2016-08-28","2016-08-29","2016-08-30","2016-08-31","2016-09-01","2016-09-02","2016-09-03","2016-09-04","2016-09-06","2016-09-07","2016-09-08","2016-09-09"))
clinton<-c(.83155,.83680,0.87960,0.91747,0.95575,0.95986,0.95307 ,0.95335,0.96132,.95451,0.9562,0.96045,0.96349,0.96811,0.95772,0.94783,0.94794,0.93742,0.94502,0.94093,0.9342,0.95421,0.9448,0.94461,0.93852,0.93504,0.92904,0.88799,0.90909,0.90323,0.90996,0.89614,0.84698,0.88154,0.88045)
trump<-c(.16209,.15642,0.11489,0.07887,0.04211,0.03813,0.0486,0.0435,0.03681,0.04327,0.04153,0.03754,0.0346,0.03008,0.0416,0.04958,0.04929,0.05936,0.0525,0.05614,0.06256,0.04343,0.05232,0.0525,0.05842,0.06182,0.06762,0.10744,0.08675,0.09266,0.08588,0.09899,0.1466,0.11262,0.11455)
tie<-c(.00636,0.00678,0.00551,0.00366,0.00214,0.00201,0.00207,0.0023,0.00187,0.00222,0.00227,0.00201,0.00191,0.00181,0.00212,0.00259,0.00277,0.00322,0.00246,0.00293,0.00324,0.00236,0.00288,0.00289,0.00306,0.00314,0.00334,0.00457,0.00416,0.00411,0.00416,0.00487,0.00642,0.00584,0.005)



dev.off()#Most likely electoral vote outcomes
sort(table(unlist(EVsimList)))

probs<-table(cut(abs(unlist(EVsimList)-270)+270,c(0,279,299,319,339,359,379,399,1000)))/100000
    



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

rho<-cor((demVote[,1:50]/100))
library(MASS)
Sigma<-rho
for (i in 1:50){
  for (j in 1:50){
    vari<-(dat$demPrice[i]*(1-dat$demPrice[i]))
    varj<-(dat$demPrice[j]*(1-dat$demPrice[j]))
    Sigma[i,j]<-rho[i,j]*sqrt(vari*varj)
  }
}

#add electoral vote values to the states
dat$EV <- c(3,9,11,6,55,9,7,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)

#Simulate the election nsim times 
nsim<-100000
EVsimList<-list()
for (i in 1:nsim){print(i)
  sim<-(mvrnorm(1,dat$demPrice,Sigma)>0.5)+0
  EVsimList[[i]]<-sim%*%c(dat$EV)+3  #Plus 3 for Washington DC
}

#How many red bars and blue bars? 
redN<-sum(as.numeric(names(table(unlist(EVsimList))))<270)
blueN<-sum(as.numeric(names(table(unlist(EVsimList))))>270)


#Make the plot 
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/PredictItPresident_20160909_DC_corr.png",h=10,w=10,res=300,units="in")
plot(table(unlist(EVsimList)),xlim=c(0,538),col=c(rep("red",redN),"black",rep("blue",blueN)),yaxt='n',ylab="Probability",xlab="Electoral Votes",main="Electoral Vote Projections Based \n on Predictit.org State Markets (with correlation) \n September 9, 2016",sub="@statsinthewild")
#abline(v=270,lwd=1,col=rgb(0,0,0,0.1))
polygon(c(270,538,538,270),c(0,0,4000,4000),col=rgb(0,0,1,0.5))
polygon(c(0,270,270,0),c(0,0,4000,4000),col=rgb(1,0,0,0.5))
axis(2,seq(0,5000,250),seq(0,5000,250)/nsim)
text(538-100,3000,paste0(100*sum(unlist(EVsimList)>=270)/nsim,"%"),cex=3)
text(100,3000,paste0(100*sum(unlist(EVsimList)<=268)/nsim,"%"),cex=3)
text(270,3000,paste0(100*sum(unlist(EVsimList)==269)/nsim,"%"),cex=3)
dev.off()

#Dem win prob
sum(unlist(EVsimList)>=270)/nsim
#time Prob
sum(unlist(EVsimList)==269)/nsim
#rep win prob
sum(unlist(EVsimList)<=268)/nsim

#Over time
date2<-as.Date(c("2016-08-25","2016-08-26","2016-08-27","2016-08-29","2016-08-30","2016-08-31","2016-09-04","2016-09-07","2016-09-08"))
clinton2<-c(.75324,0.75795,0.75537,0.74627,0.74367,0.73591,0.72349,0.675,0.69795)
trump2<-c(.24017,.23613,0.2387,0.24894,0.25092,0.25946,0.27016,0.31749,0.29378)
tie2<-c(0.00659,0.00592,0.00593,0.00479,0.00541,0.00463,0.00635,0.0068,0.00827)

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/PredictItPresident_time.png",h=10,w=10,res=300,units="in")
plot(date,clinton,ylim=c(0,1),col="white",ylab="Probability",main="Probability of Winning the Electoral College",sub="Based on state electoral college markets on predictit.org",xlab="Date")
points(date,clinton,col="blue",pch=16,cex=3)
points(date,clinton,col="blue",type='l',lwd=5)
points(date,trump,col="red",pch=16,cex=3)
points(date,trump,col="red",type='l',lwd=5)
points(date,tie,col="black",pch=16)
points(date,tie,col="black",type='l')

points(date2,clinton2,col="lightblue",pch=16,cex=3)
points(date2,clinton2,col="lightblue",type='l',lwd=5)
points(date2,trump2,col="pink",pch=16,cex=3)
points(date2,trump2,col="pink",type='l',lwd=5)
points(date2,tie2,col="gray",pch=16)
points(date2,tie2,col="gray",type='l')

abline(h=c(0:10)/5,lty=3,col=rgb(0.5,0.5,0.5,0.5))
abline(v=c(17012)+c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),lty=5,col=rgb(0.5,0.5,0.5,0.5))
legend(17012,0.7,c("Clinton (Ind)","Trump (Ind)","Tie (Ind"),pch=16,col=c("blue","red","black"))
legend(17012,0.3,c("Clinton (Corr)","Trump (Corr)","Tie (Corr)"),pch=16,col=c("lightblue","pink","gray"))
dev.off()


sort(table(unlist(EVsimList)))

probsCorr<-table(cut(abs(unlist(EVsimList)-270)+270,c(0,279,299,319,339,359,379,399,1000)))/100000

1-(probsCorr+probs)/2
(probsCorr+probs)/2

################################################################################
#Conditional win probabilities given a dem win in a state.  
################################################################################
stList<-list()
#Given a win in a state for Democrat
for (st in c("OH","PA","FL","TX","GA","TN","IN","MO","AZ","NC","SC","KY","AL","LA","CA","UT","MI","NY","VA","NV","WI","IA","CO","MN","NJ","NH","IL","WA","MA","MD")){print(st)
tempPrice <- dat$demPrice
tempPrice[rownames(dat)==st]<-1
nsim<-10000
EVsimList<-list()
for (i in 1:nsim){
  sim<-rbinom(50,1,tempPrice)
  EVsimList[[i]]<-sim%*%dat$EV
}
stList[[st]]<-c(sum(unlist(EVsimList)>=270)/nsim,sum(unlist(EVsimList)==269)/nsim,sum(unlist(EVsimList)<=268)/nsim)
}

demState<-do.call(rbind,stList)
demState<-demState[order(-demState[,1]),]

#Given a win in a state for Republican
stListRep<-list()
#Given a win in a state
for (st in c("OH","PA","FL","TX","GA","TN","IN","MO","AZ","NC","SC","KY","AL","LA","CA","UT","MI","NY","VA","NV","WI","IA","CO","MN","NJ","NH","IL","WA","MA","MD")){print(st)
  tempPrice <- dat$repPrice
  tempPrice[rownames(dat)==st]<-1
  nsim<-50000
  EVsimList<-list()
  for (i in 1:nsim){
    sim<-rbinom(50,1,tempPrice)
    EVsimList[[i]]<-sim%*%dat$EV
  }
  stListRep[[st]]<-c(sum(unlist(EVsimList)>=270)/nsim,sum(unlist(EVsimList)==269)/nsim,sum(unlist(EVsimList)<=268)/nsim)
}

repState<-do.call(rbind,stListRep)
repState<-repState[order(-repState[,1]),]

#output for blog post
library(R2HTML)
print(xtable(repState), type = "html")
print(xtable(demState), type = "html")

#Given a win in a state for Republican
stListRep<-list()
#Given a win in a state
  tempPrice <- dat$repPrice
  tempPrice[rownames(dat)=="FL"]<-1
  #tempPrice[rownames(dat)=="OH"]<-1
  tempPrice[rownames(dat)=="PA"]<-1
  nsim<-10000
  EVsimList<-list()
  for (i in 1:nsim){print(i)
    sim<-rbinom(50,1,tempPrice)
    EVsimList[[i]]<-sim%*%dat$EV
  }
  c(sum(unlist(EVsimList)>=270)/nsim,sum(unlist(EVsimList)==269)/nsim,sum(unlist(EVsimList)<=268)/nsim)


repState<-do.call(rbind,stListRep)
repState<-repState[order(-repState[,1]),]

#Given a win in a state
tempPrice <- dat$repPrice
tempPrice[rownames(dat)=="FL"]<-1
tempPrice[rownames(dat)=="OH"]<-1
tempPrice[rownames(dat)=="PA"]<-1
tempPrice[rownames(dat)=="NC"]<-1
nsim<-10000
EVsimList<-list()
for (i in 1:nsim){print(i)
  sim<-rbinom(50,1,tempPrice)
  EVsimList[[i]]<-sim%*%dat$EV
}
c(sum(unlist(EVsimList)>=270)/nsim,sum(unlist(EVsimList)==269)/nsim,sum(unlist(EVsimList)<=268)/nsim)





# #By state
# simList<-list()
# nsim<-100000
# i<-1
# EVsimList<-list()
# while (i <= 100000){print(i)
#   sim<-rbinom(50,1,dat$demPrice)
#   if(sum(sim%*%dat$EV)>=270){
#     simList[[i]]<-sim
#     i <- i + 1
#   }
# 
# }
# 
# greg<-do.call(rbind,simList)
# res<-data.frame(state=rownames(dat),probWhenWin=cbind(apply(greg,2,mean)),prob=dat$demPrice)
# res$increase<-res$probWhenWin/res$prob
# res[order(-res$increase),]
# 



princomp(cor((demVote)))
dim(cbind(a$loadings[,1],a$loadings[,2]))

test<-data.frame(loadings=a$loadings[,1],state=state)
test[order(test$loadings),]

test<-data.frame(loadings=a$loadings[,2],state=state)
test[order(test$loadings),]


col<-c(rgb(1-dat$demPrice,0,dat$demPrice),rgb(0,0,1))
plot(a$loadings[,1],a$loadings[,2],col="white",xlim=c(-.25,.25))
text(a$loadings[,1],a$loadings[,2],state,cex=sqrt(dat$EV/20),col=col)

col<-c(rgb(1-dat$demPrice,0,dat$demPrice),rgb(0,0,1))
plot(a$loadings[,1],a$loadings[,3],col="white",xlim=c(-.25,.25))
text(a$loadings[,1],a$loadings[,3],state,cex=sqrt(dat$EV/20),col=col)

col<-c(rgb(1-dat$demPrice,0,dat$demPrice),rgb(0,0,1))
ggg<-cmdscale(dist(t(demVote)))
plot(ggg,col="white")
text(ggg[,1],ggg[,2],c(state.abb,"DC"),cex=sqrt(dat$EV/20),col=col)

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/MDSVotes.png",units="in",res=300,w=10,h=10)
col<-c(rgb(1-dat$demPrice,0,dat$demPrice),rgb(0,0,1))
ggg<-cmdscale(dist(t(demVote)[-51,]))
plot(ggg,col="white",xlim=c(-60,50),xlab="Dim1",ylab="Dim2",main="Multidimensional Scaling of State\n Voting Patterns 1984-2012",sub="@statsinthewild")
text(ggg[,1],ggg[,2],state.abb,cex=sqrt(dat$EV/5),col=col)
dev.off()

