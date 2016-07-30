library(RCurl)

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
      if (as.character(test[14,"value"])=="Democratic"){
      demPrice[[st]]<-as.numeric(gsub("}","",as.character(test[22,"value"])))
      repPrice[[st]]<-as.numeric(gsub("}]","",as.character(test[37,"value"])))
      }
      if (as.character(test[14,"value"])=="Republican"){
        demPrice[[st]]<-as.numeric(gsub("}]","",as.character(test[37,"value"])))
        repPrice[[st]]<-as.numeric(gsub("}","",as.character(test[22,"value"])))
      }
      }
}



#When there is no marker available set a state to 0 or 1 based on last election.  
for (st in c("AK" ,"AR", "ID", "KS" , "MS" ,"MT" ,"NE" , "ND", "OK" , "SD",  "WV", "WY")){
  demPrice[[st]]<-0;repPrice[[st]]<-1
}

for (st in c( "CT", "DE" ,"HI" ,"ME", "NM", "OR" ,"RI", "VT")){
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
EVsimList[[i]]<-sim%*%dat$EV
}

#How many red bars and blue bars? 
redN<-sum(as.numeric(names(table(unlist(EVsimList))))<270)
blueN<-sum(as.numeric(names(table(unlist(EVsimList))))>270)


#Make the plot 
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/PredictItPresident_20160730.png",h=10,w=10,res=300,units="in")
plot(table(unlist(EVsimList)),xlim=c(150,425),col=c(rep("red",redN),"black",rep("blue",blueN)),yaxt='n',ylab="Probability",xlab="Electoral Votes",main="Electoral Vote Projections Based \n on Predictit.org State Markets \n July 30, 2016",sub="@statsinthewild")
#abline(v=270,lwd=1,col=rgb(0,0,0,0.1))
polygon(c(270,500,500,270),c(0,0,4000,4000),col=rgb(0,0,1,0.5))
polygon(c(0,270,270,0),c(0,0,4000,4000),col=rgb(1,0,0,0.5))
axis(2,seq(0,2000,250),seq(0,2000,250)/nsim)
text(388,1250,paste0(100*sum(unlist(EVsimList)>=270)/nsim,"%"),cex=3)
text(188,1250,paste0(100*sum(unlist(EVsimList)<=268)/nsim,"%"),cex=3)
text(270,1250,paste0(100*sum(unlist(EVsimList)==269)/nsim,"%"),cex=3)
dev.off()

#Dem win prob
sum(unlist(EVsimList)>=270)/nsim
#time Prob
sum(unlist(EVsimList)==269)/nsim
#rep win prob
sum(unlist(EVsimList)<=268)/nsim

#Most likely electoral vote outcomes
sort(table(unlist(EVsimList)))


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
  nsim<-10000
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
