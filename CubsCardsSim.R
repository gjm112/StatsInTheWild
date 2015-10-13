oddsRatio<-rep(NA,9)
for (i in 1:19){print(i)
#Individual game win probabilities for the Cubs for each round (WC, DS, CS, WS)
winProb<-c(.5,i/20,.5,.5)

probVec<-rep(NA,4)
nsim<-1000000

#Real World
#Cubs Play Cardinals in the NLDS
#Pirates game
x<-rnbinom(nsim,1,winProb[1])
probVec[1]<-sum(x<=0)/nsim 

#Cardinals Series
x<-rnbinom(nsim,3,winProb[2])
probVec[2]<-sum(x<=2)/nsim 

#NLCS
x<-rnbinom(nsim,4,winProb[3])
probVec[3]<-sum(x<=3)/nsim 

#World Series
x<-rnbinom(nsim,4,winProb[4])
probVec[4]<-sum(x<=3)/nsim 

real<-prod(probVec)


#Theoretical World
#Cubs Play Cardinal in NLCS
probVec<-rep(NA,4)
nsim<-100000
#Pirates game
x<-rnbinom(nsim,1,winProb[1])
probVec[1]<-sum(x<=0)/nsim 

#Cardinals Series
x<-rnbinom(nsim,3,winProb[3])
probVec[2]<-sum(x<=2)/nsim 

#NLCS
x<-rnbinom(nsim,4,winProb[2])
probVec[3]<-sum(x<=3)/nsim 

#World Series
x<-rnbinom(nsim,4,winProb[4])
probVec[4]<-sum(x<=3)/nsim 

fake<-prod(probVec)

oddsRatio[i]<-real/(1-real)/(fake/(1-fake))
}

plot(c(5:19)/20,oddsRatio[5:19],ylab="Odds Ratio",xlab="Probability Cubs beat Cardinals in a given game",main="Comparing the odds \n of the Cubs winning the World Series playing \n the Cardinals in NLDS and the NLCS",pch=16)
abline(h=1,lty=3)
