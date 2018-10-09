degrom <- read.csv("/Users/gregorymatthews/Dropbox/StatsInTheWild/deGrom2018.csv")

substring(as.character(degrom$Rslt),2,nchar(as.character(degrom$Rslt)))


library(dplyr)
score <- degrom$Rslt %>% as.character() %>% substring(2,nchar(as.character(degrom$Rslt))) %>% strsplit("-") 
score <- t(apply(do.call(rbind,score),1,as.numeric))

dat <- data.frame(mets = score[1:32,1], opp = score[1:32,2], ER = degrom$ER[1:32], IP = degrom$IP[1:32], result = substring(degrom$Rslt,1,1)[1:32])

table(dat$mets)

#"Bullpen" ERA
9*sum(dat$opp - dat$ER)/sum(dat$IP_Bullpen)
9*32
sum(dat$IP)/(9*32)

sum(dat$opp - dat$ER)
sum(dat$ER)

sum(dat$IP)
sum(dat$IP_Bullpen)

dat2 <- group_by(dat, mets, opp, result) %>% summarise(n = as.factor(n()))
dat2$n<-as.factor(as.numeric(as.character(dat2$n)))

dat3 <- group_by(dat, ER, opp, result) %>% summarise(n = as.factor(n()))
dat3$n<-as.factor(as.numeric(as.character(dat3$n)))


library(ggplot2)
ggplot(aes(x = opp, y = mets, size = n), data = dat2) + geom_point() +
  geom_abline(slope = 1, intercept = 0) + scale_y_continuous(breaks=seq(0, 14, 2))

library(ggplot2)
ggplot(aes(x = ER, y = opp, size = n), data = dat3) + geom_point() +
  geom_abline(slope = 1, intercept = 0) + scale_y_continuous(breaks=seq(0, 14, 2))


#https://www.baseball-reference.com/leagues/MLB/pitch.shtml
#Average ERA was 4.15 in 2018

dat$IP<-as.character(dat$IP)
dat$IP[nchar(as.character(dat$IP))==1] <- paste0(as.character(dat$IP)[nchar(as.character(dat$IP))==1],".0")
temp <- apply(do.call(rbind,(strsplit(as.character(dat$IP),"[.]"))),2,as.numeric)
dat$IP <- temp[,1] + temp[,2]/3


dat$IP_Bullpen <- 9 - dat$IP
dat$Exp_Runs <- dat$ER + dat$IP_Bullpen * 4.15/9

plot(dat$Exp_Runs,dat$opp)
abline(a = 0,b = 1)

dat$Exp_Runs <- dat$ER + dat$IP_Bullpen * 4.15/9

table(dat$mets > dat$Exp_Runs)
table(dat$result)


lambda <- 4.15/27 #ERA
lambda <- 4.45/27 #Runs per game
simruns <- function(n){
  out <- sum(rpois(n*3,lambda))
  return(out)
}
simruns <- Vectorize(simruns)

set.seed(1234)
W <- list()
for (i in 1:10000){print(i)
  dat$simruns <- dat$ER + simruns(dat$IP_Bullpen)
  dat$simW <- (dat$mets > dat$simruns) + 0
  dat$simW[dat$mets == dat$simruns] <- rbinom(sum(dat$mets == dat$simruns),1,0.5)
  W[[i]] <- sum(dat$simW)  
}

wins <- data.frame(W = unlist(W))
ggplot(aes(x = W), data = wins) + geom_histogram() + scale_x_continuous(breaks = c(10:31))


dat$Exp_Result <- "L"
dat$Exp_Result[dat$mets > dat$Exp_Runs] <- "W"

table(dat$result,dat$Exp_Result)



dat[dat$result!=dat$Exp_Result,]



