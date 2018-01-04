# Must install the devtools package using the below commented out code
# install.packages('devtools')
library(devtools)

devtools::install_github(repo = "maksimhorowitz/nflscrapR")
#> Skipping install for github remote, the SHA1 (05815ef8) has not changed since last install.
#>   Use `force = TRUE` to force installation

# Load the package
library(nflscrapR)

#Get the data 
qbs<-list()
for (i in 2017:2000){print(i)
out<-season_player_game(i)
qbs[[i]]<-subset(out,pass.att>0)
}

#save(qbs,file="~/qbs.RData")
qb<-do.call(rbind,qbs)
qb$initials<-gsub("[.]","",substring(qb$name,1,3))

#Some ad hoc corrections
qb$initials[qb$initials=="Sh."]<-"SH"
qb$initials[qb$initials=="Sh"]<-"SH"
qb$initials[qb$initials=="Ale"]<-"AS"
qb$name[qb$name=="Alex Smith"]<-"A.Smith"
qb$Team[qb$Team=="SD"]<-"LAC"
qb$Team[qb$Team=="STL"]<-"LA"
qb$Team[qb$Team=="JAC"]<-"JAX"

#pull out the player with the most pass attempts.  
teams<-unique(qb$Team)
starters<-list()
nnn<-list()
for (ttt in teams){
sub<-subset(qb,pass.att>0)
subTemp<-subset(sub,Team==ttt)
subTemp<-subTemp[order(subTemp$game.id,-subTemp$pass.att),]
starters[[ttt]]<-subTemp[!duplicated(subTemp$game.id),c("Season","game.id","date","Team","playerID","name","initials","pass.att")]
nnn[[ttt]]<-max(table(starters[[ttt]]$name))
}

#list the teams in order from smallest max number of starts to largest max number of starts 
teams<-names(sort(unlist(nnn)))

png("~/Dropbox/qb.png",res=300,h=10,w=40,units="in")
plot(0,0,frame.plot=FALSE,ylab="",xlab="",yaxt='n',xaxt='n',col="white",ylim=c(0,32),xlim=c(0,144))
for (j in 1:32){
  id<-names(-sort(-table(starters[[teams[j]]]$name)))
  num<-(-sort(-table(starters[[teams[j]]]$name)))
  col<-rainbow(length(id))
  col2<-"black"
for (i in 1:144){
polygon(c(0,1,1,0)+i-1,c(0,0,1,1)+j-1,col=col[which(id==starters[[teams[j]]]$name[i])],lwd=0.0000001)
text(i-0.5,j-0.5,starters[[teams[j]]]$initials[i],col=col2[which(id==starters[[teams[j]]]$name[i])],cex=0.75)  
}
  
}
for (j in 1:32){
text(-2,j-0.5,teams[j])
text(146,j-0.5,teams[j])
}

axis(1,at=c(1:9)*16-8,as.character(c(2009:2017)),cex=5)

abline(v=c(0:10)*16,lwd=5)
abline(h=c(0:10)*4,lwd=2)
dev.off()
