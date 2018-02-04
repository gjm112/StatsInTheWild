nfl<-read.csv("https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv")

# Color housekeeping
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(16)

p <- ggplot(nfl, aes(x=factor(score1%%10),y=factor(score2%%10)))
h3 <- p + stat_bin2d()  + scale_fill_gradientn(colors=r)
h3

nfl$end1<-nfl$score1%%10
nfl$end2<-nfl$score2%%10

for (i in 1:nrow(nfl)){
nfl$endMax[i]<-max(nfl$end1[i],nfl$end2[i])
nfl$endMin[i]<-min(nfl$end1[i],nfl$end2[i])
}

p <- ggplot(nfl, aes(y=factor(endMin),x=factor(endMax)))
h3 <- p + stat_bin2d()  + scale_fill_gradientn(colors=r)
h3

dat<-rbind(data.frame(season=nfl$season,score=nfl$score1),
data.frame(season=nfl$season,score=nfl$score2))

dat$end<-dat$score%%10


library(dplyr)
dat2 <- dat %>% group_by(season,end) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))

ggplot(data=dat2,aes(x=season,y=freq,colour=factor(end))) + geom_point() +geom_smooth() + facet_wrap(~factor(end))
ggplot(data=dat2,aes(x=season,y=freq,colour=factor(end))) + geom_point() +geom_smooth() 


dat3<-subset(dat2,season>=1960)
plot(0,0,xlim=c(1960,2018),ylim=c(-1,9.5),ylab="Last Digit",xlab="Year")
square<-function(i,j,col){
polygon(c(0,1,1,0)+i,c(0,0,1,1)+j-0.5,col=col,border = col)
}

dat3$grp<-as.numeric(cut(dat3$freq,32))
for (i in 1:nrow(dat3)){
square(dat3$season[i],dat3$end[i],col=rf(32)[dat3$grp[i]])
}

axis(2,at=c(0:9),labels=as.character(c(0:9)))

