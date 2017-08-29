library("rvest")
url <- "https://en.wikipedia.org/wiki/List_of_largest_lakes_of_the_United_States_by_volume"
#http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html
lakes <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()
lakes<-lakes[[1]]

lakes[1,]

start<-unlist(gregexpr("[(]",lakes$`Capacity (normal)`))
end<-unlist(gregexpr("[)]",lakes$`Capacity (normal)`))

for (i in 1:nrow(lakes)){
lakes$volKM[i] <- substring(lakes$`Capacity (normal)`[i],start[i]+1,end[i]-1)
}

lakes$volKM <- gsub("[,]","",lakes$volKM)
lakes$volKM <- gsub("km3","",lakes$volKM)
lakes$volKM <- gsub(" ","",lakes$volKM)
lakes$volKM <- as.numeric(lakes$volKM)

25 trillion gallons
Cubic KM: 94.635294599999995



lakes$volKMradius <- sqrt(lakes$volKM/pi)

lakes<-rbind(lakes,c("1000","Harvey","Houston","NA","NA","NA",94.635294599999995,sqrt(94.635294599999995/pi)))
lakes$volKM <- as.numeric(lakes$volKM)
lakes$volKMradius <- as.numeric(lakes$volKMradius)
lakes<-lakes[order(-lakes$volKM),]

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/lakes1.png",res=300,units="in",h=5,w=5)
library("plotrix")
n<-1000
plot(0,0,col="white",ylim=c(-70,70),xlim=c(-70,70),asp=1,frame.plot=FALSE,xaxt="n",yaxt='n',ylab="",xlab="")

draw.circle(0,0,lakes$volKMradius[1:5],nv=1000,border=rgb(0.5,0.5,0.5,0.5),col=NA,lty=1,lwd=1)
draw.circle(0,0,5.488,nv=1000,border=rgb(0.5,0.5,0.5,0.5),col=rgb(0,0,1,0.5),lty=1,lwd=1)

text(rep(0,5),lakes$volKMradius[1:5],lakes$Name[1:5],cex=0.5)
text(0,0,"Harvey",cex=1)
dev.off()

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/lakes2.png",res=300,units="in",h=5,w=5)
library("plotrix")
n<-1000
plot(0,0,col="white",ylim=c(-7,7),xlim=c(-7,7),asp=1,frame.plot=FALSE,xaxt="n",yaxt='n',ylab="",xlab="")

draw.circle(0,0,lakes$volKMradius[c(6:10,22)],nv=1000,border=rgb(0.5,0.5,0.5,0.5),col=NA,lty=1,lwd=1)
draw.circle(0,0,5.488,nv=1000,border=rgb(0.5,0.5,0.5,0.5),col=rgb(0,0,1,0.5),lty=1,lwd=1)


text(rep(0,5),lakes$volKMradius[c(6:10,22)],lakes$Name[c(6:10,22)],cex=c(0.5,0.5,1,0.5,0.5,0.5))
dev.off()

