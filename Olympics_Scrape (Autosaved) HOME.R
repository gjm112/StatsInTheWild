library(XML)
library(scrapeR)
#url<-"http://www.sports-reference.com/olympics/athletes/aa/"
#readHTMLTable(url)

#results.list<-list()
letter.pairs <- c( outer( letters[1:26], letters[1:26], FUN=paste, sep="") )

#letters<-'aa'
url<-paste("http://www.sports-reference.com/olympics/athletes/",letter.pairs,"/",sep="")

#getting all the names

webpage<-getURL(url)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
ind.vec<-rep(0,length(webpage))
for (i in 1:length(ind.vec)){
ind.vec[i]<-gregexpr(paste("/olympics/athletes/",letters,sep=""),webpage[i])[[1]][1]
}
text.raw<-webpage[ind.vec>0]

#correct for non-english letters
if (length(text.raw)>0){

names<-vector()
for (i in 1:length(text.raw)){
start<-gregexpr(">",text.raw[i])[[1]][1]
end<-gregexpr("<",text.raw[i])[[1]][2]
names[i]<-gsub(" ","-",tolower(substring(text.raw[i],start+1,end-1)))
names[i]<-gsub("ä","a",names[i])
names[i]<-gsub("ö","o",names[i])
names[i]<-gsub("é","e",names[i])
names[i]<-gsub("ø","o",names[i])
names[i]<-gsub("æ","ae",names[i])
names[i]<-gsub("é","e",names[i])
names[i]<-gsub("ú","u",names[i])
names[i]<-gsub("í","i",names[i])
names[i]<-gsub("ë","e",names[i])
names[i]<-gsub("á","a",names[i])
names[i]<-gsub("ə","",names[i])
names[i]<-gsub("ú","u",names[i])
names[i]<-gsub("à","a",names[i])
names[i]<-gsub("[.,']","",names[i])
names[i]<-gsub("ó","o",names[i])
names[i]<-gsub("ü","u",names[i])
names[i]<-gsub("å","a",names[i])
names[i]<-gsub("ā","a",names[i])
names[i]<-gsub("č","c",names[i])
names[i]<-gsub("ć","c",names[i])
names[i]<-gsub("ł","l",names[i])
names[i]<-gsub("ı","i",names[i])
names[i]<-gsub("ğ","g",names[i])
names[i]<-gsub("ñ","n",names[i])
names[i]<-gsub("ř","r",names[i])
names[i]<-gsub("š","s",names[i])
names[i]<-gsub("ş","s",names[i])
names[i]<-gsub("è","e",names[i])
names[i]<-gsub("ï","i",names[i])
names[i]<-gsub("ū","u",names[i])
names[i]<-gsub("â","a",names[i])
names[i]<-gsub("þ","th",names[i])
names[i]<-gsub("ð","d",names[i])
names[i]<-gsub("ă","a",names[i])
names[i]<-gsub("ã","a",names[i])
names[i]<-gsub("ϊ","i",names[i])
names[i]<-gsub("ç","c",names[i])

names[i]<-gsub("ń","n",names[i])

names[i]<-gsub("ś","s",names[i])
names[i]<-gsub("ē","e",names[i])
names[i]<-gsub("ī","i",names[i])
names[i]<-gsub("ô","o",names[i])
if (letters%in%c("as","au")){names[i]<-gsub("ß","ss",names[i])}
names[i]<-gsub("ß","ss",names[i])
names[i]<-gsub("ţ","t",names[i])
names[i]<-gsub("ļ","l",names[i])
names[i]<-gsub("ģ","g",names[i])
names[i]<-gsub("đ","d",names[i])

names[i]<-gsub("ő","o",names[i])
names[i]<-gsub("ņ","n",names[i])
names[i]<-gsub("ż","z",names[i])
names[i]<-gsub("ž","z",names[i])
names[i]<-gsub("--","-",names[i])
names[i]<-gsub("ė","e",names[i])


names[i]<-gsub("ľ","l",names[i])
names[i]<-gsub("ĕ","e",names[i])

names[i]<-gsub("ò","o",names[i])








names[i]<-gsub("î","i",names[i])
names[i]<-gsub("ň","n",names[i])
names[i]<-gsub("ď","d",names[i])
names[i]<-gsub("ą","a",names[i])
names[i]<-gsub("ě","e",names[i])
names[i]<-gsub("ů","u",names[i])


names[i]<-gsub("ý","y",names[i])
names[i]<-gsub("ů","u",names[i])
names[i]<-gsub("ů","u",names[i])
names[i]<-gsub("ů","u",names[i])


}

	print(letters)
	tab<-data.frame(table(names))
names(tab)<-c("Name","Cnt")
tab$Name<-as.character(tab$Name)
for (i in 1:length(tab$Name)){
N<-tab$Cnt[i]
print(N)
#get ages
		for (q in 1:N){print(tab$Name[i])
url2<-paste("http://www.sports-reference.com/olympics/athletes/",letters,"/",tab$Name[i],"-",q,".html",sep="")
#if (names[i]=="batchuluuny-bat–orgil") {url2<-"http://www.sports-reference.com/olympics/athletes/ba/batchuluuny-bat-orgil-1.html"}
temp<-try(readHTMLTable(url2)$results)
if (class(temp)!="try-error"){results.list[[letters]][[paste(tab$Name[i],q,sep="")]]<-temp}
}

			}

		}

	}
	save.image("Olympics_Scrape.RData")
}
#save.image("Olympics_Scrape.RData")

#load("Olympics_Scrape.RData")

crush<-list()
for (i.one in 1:26){
		for (i.two in 1:26){
			letters<-paste(alphabet[i.one],alphabet[i.two],sep="")
			print(letters)
			if(!is.null(results.list[[letters]])){	crush[[letters]]<-do.call(rbind,results.list[[letters]])	}
			}
		}
		
dat<-do.call(rbind,crush)
save.image("Olympics_Scrape.RData")
dim(dat)
dat$Name<-rownames(dat)
dat$Year<-substring(dat$Games,1,4)
dat$Sex<-"B"

dat$Sex[as.character(substring(dat$Event,1,3))=="Wom"]<-"F"
dat$Sex[as.character(substring(dat$Event,1,3))=="Men"]<-"M"

#for (i in 1:length(dat$Name)){print(i)
#dat$Name[i]<-gsub("[.][0-9]","",dat$Name[i])
#dat$Name[i]<-substring(dat$Name[i],4,length(dat$Name))
#if (gregexpr("Wom",dat$Event[i])[[1]][1]>0) dat$Sex[i]<-"F"
#if (gregexpr("Men",dat$Event[i])[[1]][1]>0) dat$Sex[i]<-"M"
#}


dat.uniq<-dat[!duplicated(paste(dat$Name,dat$Year,sep="")),]

#Summer games 2000-2008
dat.summer<-dat.uniq[unlist(gregexpr("Summer",dat.uniq$Games))>0,]
dat.summer<-dat.summer[dat.summer$Year%in%c("2008","2004","2000"),]

dat.summer$Age<-as.numeric(as.character(dat.summer$Age))


#Remove some sports
dat.summer<-dat.summer[dat.summer$Sport!="Motorboating",]
dat.summer<-dat.summer[dat.summer$Sport!="Art Competitions",]
dat.summer<-dat.summer[dat.summer$Sport!="Croquet",]
dat.summer<-dat.summer[dat.summer$Sport!="Cricket",]
dat.summer<-dat.summer[dat.summer$Sport!="Ice Hockey",]

dat.summer$SportSex<-paste(as.character(dat.summer$Sport),as.character(dat.summer$Sex),sep=", ") 

labs<-sort(unique(dat.summer$SportSex))
col<-rep("green",length(labs))
for (i in 1:length(labs)){
if (gregexpr(", F",labs[i])[[1]][1]>0){col[i]<-"pink"}
if (gregexpr(", M",labs[i])[[1]][1]>0){col[i]<-"blue"}
}

dat.summer$Age<-dat.summer$Age+rnorm(length(dat.summer$Age),0,.2)

par(oma=c(6,1,1,1))
boxplot(dat.summer$Age~dat.summer$SportSex,col=col,las=2,main="Age distribution of Olympic Athletes by Sport and Gender: 2000-2008 \n Sorted by Sport \n Female = Pink, Male = Blue, Both = Green",ylab="Age")
abline(h=seq(10,100,5),lty=3,col="gray75")

#ordering
ab<-as.matrix((by(dat.summer$Age[!is.na(dat.summer$Age)],dat.summer$SportSex[!is.na(dat.summer$Age)],median)))
ord<-data.frame(rownames(ab),ab)
names(ord)<-c("SportSex","Age.median")
ord<-ord[!is.na(ord$Age.median),]
ord<-ord[order(ord$Age),]

dat.summer<-merge(dat.summer,ord,by.x="SportSex",by.y="SportSex",all.x=TRUE)
dat.summer$SportSex<-factor(dat.summer$SportSex,c(as.character(ord$SportSex)))

labs<-ord$SportSex
col<-rep("green",length(labs))
for (i in 1:length(labs)){
if (gregexpr(", F",labs[i])[[1]][1]>0){col[i]<-"pink"}
if (gregexpr(", M",labs[i])[[1]][1]>0){col[i]<-"blue"}
}

#dat.summer$Age<-dat.summer$Age+rnorm(length(dat.summer$Age),0,.05)


png("oly.png",w=3000,h=2000)
boxplot(dat.summer$Age~dat.summer$SportSex,col=col,las=2,main="Age distribution of Olympic Athletes by Sport and Gender: 2000-2008 \n Sorted by Median Age \n Female = Pink, Male = Blue, Both = Green",ylab="Age")
abline(h=seq(10,100,5),lty=3,col="gray75")


#by gender 2000-2008
par(oma=c(2,1,1,1))
boxplot(dat.summer$Age~dat.summer$Sex,col=c("green","pink","blue"),las=2,main="Age distribution of Olympic Athletes by Year: 2000-2008" ,ylab="Age",xlab="Gender")
abline(h=seq(10,100,5),lty=3,col="gray75")


#things<-gsub('-',' ',gsub('[0-9.]',"",substring(dat.summer$Name[dat.summer$Age>50],4,length(dat.summer$Name[dat.summer$Age>50]))))
#text(c(1:length(ord$SportSex))[ord$SportSex%in%dat.summer$SportSex[dat.summer$Age>50]],dat.summer$Age[dat.summer$Age>50],things)


#Summer games all time
dat.uniq<-dat[!duplicated(paste(dat$Name,dat$Year,sep="")),]

dat.summer<-dat.uniq[unlist(gregexpr("Summer",dat.uniq$Games))>0,]

dat.summer$Age<-as.numeric(as.character(dat.summer$Age))

#Remove some sports
#dat.summer<-dat.summer[dat.summer$Sport!="Motorboating",]
#dat.summer<-dat.summer[dat.summer$Sport!="Art Competitions",]
#dat.summer<-dat.summer[dat.summer$Sport!="Croquet",]
#dat.summer<-dat.summer[dat.summer$Sport!="Cricket",]
#dat.summer<-dat.summer[dat.summer$Sport!="Ice Hockey",]

dat.summer$SportSex<-paste(as.character(dat.summer$Sport),as.character(dat.summer$Sex),sep=", ")

#ordering
ab<-as.matrix((by(dat.summer$Age[!is.na(dat.summer$Age)],dat.summer$SportSex[!is.na(dat.summer$Age)],median)))
ord<-data.frame(rownames(ab),ab)
names(ord)<-c("SportSex","Age.median")
ord<-ord[!is.na(ord$Age.median),]
ord<-ord[order(ord$Age),]



dat.summer<-merge(dat.summer,ord,by.x="SportSex",by.y="SportSex",all.x=TRUE)
dat.summer$SportSex<-factor(dat.summer$SportSex,c(as.character(ord$SportSex)))

labs<-ord$SportSex
col<-rep("green",length(labs))
for (i in 1:length(labs)){
if (gregexpr(", F",labs[i])[[1]][1]>0){col[i]<-"pink"}
if (gregexpr(", M",labs[i])[[1]][1]>0){col[i]<-"blue"}
}


dat.summer$Age<-dat.summer$Age+rnorm(length(dat.summer$Age),0,.2)

par(oma=c(6,1,1,1))
boxplot(dat.summer$Age~dat.summer$SportSex,col=col,las=2,main="Age distribution of Olympic Athletes by Sport and Gender: All-time \n Female = Pink, Male = Blue, Both = Green",ylab="Age")
abline(h=seq(10,100,5),lty=3,col="gray75")


#By year
col<-c("lightblue","lightblue","salmon",rep("lightblue",6),"salmon","lightblue","lightblue","lightblue","gold","lightblue","lightgreen","salmon","lightblue","salmon","lightblue","salmon","lightgreen","lightblue","salmon","gold","lightblue","lightgreen")
par(oma=c(2,1,1,1))
boxplot(dat.summer$Age~dat.summer$Year,col=col,las=2,main="Age distribution of Olympic Athletes by Year: All-time" ,ylab="Age",sub="Color of boxplot represents host continent")
abline(h=seq(10,100,5),lty=3,col="gray75")
legend(0,60,c("Asia","Australia","Europe", "N. America"),col=c("lightgreen","gold","lightblue","salmon"),pch=16)

#By Gender
par(oma=c(2,1,1,1))
boxplot(dat.summer$Age~dat.summer$Sex,col=c("green","pink","blue"),las=2,main="Age distribution of Olympic Athletes by Year: All-time" ,ylab="Age",xlab="Gender")
abline(h=seq(10,100,5),lty=3,col="gray75")





par(oma=c(1,8,1,1))
boxplot(dat.summer$Age~paste(as.character(dat.summer$Sport),as.character(dat.summer$Sex),sep=", "),col=col,las=2,horizontal=TRUE)
abline(h=c(15,20,25,30,35,40,45),lty=3,col="gray75")
boxplot(dat.summer$Age~as.character(dat.summer$Sex)+as.character(dat.summer$Sport),col=c("pink","blue","green"),las=2)
axis(1,c(1:length(labs)),labs,las=2)



#load("Olympics_Scrape.RData")

