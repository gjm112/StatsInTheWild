#get the players names
library(XML)
library(RCurl)
death.list<-birth.list<-list()
nama.list<-year.list<-list()
height.list<-weight.list<-list()

for (lt in LETTERS){print(lt)
 for (nb in 1:8){print(nb)
url2<-paste0("http://www.football-almanac.com/players/players-",lt,nb,".shtml")
nam<-getURL(url2,header=FALSE)
nama<-strsplit(nam,"\n")[[1]]
nama<-nama[unlist(gregexpr("/players/",nama))>0]
nama<-substring(nama,unlist(gregexpr('">',nama))+2,nchar(nama)-8)
nama.list[[lt]]<-nama[unlist(gregexpr("currentplayer",nama))<0]

##Scrape players ages
for (i in nama.list[[lt]]){print(i)
tmp<-strsplit(i," ")[[1]]
url<-paste0("http://www.pro-football-reference.com/players/",lt,"/",substring(tmp[2],1,min(4,nchar(tmp[2]))),substring(tmp[1],1,min(2,nchar(tmp[1]))),"20.htm")

test<-getURL(url)
abb<-strsplit(test,"\n")[[1]]
birth<-abb[unlist(gregexpr("data-birth",abb))>0]
if (!sum(unlist(gregexpr("data-birth",abb))>0)==0){birth.list[[i]]<-substring(birth,gregexpr("data-birth",birth)[[1]][1]+12,gregexpr("data-birth",birth)[[1]][1]+21)}
death<-abb[unlist(gregexpr("data-death",abb))>0]
if (!sum(unlist(gregexpr("data-death",abb))>0)==0){death.list[[i]]<-substring(death,gregexpr("data-death",death)[[1]][1]+12,gregexpr("data-death",death)[[1]][1]+21)}

if (!sum(unlist(gregexpr("data-birth",abb))>0)==0 & !sum(unlist(gregexpr("Height",abb))>0)==0){
hw<-abb[unlist(gregexpr("Height",abb))>0]
height.list[[i]]<-substring(gsub(" ","",strsplit(hw,">")[[1]][4]),1,3)
weight.list[[i]]<-substring(gsub(" ","",strsplit(hw,">")[[1]][6]),1,3)

yr<-abb[unlist(gregexpr("/years",abb))>0]
yr<-unique(substring(gsub("[^0-9]","",yr),1,4))[unique(substring(gsub("[^0-9]","",yr),1,4))!=""]
year.list[[i]]<-yr

}

}

}

}




#get the players names for baseball
library(XML)
library(RCurl)
#death.list<-birth.list<-list()
#nama.list<-year.list<-list()
#height.list<-weight.list<-list()

for (lt in letters[-1]){print(lt)
 for (nb in 1:8){print(nb)
#url2<-paste0("http://www.football-almanac.com/players/players-",lt,nb,".shtml")
url2<-paste0("http://www.baseball-almanac.com/players/player-",lt,".shtml")
nam<-getURL(url2,header=FALSE)
nama<-strsplit(nam,"\n")[[1]]
nama<-nama[unlist(gregexpr("Show stats",nama))>0]
nama.list[[paste0(lt,nb)]]<-nama<-substring(nama,unlist(gregexpr('[a-z.]\">[A-Z]',nama))+3,unlist(gregexpr('</a>',nama))-1)


##Scrape players ages
for (i in nama.list[[paste0(lt,nb)]]){print(i)
	for (j in 1:9){print(j)
tmp<-strsplit(i," ")[[1]]
#url<-paste0("http://www.pro-football-reference.com/players/",lt,"/",substring(tmp[2],1,min(4,nchar(tmp[2]))),substring(tmp[1],1,min(2,nchar(tmp[1]))),"20.htm")
url<-paste0("http://www.baseball-reference.com/players/",lt,"/",substring(tolower(tmp[2]),1,min(5,nchar(tmp[2]))),substring(tolower(tmp[1]),1,2),"0",j,".shtml")

test<-getURL(url)
abb<-strsplit(test,"\n")[[1]]
birth<-abb[unlist(gregexpr("data-birth",abb))>0]
if (!sum(unlist(gregexpr("data-birth",abb))>0)==0){birth.list[[paste0(i,j)]]<-substring(birth,gregexpr("data-birth",birth)[[1]][1]+12,gregexpr("data-birth",birth)[[1]][1]+21)}
death<-abb[unlist(gregexpr("data-death",abb))>0]
if (!sum(unlist(gregexpr("data-death",abb))>0)==0){death.list[[paste0(i,j)]]<-substring(death,gregexpr("data-death",death)[[1]][1]+12,gregexpr("data-death",death)[[1]][1]+21)}

if (!sum(unlist(gregexpr("data-birth",abb))>0)==0 & !sum(unlist(gregexpr("Height",abb))>0)==0){
hw<-abb[unlist(gregexpr("Height",abb))>0]
height.list[[paste0(i,j)]]<-gsub("\"","",substring(gsub(" ","",strsplit(hw,">")[[1]][4]),1,4))
weight.list[[paste0(i,j)]]<-substring(gsub(" ","",strsplit(hw,">")[[1]][6]),1,3)

yr<-abb[unlist(gregexpr("year=",abb))>0]
yr<-gsub("[^0-9]","",substring(yr,unlist(gregexpr("year=",yr)),unlist(gregexpr("</a>",yr))-1))
yr<-sort(unique(substring(yr[nchar(yr)==8],1,4)))
year.list[[paste0(i,j)]]<-yr

}

}

}

}

}

birth<-as.data.frame(do.call(rbind,birth.list))
birth$name<-names(birth.list)

death<-as.data.frame(do.call(rbind,death.list))
death$name<-names(death.list)

dat<-merge(birth,death,by.x="name",by.y="name",all.x=TRUE)

year.list.2<-list()
for (i in names(year.list)){
year.list.2[[i]]<-c(min(as.numeric(year.list[[i]])),max(as.numeric(year.list[[i]])))
}
