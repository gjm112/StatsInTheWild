library(RCurl)
library(rjson)
datList<-list()

x<-fromJSON(getURL("http://elections.huffingtonpost.com/pollster/api/polls.json"))
for (i in 1:10){
  datList[[i]]<-x[[i]]
}

for (n in 1001:1000){print(n)
x<-fromJSON(getURL(paste0("http://elections.huffingtonpost.com/pollster/api/polls.json?page=",n)))
for (i in 1:10){
datList[[10*(n-1)+i]]<-x[[i]]
}
}

