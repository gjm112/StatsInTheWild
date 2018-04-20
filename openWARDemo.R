install.packages("Sxslt", repos = "http://www.omegahat.org/R",
                 type = "source")
devtools::install_github("openWAR", "beanumber",ref="v0.1")
#This takes a little bit.  Some files here are big
#devtools::install_github("beanumber/openWARData") 

library(openWAR)
data)MLBAM
#Parititons RE24 to all players on a given play
#ds <- makeWAR(MLBAM2014)
#tabulated openWAR for a player.  
#war<-getWAR(ds$openWAR)

load("/Users/gregorymatthews/Dropbox/openWARData/data/openWAR.2014.rda")
war<-openWAR.2014

war[1,]
war[1,c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")]
#Top baserunners
war[order(-war$RAA.br),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
war[order(war$RAA.br),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
#Top fielders
war[order(-war$RAA.field),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
war[order(war$RAA.field),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
#Top pitchers
war[order(-war$RAA.pitch),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
war[order(war$RAA.pitch),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
#Top batters
war[order(-war$RAA.off),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]
war[order(war$RAA.off),c("Name","RAA.br","RAA.off","RAA.field","RAA.pitch","RAA","repl")][1:10,]


