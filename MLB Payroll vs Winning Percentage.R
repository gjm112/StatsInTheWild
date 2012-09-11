library(XML)
#read in MLB salary data
url<-"http://content.usatoday.com/sportsdata/baseball/mlb/salaries/team"
x<-readHTMLTable(url)

#Organize the data
dat<-x[[2]][,1:2]
dat$TEAM<-as.character(dat$TEAM)
dat$'TOTAL PAYROLL'<-as.character(dat$'TOTAL PAYROLL')
dat$'TOTAL PAYROLL'<-substring(gsub("[,]","",dat$'TOTAL PAYROLL'),4,length(dat$'TOTAL PAYROLL'))
#Manually adding winning percentages
dat$WPCT<-c(.489,.57,.557,.447,.390,.543,.599,.418,.407,.521,.312,.450,.546,.525,.444,.496,.418,.461,.564,.571,.496,.514,.472,.560,.475,.532,.550,.593,.460,.617)

#Make payroll numeric
dat$'TOTAL PAYROLL'<-as.numeric(dat$'TOTAL PAYROLL')

#sort data
dat<-dat[order(dat$'TOTAL PAYROLL'),]
dat$'Payroll'<-c(rep("$",10),rep("$$$",10),rep("$$$$$",10))

#Plot using ggplot2!
qplot(dat$'TOTAL PAYROLL'/1000000,dat$WPCT,col=Payroll,data=dat)+geom_boxplot()+geom_jitter()+ylab("Win Percentage")+xlab("Payroll (Millions of $)")+geom_text(aes(y=dat$WPCT-.004,label=dat$TEAM))+xlim(49.5,202)+opts(title="Payroll vs Winning Percentage Major League Baseball \n September 11, 2012")


