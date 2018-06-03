library(teamcolors)
library(xml2)
library(rvest)
library(plotly)
library(crosstalk)
library(dplyr)
library(plyr)

#Scrape drafts
draft <- list()
for (i in 2000:2017){print(i)
  url <- paste0("https://www.basketball-reference.com/draft/NBA_",i,".html")
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")
  temp <- rvest::html_table(node, header = TRUE)
  names(temp)[1:9] <- c("Rk","Pk","Tm","Player","College","Yrs","G","MP","PTS")
  temp$Year <- i
  draft[[i]] <- temp
}

draft2 <- do.call(rbind,draft)
draft2 <- subset(draft2, Rk != "Rk")


#Scrape Rosters
roster <- list()
for (j in c("GSW","CLE","BOS","HOU","POR","OKC","UTA","NOP","SAS","MIN","DEN","LAC","LAL","SAC","DAL","MEM","PHO","TOR","PHI","IND","MIA","MIL","WAS","DET","CHO","NYK","BRK","CHI","ORL","ATL")){print(j)
url <- paste0("https://www.basketball-reference.com/teams/",j,"/2018.html")
html <- xml2::read_html(url)
node <- rvest::html_nodes(html, css = "table")
temp <- rvest::html_table(node, header = TRUE)
temp[[1]]$Team <- j
roster[[j]] <- temp[[1]]
}

rost <- do.call(rbind,roster)
rost[7] <- "Country"

#Merge rosters and draft
draft2 <- merge(draft2,rost, by.x = "Player", by.y = "Player", all.x = TRUE)
draft2$Rk <- as.numeric(draft2$Rk)
draft2$Team <- factor(draft2$Team)

draft2$Team <- as.character(draft2$Team)
draft2$Team[is.na(draft2$Team)] <- "NONE"
draft2$Team <- as.factor(draft2$Team)

#Create the color palette
pal <- c(teamcolors$primary[teamcolors$name == "Atlanta Hawks"],
         teamcolors$primary[teamcolors$name == "Boston Celtics"],
         teamcolors$primary[teamcolors$name == "Brooklyn Nets"],
         teamcolors$primary[teamcolors$name == "Chicago Bulls"],
         teamcolors$primary[teamcolors$name == "Charlotte Hornets"],
         teamcolors$primary[teamcolors$name == "Cleveland Cavaliers"],
         teamcolors$primary[teamcolors$name == "Dallas Mavericks"],
         teamcolors$primary[teamcolors$name == "Denver Nuggets"],
         teamcolors$primary[teamcolors$name == "Detroit Pistons"],
         teamcolors$primary[teamcolors$name == "Golden State Warriors"],
         teamcolors$primary[teamcolors$name == "Houston Rockets"],
         teamcolors$primary[teamcolors$name == "Indiana Pacers"],
         teamcolors$primary[teamcolors$name == "Los Angeles Clippers"],
         teamcolors$primary[teamcolors$name == "Los Angeles Lakers"],
         teamcolors$primary[teamcolors$name == "Memphis Grizzlies"],
         teamcolors$primary[teamcolors$name == "Miami Heat"],
         teamcolors$primary[teamcolors$name == "Milwaukee Bucks"],
         teamcolors$primary[teamcolors$name == "Minnesota Timberwolves"],
         "pink",
         teamcolors$primary[teamcolors$name == "New Orleans Pelicans"],
         teamcolors$primary[teamcolors$name == "New York Knicks"],
         teamcolors$primary[teamcolors$name == "Oklahoma City Thunder"],
         teamcolors$primary[teamcolors$name == "Orlando Magic"],
         teamcolors$primary[teamcolors$name == "Philadelphia 76ers"],
         teamcolors$primary[teamcolors$name == "Phoenix Suns"],
         teamcolors$primary[teamcolors$name == "Portland Trail Blazers"],
         teamcolors$primary[teamcolors$name == "Sacramento Kings"],
         teamcolors$primary[teamcolors$name == "San Antonio Spurs"],
         teamcolors$primary[teamcolors$name == "Toronto Raptors"],
         teamcolors$primary[teamcolors$name == "Utah Jazz"],
         teamcolors$primary[teamcolors$name == "Washington Wizards"],"pink")
pal <- setNames(pal, c(as.character(sort(unique(draft2$Team)))))

#Plotly doing its thing
draft2 %>% SharedData$new(~Team) %>% plot_ly(x = ~Rk,y = ~Year, color = ~Team, text = ~paste(Player,Team), colors = pal)



#Now do convex hulls 
find_hull <- function(df) df[chull(df$Rk, df$Year), ]

hulls <- ddply(draft2[!is.na(draft2$Rk),c("Rk","Year","Team")], ~Team, find_hull)


#devtools::install_github('hadley/ggplot2')
draft2$Player <- factor(draft2$Player)
draft2$Yrs <- factor(draft2$Yrs)

#Convex hull plot
p <- ggplot(subset(draft2, Team != "NONE"), aes(x= Rk, y = Year, col = Team)) + geom_point(aes(text = Player)) + scale_colour_manual(values = pal) +
  geom_polygon(data=subset(hulls,Team != "NONE"), alpha = 0.5 , aes(x=Rk, y=Year, group=Team, fill = Team)) + scale_fill_manual(values = pal)
ggplotly(p, tooltip = c("x","y","text"))


