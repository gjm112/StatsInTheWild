library(ggimage)
library(openintro)
library(RIPPEN)
library(parallel)
data(nfl)


#Collect league kicker data
kicker <- subset(nfl, PlayType=="Field Goal", select= c("FieldGoalDistance", "FieldGoalResult"))
kicker$Good <- (kicker$FieldGoalResult=="Good") + 0
kicker <- kicker[!is.na(kicker$Good),]
boot <- glm(Good ~ FieldGoalDistance, data = kicker, family = "binomial")
kickCoef <- boot$coefficients

kick_yards <- function(p){
  (log(p/(1-p)) - kickCoef[1])/(kickCoef[2])
}

####################################
#Get predict it prices
####################################
# development version, via devtools
devtools::install_github('danielkovtun/rpredictit')
library(rpredictit)

markets <- rpredictit::all_markets()
prices <- list()
#Democratic Prices 
for (i in state.name){print(i)
  ind <- intersect(grep(i,markets$name),grep("presidential",markets$name))
  if (i == "Virginia"){
    ind2 <- intersect(grep("West Virginia",markets$name),grep("presidential",markets$name))
    ind <- setdiff(ind,ind2)
  }
  
  temp <- markets[ind,]
  dem <- temp$lastTradePrice[temp$contract_name == "Democratic"]
  rep <- temp$lastTradePrice[temp$contract_name == "Republican"]
  prices[[i]] <- c(dem, rep)
}

df <- data.frame(do.call(rbind,prices))
df <- as.data.frame(t(apply(df[,1:2],1, function(x){x/sum(x)})))
names(df) <- c("demPrice","repPrice")
df$state <- names(prices)
}

df$demFG <- kick_yards(df$demPrice)
df$repFG <- kick_yards(df$repPrice)




fte <- read.csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_state_toplines_2020.csv")

fte$demFG <- kick_yards(fte$winstate_chal)
fte$repFG <- kick_yards(fte$winstate_inc)

fte$abbr <- state2abbr(fte$state)

fte$voteshare_inc_adj <- fte$voteshare_inc - 25
fte$voteshare_chal_adj <- fte$voteshare_chal - 25

sub <- subset(fte, modeldate == "9/1/2020" & !is.na(abbr) & abbr != "DC")
sub$x <- seq(0.5,160/3, length = nrow(sub))

#Manual adjustments 
sub$x[sub$abbr == "ME"] <- sub$x[sub$abbr == "ME"] + 5
sub$x[sub$abbr == "MS"] <- sub$x[sub$abbr == "MS"] + 5
sub$x[sub$abbr == "KS"] <- sub$x[sub$abbr == "KS"] + 5
sub$x[sub$abbr == "SD"] <- sub$x[sub$abbr == "SD"] + 5

sub$x[sub$abbr == "UT"] <- sub$x[sub$abbr == "UT"] - 5
sub$x[sub$abbr == "MT"] <- sub$x[sub$abbr == "MT"] - 5
sub$x[sub$abbr == "LA"] <- sub$x[sub$abbr == "LA"] - 5

#Football field 
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
#ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
#ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)
ymin <- 0
ymax <- 120
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

library(ggplot2)
library(cowplot)
png("/Users/gregorymatthews/test.png", h = 10, w = 10, res = 200, units = "in")
ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  geom_rect(aes(xmin = 0, xmax = 160/3, ymin = 110, ymax = 120), col = "blue", fill = "blue") +
  geom_text(aes(x = 160/3/2, y = 115, label = "Biden"), color = "white", size = 10) + 
  geom_rect(aes(xmin = 0, xmax = 160/3, ymin = 0, ymax = 10), col = "red", fill = "red") +
  geom_text(aes(x = 160/3/2, y = 5, label = "Trump"), color = "white", size = 10) + 
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
#geom_point(data = example_play, aes(x = (xmax-y), y = x, shape = team,fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
  #geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white",          vjust = 0.36, size = 3.5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  geom_label(aes(x = x, y = demFG, label = abbr), data = sub, color = rgb(1-(2*(sub$voteshare_chal-20))/100,0,(2*(sub$voteshare_chal-20))/100), size = 5) + geom_text(aes(x = 8, y = 1.5, label = "@statsinthewild")) + 
  geom_segment(aes(x = 160/3/2, y = 0, xend = 160/3/2, yend = 1), col = "gold", size = 2) + 
  geom_segment(aes(x = 160/3/2 - 2, y = 1, xend = 160/3/2 + 2, yend = 1), col = "gold", size = 2) + 
  geom_segment(aes(x = 160/3/2 - 2, y = 1, xend = 160/3/2 - 2, yend = 7), col = "gold", size = 2) + 
  geom_segment(aes(x = 160/3/2 + 2, y = 1, xend = 160/3/2 + 2, yend = 7), col = "gold", size = 2)
dev.off()
