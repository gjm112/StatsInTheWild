#Take data from 2015 - 2019
dat <- subset(Batting, yearID <= 2019 & yearID >= 2015)
library(dplyr)
#RBI
Batting %>% subset(yearID <= 2019 & yearID >= 2015) %>%group_by(playerID) %>% summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI)) %>% arrange(-RBI) %>% left_join(People) %>% select(nameFirst,nameLast,HR,Hits,RBI)

#HR
Batting %>% subset(yearID <= 2019 & yearID >= 2015) %>%group_by(playerID) %>% summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI)) %>% arrange(-HR) %>% left_join(People) %>% select(nameFirst,nameLast,HR,Hits,RBI)

#Hits
Batting %>% subset(yearID <= 2019 & yearID >= 2015) %>%group_by(playerID) %>% summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI)) %>% arrange(-Hits) %>% left_join(People) %>% select(nameFirst,nameLast,HR,Hits,RBI)

#Runs
Batting %>% subset(yearID <= 2019 & yearID >= 2015) %>%group_by(playerID) %>% summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI), Runs = sum(R)) %>% arrange(-Runs) %>% left_join(People) %>% select(nameFirst,nameLast,HR,Hits,RBI)

#Doubles
Batting %>% subset(yearID <= 2019 & yearID >= 2015) %>%group_by(playerID) %>% summarise(HR = sum(HR), Hits = sum(H), RBI = sum(RBI), Runs = sum(R), Doubles = sum(X2B), SB = sum(SB)) %>% arrange(-Doubles) %>% left_join(People) %>% select(nameFirst,nameLast,HR,Hits,RBI, Runs, Doubles, SB)




