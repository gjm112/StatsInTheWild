ladders <- data.frame(
  start = c(1,4,9,21,28,36,51,71,80),
  end = c(38,14,31,42,84,44,67,91,100)
)

chutes <- data.frame(
  start = c(16,47,49,56,62,64,87,93,95,98),
  end = c(6,26,11,53,19,60,24,73,75,78)
)

set.seed(1234)
sims <- list()
for (j in 1:10000){print(j)

#start at 0
position <- 0  
i <- 1
path <- c()

while (position < 100){
#Spin the spinner
dice <- sample(1:6, 1)

#Now move
position <- position + dice

#check chutes and ladders
if (position %in% ladders$start){
  position <- ladders$end[which(ladders$start == position)]
}

if (position %in% chutes$start){
  position <- chutes$end[which(chutes$start == position)]
}

path[i] <- position
i <- i + 1
}


sims[[j]] <- path


}

#Distributuon of turns needed to complete the game
hist(unlist(lapply(sims, length)), xlab = "number of turns", main = "Single Player")
summary(unlist(lapply(sims, length)))

#Game length with p players
hist(apply(matrix(unlist(lapply(sims, length)),ncol = 2),1,min))
summary(apply(matrix(unlist(lapply(sims, length)),ncol = 1),1,min))
summary(apply(matrix(unlist(lapply(sims, length)),ncol = 2),1,min))
summary(apply(matrix(unlist(lapply(sims, length)),ncol = 3),1,min))
summary(apply(matrix(unlist(lapply(sims, length)),ncol = 4),1,min))

#Combine into a data frame
a <- data.frame(players = 1, turns = apply(matrix(unlist(lapply(sims, length)),ncol = 1),1,min))
b <- data.frame(players = 2, turns = apply(matrix(unlist(lapply(sims, length)),ncol = 2),1,min))
c <- data.frame(players = 3, turns = apply(matrix(unlist(lapply(sims, length)),ncol = 3),1,min))
d <- data.frame(players = 4, turns = apply(matrix(unlist(lapply(sims, length)),ncol = 4),1,min))
dat <- rbind(a, b, c, d)
dat$players <- factor(dat$players)

#side by side boxplots
library(ggplot2)
ggplot(aes(y = turns, x = factor(players), color = factor(players)), data = dat) + geom_boxplot()

#Shortest game
which(unlist(lapply(sims, length)) == 7)
sims[[1466]]

#longest game
which(unlist(lapply(sims, length)) == 243)
plot(sims[[5665]], type = "l")

#Creating the image with path.  
library(png )
test <- readPNG("/Users/gregorymatthews/chutes.png")

plot(0,0,xlim = c(0,100), ylim = c(0,100), asp = 1, col = "white", frame.plot = FALSE, xaxt = 'n', yaxt = 'n', xlab = "", ylab = "")
rasterImage(test,0,0,100,100)
#points(5,5, pch = 16, col = "red", cex = 3)


points(c(5,25,5,5,35,95,95,65,95,95,65,5,5),c(5,35,35,45,45,45,55,65,65,75,75,75,95), pch = 16, type = "l", lwd = 3)

points(25,35, pch = 16, col = "red", cex = 3)
points(35,45, pch = 16, col = "red", cex = 3)
points(95,45, pch = 16, col = "red", cex = 3)
points(65,65, pch = 16, col = "red", cex = 3)
points(75,65, pch = 16, col = "red", cex = 3)
points(65,75, pch = 16, col = "red", cex = 3)
points(5,75, pch = 16, col = "red", cex = 3)

#Rolls: 1, 6, 6, 1, 1, 6, 6
