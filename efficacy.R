#Case 1
n_vax <- 10000
n_unvax <- 10000

cases_vax <- 250
cases_unvax <- 5000

incidence_rate_vax <- cases_vax / n_vax

incidence_rate_unvax <- cases_unvax / n_unvax

RR <- (incidence_rate_vax) / (incidence_rate_unvax)

efficacy <- 1 - RR
efficacy

odds_vax <- incidence_rate_vax / (1 - incidence_rate_vax)
odds_unvax <- incidence_rate_unvax / (1 - incidence_rate_unvax)

odds_vax / odds_unvax

set.seed(1234)
x <- seq(0,1,length = 100)
y <- seq(0,1,length = 100)
dat <- expand.grid(x,y)
names(dat) <- c("x","y")
col <- rep("black",10000)
col[sample(10000,250)] <- "red"
plot(dat$x, dat$y, col=col, asp = 1, cex = 0.1, xlim = c(0,2))
points(dat$x[col == "red"], dat$y[col == "red"], col = "red", pch = 16, cex = 0.1)

x <- seq(1,2,length = 100)
y <- seq(0,1,length = 100)
dat <- expand.grid(x,y)
names(dat) <- c("x","y")
col <- rep("black",10000)
col[sample(10000,5000)] <- "red"
points(dat$x, dat$y, col=col, asp = 1, cex = 0.1)
points(dat$x[col == "red"], dat$y[col == "red"], col = "red", pch = 16, cex = 0.1)

text(0.5, 1.1, "Vaccinated")
text(1.5, 1.1, "Unvaccinated")

text(0.5,-.15, "Incidence Rate \n 2.5%")
text(1.5,-.15, "Incidence Rate \n 50%")

text(1,1.4, "Efficacy = 95%")

#Case 2
n_vax <- 10000
n_unvax <- 10000

cases_vax <- 5
cases_unvax <- 100

(incidence_rate_vax <- cases_vax / n_vax)

(incidence_rate_unvax <- cases_unvax / n_unvax)

RR <- (incidence_rate_vax) / (incidence_rate_unvax)

efficacy <- 1 - RR
efficacy

odds_vax <- incidence_rate_vax / (1 - incidence_rate_vax)
odds_unvax <- incidence_rate_unvax / (1 - incidence_rate_unvax)

odds_vax / odds_unvax


set.seed(1234)
x <- seq(0,1,length = 100)
y <- seq(0,1,length = 100)
dat <- expand.grid(x,y)
names(dat) <- c("x","y")
col <- rep("black",10000)
col[sample(10000,5)] <- "red"
plot(dat$x, dat$y, col=col, asp = 1, cex = 0.1, xlim = c(0,2))
points(dat$x[col == "red"], dat$y[col == "red"], col = "red", pch = 16, cex = 0.1)

x <- seq(1,2,length = 100)
y <- seq(0,1,length = 100)
dat <- expand.grid(x,y)
names(dat) <- c("x","y")
col <- rep("black",10000)
col[sample(10000,100)] <- "red"
points(dat$x, dat$y, col=col, asp = 1, cex = 0.1)
points(dat$x[col == "red"], dat$y[col == "red"], col = "red", pch = 16, cex = 0.1)

text(0.5, 1.1, "Vaccinated")
text(1.5, 1.1, "Unvaccinated")

text(0.5,-.15, "Incidence Rate \n 0.05%")
text(1.5,-.15, "Incidence Rate \n 1%")

text(1,1.4, "Efficacy = 95%")

