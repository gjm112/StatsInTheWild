par(mfrow=c(1,2),pty="s",ask=T)

intervals <- function(groupcorr = 0, numintervals = 100, groupsize = 5, 
      sampgroups=10, popgroups=5000, mu = 0, sigma = 1) {


#Description:

#     Simulate a population of clusters, then draw a simple random 
#     sample of clusters and construct interval estimates using incorrect
#     SRS formulae and formulae appropriate for cluster samples

#Usage:

#     intervals(groupcorr=0, numintervals = 100, groupsize = 5, sampgroups=10, 
#        popgroups=5000, mu = 0, sigma = 1)

#Arguments:

#groupcorr: The intracluster correlation coefficient rho.

#numintervals: Number of samples to be taken from population.
          
#groupsize: Number of elements in each population cluster.

#sampgroups: Number of clusters to be sampled

#popgroups: Number of clusters in population

#    mu:  Mean for generating population

# sigma: Standard deviation for generating population

#Value:

#   Produces 3 graphs:

#  (1) Dotplot of data from last sample, displaying similarity within clusters

#  (2) Interval estimates for mean using SRS formulas. Red lines
#      are the intervals that do not include the population mean

#  (3) Confidence intervals using clusters as unit of analysis.


  if (groupcorr < 0 | groupcorr > 1) stop("correlation must be between 0 and 1")

  betweenvar <- groupcorr * sigma^2
  withinvar <- sigma^2*(1 - groupcorr)
  group <- rep(1:sampgroups,each=groupsize)

 # Generate population with correlation structure

  gpmeans <- rnorm(popgroups,mu,sqrt(betweenvar))
  yy <- rep(gpmeans,each=groupsize) + rnorm(popgroups*groupsize,0,sqrt(withinvar))
  yy <- matrix(yy,nrow=groupsize,ncol=popgroups)

  indci <- matrix(0,ncol=2,nrow=numintervals)
  clci <- indci
  yy <- yy - mean(yy) + mu  # Adjust population so it has mean mu

  for (i in 1:numintervals) {

 # Take cluster sample of size groupsize

    sampindex <- sample(1:popgroups,sampgroups)
    mysamp <- yy[,sampindex]

    indci[i,] <- t.test(as.vector(mysamp))$conf.int
    sampmeans <- apply(mysamp,2,mean)
    clci[i,] <- t.test(sampmeans)$conf.int

  }

   print(paste("Data from sample",numintervals))
   print(mysamp)
   plot(group,as.vector(mysamp),main=paste("Data values from sample",numintervals),
       xlab="group number",ylab="x")
   plot(as.vector(mysamp),as.vector(mysamp),type="n",axes=F,xlab="",ylab="")

 # Determine if true value inside intervals

   indcover <- indci[,1] < mu & indci[,2] > mu
   indcol <- rep("black",numintervals)
   indcol[!indcover] <- "red"

   clcover <- clci[,1] < mu & clci[,2] > mu
   clcol <- rep("black",numintervals)
   clcol[!clcover] <- "red"

 # Draw confidence intervals

   plot(indci[,1],1:numintervals,type="n",xlim=c(-2*sigma+mu,2*sigma+mu),
      ylab="interval",xlab="",main = "assuming SRS")
   abline(v=mu)
   for ( i in 1:numintervals) lines( c(indci[i,1],indci[i,2]),c(i,i),col=indcol[i] )

   plot(clci[,1],1:numintervals,type="n",xlim=c(-2*sigma+mu,2*sigma+mu),
      ylab="interval",xlab="",main = "using sampling design")
   abline(v=mu)
   for ( i in 1:numintervals) lines( c(clci[i,1],clci[i,2]),c(i,i),col=clcol[i] )
}

