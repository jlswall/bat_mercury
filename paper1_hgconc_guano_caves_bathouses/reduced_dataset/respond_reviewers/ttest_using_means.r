overall.mean <- 0.55
overall.sd <- 0.15  ## Is about 0.12 in our data.
numIter <- 10000

n <- c(20, 11, 21, 12, 19, 6, 5, 12)
loc <- rep(c("C1", "C2", "C3", "C4", "C5", "C6", "H1", "H2"), times=n)
N <- sum(n)

pvals <- rep(NA, numIter)
confints <- matrix(NA, nrow=numIter, ncol=2)

set.seed(7194614)
for (i in 1:numIter){

  ## Generate samples from normal distribution.
  iSample <- rnorm(N, mean=overall.mean, sd=overall.sd)
  ## Calculate the sample mean for each group.
  iLocMean <- tapply(iSample, loc, mean)

  ## Take first character of the location label to find out whether
  ## the sample mean belongs to cave ("C") or bat house ("H") group.
  ## Put locations and sample means in a data frame.
  igrpNm <- substring(names(iLocMean), first=1, last=1)
  testDF <- data.frame(grp=igrpNm, locMean=iLocMean)
  
  ## Do t.test comparing the "groups".
  ttestRes <- t.test(locMean ~ grp, data=testDF, var.equal=TRUE)
  ## Save p-value and CI.
  pvals[i] <- ttestRes$p.value
  confints[i,] <- as.vector(ttestRes$conf.int)
}
rm(i, iSample, iLocMean, igrpNm, ttestRes)

sum(pvals<0.05)/length(pvals)
