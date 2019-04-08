overall.mean <- 0.55
overall.sd <- 0.15  ## Is about 0.12 in our data.
numIter <- 10000

## Sample size for each location.
n <- c(20, 11, 21, 12, 19, 6, 5, 12)
## Location name
locNm <- rep(c("C1", "C2", "C3", "C4", "C5", "C6", "H1", "H2"), times=n)
## To simulate equal sample sizes
##   n <- rep(14, 8)
##   locNm <- rep(c("C1", "C2", "C3", "C4", "H1", "H2", "H3", "H4"), times=n)
## Group name ("C" or "H")
grpNm <- substring(locNm, first=1, last=1)
N <- table(grpNm)

wrongtPvals <- rep(NA, numIter)
righttPvals <- rep(NA, numIter)

set.seed(6942307)
for (i in 1:numIter){

  ## Generate samples from normal distribution.
  iSample <- rnorm(sum(n), mean=overall.mean, sd=overall.sd)
  ## Calculate the sample mean for each group.
  iLocMean <- tapply(iSample, locNm, mean)
  ## Take first character of the location label to find out whether
  ## the sample mean belongs to cave ("C") or bat house ("H") group.
  ## Put locations and sample means in a data frame.
  igrpNm <- substring(names(iLocMean), first=1, last=1)
  testDF <- data.frame(grp=igrpNm, locMean=iLocMean)
  
  ## Do t.test comparing the "groups".
  ttestRes <- t.test(locMean ~ grp, data=testDF)
  ## Save p-value.
  wrongtPvals[i] <- ttestRes$p.value
  
  ## What should the p-value be, using all observations?
  righttPvals[i] <- t.test(iSample ~ substring(locNm, first=1, last=1))$p.value
}
rm(i, iSample, iLocMean, igrpNm, ttestRes)

sum(wrongtPvals<0.05)/length(wrongtPvals)
sum(righttPvals<0.05)/length(righttPvals)
