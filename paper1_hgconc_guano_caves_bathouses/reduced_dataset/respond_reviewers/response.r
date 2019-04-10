library("ggplot2")
library("openxlsx")
library("nlme")
## library("lsmeans")
library("emmeans")
library("figdim")
library("userfriendlyscience")  ## Games-Howell, posthocTGH()
library("coin")  ## Wilcoxon-Mann-Whitney test, wilcox_test()


## #############################################
## Read in the data from the Excel file.
fileWpath = "../Hg_DATA_ActaChirp.xlsx"

## The first sheet contains measurements from caves.
cavesDF <- read.xlsx(xlsxFile = fileWpath, sheet="guano_caves",
                     detectDates=TRUE)
## Rename the columns to shorter names.
colnames(cavesDF) <- c("Place", "Species", "Date", "Region", "Notes",
                       "OM", "Mercury")

## The second sheet contains measurements from bat houses.
housesDF <- read.xlsx(xlsxFile = fileWpath, sheet="guano_bat houses",
                      detectDates=TRUE)
colnames(housesDF) <- c("Place", "Species", "Date", "Region", "Notes",
                        "OM", "Mercury")

## Now combine the caves and bat houses into one data frame, adding a
## column to denote whether the observation is from a cave or a bat
## house.
cavesDF$CaveOrHouse <- "cave"
housesDF$CaveOrHouse <- "bat house"
allDF <- rbind(cavesDF, housesDF)
rm(cavesDF, housesDF)

## Make CaveOrHouse and Region factor variables.
allDF$CaveOrHouse <- as.factor(allDF$CaveOrHouse)
allDF$Region <- as.factor(allDF$Region)

rm(fileWpath)
## #############################################



## #############################################
## Deal with the core samples.

## Some of these data come from core samples, with measurements taken
## every inch throughout the core.  We add an extra column that
## represents the core to which the measurements belong (contains NA
## for the rows which aren't from core samples).  I have to
## do this by hand, because some of the notes that contain "core" are
## composite measurements, or there could be more than one core per
## cave.

## Initialize coreID and distFromSurface variables:
allDF$coreID <- "not core"
allDF$distFromSurface <- 0


## For Climax Cave, we have a 10-in core:
allDF$coreID[11:20] <- "core 1"
allDF$distFromSurface[11:20] <- 0:9

## For Cottondale, we have a 6-in core:
allDF$coreID[27:32] <- "core 1"
allDF$distFromSurface[27:32] <- 0:5

## For Florida Caverns Old Indian Cave, we have two 8-in cores:
allDF$coreID[33:40] <- "core 1"
allDF$distFromSurface[33:40] <- 0:7
allDF$coreID[41:48] <- "core 2"
allDF$distFromSurface[41:48] <- 0:7

## For Judge's Cave, we have an 11-inch core and a 8-inch core:
allDF$coreID[66:76] <- "core 1"
allDF$distFromSurface[66:76] <- 0:10
allDF$coreID[77:84] <- "core 2"
allDF$distFromSurface[77:84] <- 0:7


## For UF Gainseville Bat House, we have two 6-in cores:
allDF$coreID[101:106] <- "core 1"
allDF$distFromSurface[101:106] <- 0:5
allDF$coreID[107:112] <- "core 2"
allDF$distFromSurface[107:112] <- 0:5
## #############################################


## #############################################
## Count number of locations in which cores were collected.

## Make table with the counts by coreID (core 1, core 2, not core).
tmpTbl <- with(allDF, table(Place, coreID))

## Now, find rows (places) in which there are some core
## observations and some non-core observations.
tmpTbl[(tmpTbl[,"not core"]>0) & ((tmpTbl[,"core 1"]>0) | (tmpTbl[,"core 2"]>0)),]
## These are Climax Cave, Cottondal, and Florida Caverns Old Indian Cave.

## Now, find rows (places) in which there are only core observations
## (no non-core observations).
tmpTbl[(tmpTbl[,"not core"]==0),]
## These are Judge's Cave and UF Gainesville Bat House.

rm(tmpTbl)
## #############################################



## #############################################
## Look at relationship among core measurements.  Do they have less
## variability than the non-core measurements?  Are there signs of
## strong correlation within each core?

## This plot does not indicate a strong pattern across cores and
## locations in Hg concentration with position in the core.  It also
## does not indicate that, in general, the spread of measurements in a
## core has much less variance than the other measurements taken in
## the cave.  For some caves, this might be true (such as Climax
## Cave), but it doesn't seem to be true for the Florida Caverns Old
## Indian Cave, for example.
ggplot(allDF, aes(x=coreID, y=Mercury, color=CaveOrHouse)) +
  scale_y_sqrt() +
  scale_shape_identity() +
  geom_jitter(mapping=aes(shape=48+distFromSurface), size=3, width=0.2) +
  facet_wrap(~Place)


## ##################
## Check for equality of variances between core and non-core populations
## in the same cave/bat house.

## For each cave/bat house look at variability between core 1, core 2,
## and not core measurements to try to get a sense of whether this
## variability is different for core vs. core or core vs. not core.
for (iPlace in unique(allDF$Place)){
  tmpDF <- subset(allDF, Place==iPlace)
  ## Check whether this cave has measurements in more than one category.
  if ( length(unique(tmpDF$coreID)) > 1 ){
    iResult <- with(tmpDF, fligner.test(Mercury ~ as.factor(coreID)))
    print(paste0(iPlace, ", p=", iResult$p.value))
  }
  else
    print(paste0(iPlace, " only has obs in '", unique(tmpDF$coreID), "'"))
}
rm(iPlace, iResult, tmpDF)
## For the 5 caves/bat houses which had core(s) measured, none of the
## p-values were less than 0.05.  The closest was Climax Cave with
## p=0.0691.  Seven other caves/bat houses did not have cores taken.
## ##################



## ##################
## For each core (1 or 2) in each cave/bat house which has cores,
## investiage autocorrelation for the core measurements.
par(mfrow=c(3,3))
for (iPlace in unique(allDF$Place)){
  for (jCore in c("core 1", "core 2")){
    ## Subset to the ith place, and exclude "not core" measurements.
    tmpDF <- subset(allDF, (Place==iPlace) & (coreID==jCore))

    ## Check to make sure we have more than one observation for the core.
    if (nrow(tmpDF) > 0){

      ## Check to make sure the cores are in order in our data.frame.
      if (is.unsorted(tmpDF$distFromSurface))
        stop(paste0("In ", iPlace, " core obs are not in sorted order!"))

      acf(tmpDF$Mercury, main=paste0(iPlace, " - ", jCore))
    }
    else
      print(paste0("In ", iPlace, " we don't have ", jCore))
  }
}
rm(iPlace, jCore, tmpDF)
## Of 8 cores distributed among the 12 caves/bathouses, core 1 in
## Judge's Cave (11 measurements) is the only core which shows some
## potential correlation.  The acf at lag 1 is estimated at 0.667,
## with a confidence inteval of (0.047, 1).  The significance cutoff
## is about 0.620.
## ##################


## ##################
## For each core (1 or 2) in each cave/bat house which has cores,
## investiage linear trend of mercury vs. distance from the top of the
## core.
par(mfrow=c(3,3))
for (iPlace in unique(allDF$Place)){
  for (jCore in c("core 1", "core 2")){
    ## Subset to the ith place, and exclude "not core" measurements.
    tmpDF <- subset(allDF, (Place==iPlace) & (coreID==jCore))

    ## Check to make sure we have more than one observation for the core.
    if (nrow(tmpDF) > 0){
      with(tmpDF, plot(distFromSurface, Mercury, main=paste0(iPlace, " - ", jCore)))
      tmpCorrel <- round(with(tmpDF, cor(distFromSurface, Mercury)), 2)
      tmppval <- round(with(tmpDF, cor.test(distFromSurface, Mercury))$p.value, 2)
      legend("topleft", paste0("r=", tmpCorrel, " p=", tmppval))
      print(paste0("For ", iPlace, " ", jCore, ": r=", tmpCorrel, " p=", tmppval))
    }
    else
      print(paste0("In ", iPlace, " we don't have ", jCore))
  }
}
rm(iPlace, jCore, tmpDF, tmpCorrel, tmppval)
## Of 8 cores distributed among the 12 caves/bathouses, core 1 in
## Judge's Cave (11 measurements) is the only core which shows
## significant linear correlation.  The correlation coefficient is
## estimated at about 0.89, with a p-value less than 0.01.
## ##################
#############################################



#############################################
## Type II error probability estimates for linear regression of
## mercury vs. distance from surface for core subsamples.

## Subset to only core samples.
onlycoresDF <- subset(allDF, coreID!="not core")

## Find the errors for the MSE for the regressions for each core.
powerParamsDF <- unique(onlycoresDF[,c("Place", "coreID")])
powerParamsDF$rse <- NA
powerParamsDF$n <- NA
powerParamsDF$cii <- NA
powerParamsDF$betahat <- NA
for (i in 1:nrow(powerParamsDF)){
  
  ## Subset to cave and core in row i.
  tmpDF <- subset(onlycoresDF, (Place==powerParamsDF$Place[i]) & (coreID==powerParamsDF$coreID[i]))

  ## Check to make sure we have more than one observation for the core.
  if (nrow(tmpDF)== 0){
    stop(paste0("No observations for ", powerParamsDF$Place[i], coreID==powerParamsDF$coreID[i], sep=""))
  }

  ## Conduct linear regression, saving the residual std. error, the number of observations for the core, and the fitted coefficient.
  my.lm <- lm(Mercury ~ distFromSurface, data=tmpDF)
  powerParamsDF$rse[i] <- summary(my.lm)$sigma
  powerParamsDF$n[i] <- nrow(tmpDF)
  powerParamsDF$cii[i] <- summary(my.lm)$cov.unscaled[2,2]
  powerParamsDF$betahat[i] <- my.lm$coefficients[["distFromSurface"]]
}
rm(i, tmpDF, my.lm)


## Possible combinations of sample size and size of depth effect.
beta.possible <- seq(0, 0.05, by=0.005)
n.possible <- 6:11
rse.possible <- c(0.05, 0.1, 0.15)
combosDF <- data.frame(expand.grid(beta.possible, n.possible, rse.possible))
colnames(combosDF) <- c("beta", "n", "rse")
combosDF$estType2Err <- NA
combosDF$estPower <- NA
rm(beta.possible, n.possible, rse.possible)
## Loop through potential number of core samples.
for (i in 1:nrow(combosDF)){
  n <- combosDF[i, "n"]
  beta <- combosDF[i, "beta"]
  rse <- combosDF[i, "rse"]
  
  designMat <- matrix(c(rep(1, n), seq(0, length=n)), ncol=2)
  cii <- solve(t(designMat) %*% designMat)[2,2]
  se.betahat <- rse * sqrt(cii)

  tstat <- qt(0.975, df=n-2)
  ## tstat <- qnorm(0.975)
  bdLower <- ((-tstat * se.betahat) - beta)/se.betahat
  bdUpper <- ((tstat * se.betahat) - beta)/se.betahat
  combosDF$estType2Err[i] <- pt(bdUpper, df=n-2) - pt(bdLower, df=n-2)
  ## combosDF$estType2Err[i] <- pnorm(bdUpper) - pnorm(bdLower)
  combosDF$estPower[i] <- 1 - combosDF$estType2Err[i]
}
rm(i, n, beta, rse, designMat, cii, se.betahat, tstat, bdLower, bdUpper)
## library("dplyr")
## combosDF %>% filter((estPower>=0.80) & (rse <= 0.09)) %>% arrange(rse, n, estPower)
## detach(package:dplyr)
#############################################



#############################################
## Subset data frame to sites with more than 2 observations.
## This excludes Big Mouth Cave (1 obs), Newberry Bat Cave (1 obs),
## Thornton's Cave (aka Sumter Bat Cave) (2 obs), Waterfall Cave (2
## obs).
enoughObsDF <- subset(allDF, Place %in% names(table(allDF$Place))[table(allDF$Place) > 2], c("CaveOrHouse", "Place", "Mercury", "coreID"))
#############################################




#############################################
## Make boxplot of each location:
## - bat houses in lighter gray, caves in darker gray
## - caves on left side, bat houses on right

## Set up colors.
myColors <- c(gray(0.8), gray(0.45))
names(myColors) <- c("bat house", "cave")

## Set up placement of boxplots. The first 6 places are caves; the
## last 2 are bat houses.  We want them in different shades and some
## spaced between them.
xPosit <- c(1:6, 7.75:8.75)

## Make boxplots of measurements by cave name.
init.fig.dimen(file="fig2_alternative_w_notched_boxplot_by_place_caves_bathouses.pdf", height=4.0, width=6.5,
               cex.axis=0.75, cex.lab=0.75, cex.main=0.75, cex.sub=0.75,
               mai=c(1.1, 0.5, 0.1, 0.1), tcl=-0.2)
## Save the parameters of the boxplot as you plot it.
paramsPlot <- boxplot(Mercury ~ Place, data=enoughObsDF,
                      outcex=0.6, xaxt="n", at=xPosit, notch=TRUE,
                      col=c(rep(myColors["cave"],6), rep(myColors["bat house"], 2)),
                      xlab=NA, ylab="Mercury concentration (ppm)")
## Put on reference lines.
abline(h=seq(0, 2, by=0.25), col="lightgray", lty=3)
## The cave names are so long that the x-axis has to be done
## separately, with carriage returns in the naming strings.
paramsPlot$names  ## See the names
caveNmPlot <- c("Climax Cave",
               "Cottondale\nCave",
               "Florida Caverns\nOld Indian Cave",
               "Jerome's\nBat Cave",
               "Judge's Cave",
               "Snead's Cave",
               "Suwannee NWR\nBat House",
               "UF Gainesville\nBat House")
axis(1, las=2, at=xPosit, labels=caveNmPlot)
## Re-plot the boxes so that they appear on top of the reference lines.
paramsPlot <- boxplot(Mercury ~ Place, data=enoughObsDF,
                      outcex=0.6, xaxt="n", at=xPosit, notch=TRUE,
                      col=c(rep(myColors["cave"],6), rep(myColors["bat house"], 2)),
                      xlab=NA, ylab="Mercury concentration (ppm)",
                      add=TRUE)
## Add sample size for each box.
text(xPosit, 1.1, paste("n =", paramsPlot$n), cex=0.7)  ## Height was 1.35
## Add labels for cave section and bat house section.
text(mean(xPosit[1:6]), 1.6, "Caves", cex=0.8)
text(mean(xPosit[7:8]), 1.6, "Bat houses", cex=0.8)
abline(v=mean(xPosit[6:7]))
dev.off()

rm(myColors, xPosit, paramsPlot, caveNmPlot)
#############################################



#############################################
## Examining just caves (no bat houses).

## Subset to only caves that have more than 2 observations.
cavesDF <- subset(enoughObsDF, CaveOrHouse=="cave")
cavesDF$Place <- as.factor(cavesDF$Place)


## ##########
## Check whether there is reason to believe that the variances are
## different between caves.
fligner.test(Mercury ~ Place, data=cavesDF)
library("car")
leveneTest(Mercury ~ Place, data=cavesDF)
## Indicates strong evidence of unequal variances (p<0.001).
## ##########


## ##########
## The above tests indicate that variances are likely unequal, so we
## run Welch's ANOVA.
oneway.test(Mercury ~ Place, data=cavesDF, var.equal=FALSE)
## Indicates strong evidence that at least one pair of means is
## different (p < 0.001).
## ##########


## ##########
## Not needed: Another oneway analysis of variance, with Welch correction for
## nonhomogeneous variances in the p-value at the end.
## oneway(y=cavesDF$Mercury, x=cavesDF$Place, corrections=TRUE, posthoc="games-howell")
## ##########


## ##########
## Use the Games-Howell post-hoc comparison procedure.
resDF <- posthocTGH(y=cavesDF$Mercury, x=cavesDF$Place, method="games-howell")[["output"]][["games.howell"]]#[c(2,4),]
## CI for FL Caverns Old Indian - Climax: (0.1140023, 0.2800839) or approx 0.20 +/- 0.08
## CI for Judge's - Climax: (0.1037451, 0.3123842) or approx 0.21 +/- 0.10
## ##########


## ##########
## Try gls.

## Fit gls model (function in package "nlme").
caves.gls <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place),
                data=cavesDF)
## Use emmeans package, which supersedes the original lsmeans.
## See
## https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
caves.emm <- emmeans(caves.gls, "Place")
## To get test stats and p-values for pairwise differences.
## pairs(caves.emm)
## To get CIs, test stats, and p-values for pairwise differences.
summary(pairs(caves.emm), infer=c(TRUE, TRUE))
## To get CIs.
cavePairCIs <- as.data.frame(confint(pairs(caves.emm)))#[c(2,4),]
## CI for Climax - FL Caverns Old Indian: (-0.2800842, -0.11400198) or approx -0.197043095 +/- 0.08304111
## CI for Climax - Judge's: (-0.3123847, -0.10374456) or approx -0.294196667 +/- 0.1043201


## Fit the contrast for the average of caves.
avgCaveContr <- rep(1/6, 6)  # average of 6 caves
## Find the conf. interval.
est.avg <- sum(avgCaveContr * coef(caves.gls)) # t(c) %*% betahat
est.avg.sd <- sqrt(as.numeric(t(avgCaveContr) %*% caves.gls$varBeta %*% avgCaveContr))
t.df <- nrow(enoughObsDF) - length(coef(caves.gls))
ci.avg <- est.avg + c(qt(0.025, df=t.df)*est.avg.sd, qt(0.975, df=t.df)*est.avg.sd)
## CI is (0.4998639, 0.6045785), or 0.5522212 +/- 0.05235732
pval.avg <- 2 * pt(-abs(est.avg/est.avg.sd), df=t.df)
## p-val: 2.575184e-38
rm(avgCaveContr, est.avg, est.avg.sd)

rm(caves.gls)
## ##########
#############################################



#############################################
## Now, look at just bat houses (no caves).

bathousesDF <- subset(enoughObsDF, CaveOrHouse=="bat house")
bathousesDF$Place <- as.factor(bathousesDF$Place)

## ##########
## First, using Welch's t test, since there are only 2 bat houses.
t.test(Mercury ~ Place, data=bathousesDF)
## p-val approx. 0.0014
## CI: (0.2248231 0.5059352) or approx 0.3653791 +/- 0.140556
## ##########


## ##########
## Fit gls model (function in package "nlme").
bathouses.gls <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place),
                data=bathousesDF)
bathouses.emm <- emmeans(bathouses.gls, "Place")
## To get test stats and p-values for pairwise differences.
## pairs(caves.emm)
## To get CIs, test stats, and p-values for pairwise differences.
summary(pairs(bathouses.emm), infer=c(TRUE, TRUE))
## 95% CI: (0.2514467, 0.4793117), or 0.3653791 +/- 0.1405561


## Fit the contrast for the average of bat houses.
avgHouseContr <- rep(1/2, 2)  # average of 2 bat houses
## Find the conf. interval.
est.avg <- sum(avgHouseContr * coef(bathouses.gls)) # t(c) %*% betahat
est.avg.sd <- sqrt(as.numeric(t(avgHouseContr) %*% bathouses.gls$varBeta %*% avgHouseContr))
t.df <- nrow(enoughObsDF) - length(coef(bathouses.gls))
ci.avg <- est.avg + c(qt(0.025, df=t.df)*est.avg.sd, qt(0.975, df=t.df)*est.avg.sd)
## CI is (0.4592107, 0.5652101), or 0.5122104 +/- 0.05299971
pval.avg <- 2 * pt(-abs(est.avg/est.avg.sd), df=t.df)
## p-val: 6.60701e-36
rm(avgHouseContr, est.avg, est.avg.sd)
## ##########


## ##########
## What about using the Wilcoxon-Mann-Whitney test, since the samples
## here are small (leading to possible questions about normality)?
wilcox_test(Mercury ~ as.factor(Place), data=bathousesDF, conf.level=0.95, distribution="exact")
## p-value = 0.0003232
## For asymptotic version of the test:
## wilcox_test(Mercury ~ as.factor(Place), data=bathouseDF, conf.level=0.95)
## p-value = 0.001565
## ##########
#############################################



#############################################
## ##########
## Look at all locations with more than 2 observations, including both
## caves and bat houses.

## Fit gls model (function in package "nlme").
all.gls <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place),
                data=enoughObsDF)

all.emm <- emmeans(all.gls, "Place")
## To get test stats and p-values for pairwise differences.
## pairs(all.emm)
## To get CIs, test stats, and p-values for pairwise differences.
summary(pairs(all.emm), infer=c(TRUE, TRUE))

## To get CIs.
allPairCIs <- as.data.frame(confint(pairs(all.emm)))#[c(2,4,6,18,25,28),]


## Fit the contrast for the difference (avg caves - avg bat houses).
diffTypeContr <- c(rep(1/6, 6), rep(-1/2, 2))  # 6 caves, 2 bat houses
## Find the conf. interval.
est.diff <- sum(diffTypeContr * coef(all.gls)) # t(c) %*% betahat
est.diff.sd <- sqrt(as.numeric(t(diffTypeContr) %*% all.gls$varBeta %*% diffTypeContr))
t.df <- nrow(enoughObsDF) - length(coef(all.gls))
ci.diff <- est.diff + c(qt(0.025, df=t.df)*est.diff.sd, qt(0.975, df=t.df)*est.diff.sd)
## CI is (-0.03452572,  0.11454729), or 0.04001079 +/- 0.07453651
pval.diff <- 2 * pt(-abs(est.diff/est.diff.sd), df=t.df)
## p-val: 0.2893798
rm(diffTypeContr, est.diff, est.diff.sd)
## ##########
#############################################



#############################################
## Power analysis by Monte Carlo.

library("mvtnorm")
## Find the mean, sd, and number of observations for each place.
## Then, use these in the Monte Carlo.
paramsT <- enoughObsDF %>% group_by(Place) %>% summarize(mean=mean(Mercury), sd=sd(Mercury), n=n())

## We'll generate samples from multivariate normal distribution, with
## diagonal coveriance matrix.
meanVec <- rep(paramsT$mean, times=paramsT$n)
varVec <- rep(paramsT$sd^2, times=paramsT$n)
placeVec <- rep(paramsT$Place, times=paramsT$n)

numIter <- 1000
pval.diff <- rep(NA, numIter)

set.seed(79348342)
for (i in 1:numIter){

  ## Generate sample from multiv. normal, put into data frame.
  iSample <- rmvnorm(n=1, mean=meanVec, sigma=diag(varVec))
  iDF <- data.frame(Place=placeVec, Mercury=as.vector(iSample))

  ## Fit gls model (function in package "nlme").
  all.gls <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place),
                data=iDF)

  ## Fit the contrast for the difference (avg caves - avg bat houses).
  diffTypeContr <- c(rep(1/6, 6), rep(-1/2, 2))  # 6 caves, 2 bat houses
  ## Find the conf. interval.
  est.diff <- sum(diffTypeContr * coef(all.gls)) # t(c) %*% betahat
  est.diff.sd <- sqrt(as.numeric(t(diffTypeContr) %*% all.gls$varBeta %*% diffTypeContr))
  t.df <- nrow(iDF) - length(coef(all.gls))
  ## ci.diff <- est.diff + c(qt(0.025, df=t.df)*est.diff.sd, qt(0.975, df=t.df)*est.diff.sd)
  pval.diff[i] <- 2 * pt(-abs(est.diff/est.diff.sd), df=t.df)
  rm(diffTypeContr, est.diff, est.diff.sd)
}


#############################################



#############################################
## Re-do analysis for caves, replace the Judge's Cave core 1
## observations with the average of this core.

## Subset to only caves that have more than 2 observations.
cavesDF <- subset(enoughObsDF, CaveOrHouse=="cave")
cavesDF$Place <- as.factor(cavesDF$Place)

library("dplyr")
cavesDF <- bind_rows(
    cavesDF %>% filter((Place!="Judge's Cave") | (coreID!="core 1")),
    cavesDF %>% filter((Place=="Judge's Cave") & (coreID=="core 1")) %>% group_by(CaveOrHouse, Place, coreID) %>% summarize(Mercury=mean(Mercury))
)

## ##########
## Check whether there is reason to believe that the variances are
## different between caves.
fligner.test(Mercury ~ Place, data=cavesDF)
library("car")
leveneTest(Mercury ~ Place, data=cavesDF)
## Indicates strong evidence of unequal variances (p<0.001).
## ##########


## ##########
## The above tests indicate that variances are likely unequal, so we
## run Welch's ANOVA.
oneway.test(Mercury ~ Place, data=cavesDF, var.equal=FALSE)
## Indicates strong evidence that at least one pair of means is
## different (p < 0.001).
## ##########


## ##########
## Use the Games-Howell post-hoc comparison procedure.
resDF <- posthocTGH(y=cavesDF$Mercury, x=cavesDF$Place, method="games-howell")[["output"]][["games.howell"]]#[c(2,4),]
## CI for FL Caverns Old Indian - Climax: (0.1140023, 0.2800839) or approx 0.20 +/- 0.08
## CI for Judge's - Climax: (0.004389111, 0.3790209) or approx 0.1917 +/- 0.1873
## ##########


## ##########
## Try gls.

## Fit gls model (function in package "nlme").
caves.gls <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place),
                data=cavesDF)
## Use emmeans package, which supersedes the original lsmeans.
## See
## https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
caves.emm <- emmeans(caves.gls, "Place")
## To get test stats and p-values for pairwise differences.
## pairs(caves.emm)
## To get CIs, test stats, and p-values for pairwise differences.
summary(pairs(caves.emm), infer=c(TRUE, TRUE))
## To get CIs.
cavePairCIs <- as.data.frame(confint(pairs(caves.emm)))#[c(2,4),]
## CI for Climax - FL Caverns Old Indian: (-0.2800844, -0.114001743)
## CI for Climax - Judge's: (-0.3735184, -0.009891646)


## Fit the contrast for the average of caves.
avgCaveContr <- rep(1/6, 6)  # average of 6 caves
## Find the conf. interval.
est.avg <- sum(avgCaveContr * coef(caves.gls)) # t(c) %*% betahat
est.avg.sd <- sqrt(as.numeric(t(avgCaveContr) %*% caves.gls$varBeta %*% avgCaveContr))
t.df <- nrow(enoughObsDF) - length(coef(caves.gls))
ci.avg <- est.avg + c(qt(0.025, df=t.df)*est.avg.sd, qt(0.975, df=t.df)*est.avg.sd)
## CI is (0.4954037, 0.6035855), or 0.5494946 +/- 0.05409087
pval.avg <- 2 * pt(-abs(est.avg/est.avg.sd), df=t.df)
## p-val: 5.383908e-37
rm(avgCaveContr, est.avg, est.avg.sd)

rm(caves.gls)
## ##########
#############################################
