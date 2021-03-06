library("ggplot2")
library("openxlsx")


## #############################################
## Read in the data from the Excel file.
## fileWpath = "C://Users//jenise//Google Drive//amy_bat_hg//reduced_dataset//Hg_DATA_ActaChirp.xlsx"
fileWpath = "Hg_DATA_ActaChirp.xlsx"

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
## Look at relationship among core measurements.  Do they have less
## variability than the non-core measurements?  Are there signs of
## strong correlation within each core?

## This plot does not indicate a strong pattern across cores and
## locations in Hg concentration with position in the core.  It also
## does not indicate that, in general, the spread of measurements in a
## core has much less variance than the other measurements taken in
## the cave.  For some caves, this might be true (such as Climax
## Cave), but it doesn't seem to be true for the Florida Caverns Old
## Indian Cave.
ggplot(allDF, aes(x=coreID, y=Mercury, color=CaveOrHouse)) +
  scale_y_sqrt() +
  scale_shape_identity() +
  geom_jitter(mapping=aes(shape=48+distFromSurface), size=3, width=0.2) +
  facet_wrap(~Place)



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
#############################################



#############################################
## Try gls.

library("nlme")

## Subset data frame to sites with more than 2 observations.
## then by location name.
enoughObsDF <- subset(allDF, Place %in% names(table(allDF$Place))[table(allDF$Place) > 2], c("CaveOrHouse", "Place", "Mercury", "coreID"))
enoughObsDF$Place <- as.factor(enoughObsDF$Place)

library("lsmeans")

## Use gls with no intercept, so each cave will have its own estimate.
glsfit <- gls(Mercury ~ -1 + Place, varIdent(form = ~1|Place), data=enoughObsDF)

## Fit the contrast for the difference (avg caves - avg bat houses).
diffTypeContr <- c(rep(1/6, 6), rep(-1/2, 2))  # 6 caves, 2 bat houses
## Find the conf. interval.
est.diff <- sum(diffTypeContr * coef(glsfit)) # t(c) %*% betahat
est.diff.sd <- sqrt(as.numeric(t(diffTypeContr) %*% glsfit$varBeta %*% diffTypeContr))
t.df <- nrow(enoughObsDF) - length(coef(glsfit))
ci.diff <- est.diff + c(qt(0.025, df=t.df)*est.diff.sd, qt(0.975, df=t.df)*est.diff.sd)
## CI is (-0.03452572,  0.11454729), or 0.04001079 +/- 0.07453651
pval.diff <- 2 * pt(-abs(est.diff/est.diff.sd), df=t.df)
## p-val: 0.2893798
rm(diffTypeContr, est.diff, est.diff.sd)


## ## Fit the contrast for the overall mean, assuming all locations equal.
## muContr <- c(rep(1/8, 8))  # 6 caves, 2 bat houses
## ## Find the conf. interval.
## est.mu <- sum(muContr * coef(glsfit)) # This is t(c) %*% betahat
## est.mu.sd <- sqrt(as.numeric(t(muContr) %*% glsfit$varBeta %*% muContr))
## t.df <- nrow(enoughObsDF) - length(coef(glsfit))
## ci.mu <- est.mu + c(qt(0.025, df=t.df)*est.mu.sd, qt(0.975, df=t.df)*est.mu.sd)
## ## CI is (0.500763, 0.583674), or 0.5422185 +/- 0.04145553
## pval.mu <- 2 * pt(-abs(est.mu/est.mu.sd), df=t.df)
## ## p-val is very close to 0.
## rm(muContr, est.mu, est.mu.sd, t.df)


## Fit the function for the overall mean, with equal weight for caves
## and bat houses.
adjmuContr <- c(0.5*rep(1/6, 6), 0.5*rep(1/2, 2))  # 6 caves, 2 bat houses
## Find the conf. interval.
est.adjmu <- sum(adjmuContr * coef(glsfit)) # This is t(c) %*% betahat
est.adjmu.sd <- sqrt(as.numeric(t(adjmuContr) %*% glsfit$varBeta %*% adjmuContr))
t.df <- nrow(enoughObsDF) - length(coef(glsfit))
ci.adjmu <- est.adjmu + c(qt(0.025, df=t.df)*est.adjmu.sd, qt(0.975, df=t.df)*est.adjmu.sd)
## CI is (0.4949476, 0.5694841), or 0.5322158 +/-0.03726825
pval.adjmu <- 2 * pt(-abs(est.mu/est.mu.sd), df=t.df)
## p-val is very close to 0.
rm(adjmuContr, est.adjmu, est.adjmu.sd, t.df)


## Look at differences between specific caves.
my.lsmeans <- lsmeans(glsfit, "Place")
pairs(my.lsmeans)  ## To get p-values for signif. differences.
my.lsmeans  ## To get CIs.
siteCIs <- as.data.frame(confint(pairs(my.lsmeans)))##[,c(1,2,5,6)]
## Climax Cave - Florida Caverns Old Indian Cave: -0.1970431 +/- 0.08532543
## Climax Cave - Judge's Cave: -0.2080646 +/- 0.1053992
## Suwannee NWR Bat House - UF Gainesville Bat House: 0.365379167 +/- 0.165567

## http://r.789695.n4.nabble.com/unequal-variance-assumption-for-lme-mixed-effect-model-td828664.html
## try1 <- gls(Mercury ~ CaveOrHouse, varIdent(form=~CaveOrHouse), data=enoughObsDF)  ## From nlme package
#############################################



#############################################
## Try JAGS.

library("rjags")

## Re-order the data frame so that it is sorted by bat house/cave,
## then by location name.
neworder <- order(allDF$CaveOrHouse, allDF$Place)
orderedDF <- allDF[neworder, c("CaveOrHouse", "Place", "Mercury", "coreID", "distFromSurface")]
enoughObsDF <- subset(orderedDF, Place %in% names(table(orderedDF$Place))[table(orderedDF$Place) > 2])

codeType <- c(1,2)
names(codeType) <- unique(enoughObsDF$CaveOrHouse)
type <- as.numeric(codeType[enoughObsDF$CaveOrHouse])

codeSite <- c(1:length(unique(enoughObsDF$Place)))
names(codeSite) <- unique(enoughObsDF$Place)
site <- as.numeric(codeSite[enoughObsDF$Place])


## ## ##########
## ## This model uses a t-likelihood, and still includes fixed effects
## ## for caves and bathouses, with separate variances for these 2
## ## groups.
## data <- list(y=enoughObsDF$Mercury, type=type, N=nrow(enoughObsDF), numType=length(codeType))
## init <- list(mu=1, tau=rep(1, data$numType))
## modelstring="
##   model {
##     for (i in 1:N){
##       y[i] ~ dt(mu + theta[type[i]], tau[type[i]], 4)
##     }
##     for (j in 1:(numType-1)) {
##       theta[j] ~ dnorm(0, 0.01)
##     }
##    theta[numType] <- -sum(theta[1:(numType-1)])
##    mu ~ dnorm(0, 0.01)
##    tau[1] ~ dgamma(1, 0.005)
##    tau[2] ~ dgamma(1, 0.005)
## }
## "
## model <- jags.model(textConnection(modelstring), data=data, inits=init)
## update(model, n.iter=10000)
## useTout <- coda.samples(model=model, variable.names=c("mu", "tau", "theta"),
##                        n.iter=100000, thin=50)
## plot(useTout)
## print(summary(useTout))
## ## ##########


## ##########
## This model includes:
##   fixed effects for caves and bathouses, as groups
##   fixed effects for individiual caves and houses
##   separate variances for individual caves and houses
##   a t likelihoood.

## For reproducibility:
set.seed(431402)

data <- list(y=enoughObsDF$Mercury, type=type, site=site, N=nrow(enoughObsDF), numType=length(codeType), numSite=length(codeSite))
init <- list(mu=1, tau=rep(1, data$numSite))
modelstring="
  model {
    for (i in 1:N){
      ## y[i] ~ dnorm(yhat[i], tau[site[i]])
      y[i] ~ dt(yhat[i], tau[site[i]], 4)
      yhat[i] = mu + theta[type[i]] + beta[site[i]]
      resid[i] = y[i] - yhat[i]
    }
    for (j in 1:(numType-1)) {
      theta[j] ~ dnorm(0, 0.0001)
    }
    theta[numType] <- -sum(theta[1:(numType-1)])
    ## First bat house means gets updated.
    beta[1] ~ dnorm(0, 0.0001)
    ## Second bat house is constrained.
    beta[2] <- -beta[1]
    ## All caves updated except for last one.
    for (k in 3:(numSite-1)){
      beta[k] ~ dnorm(0, 0.0001)
    }
    ## Last cave is constrained.
    beta[numSite] <- -sum(beta[3:(numSite-1)])
    for (k in 1:numSite){
      tau[k] ~ dgamma(1, 0.0001)
    }
    mu ~ dnorm(0, 0.0001)
}
"
model <- jags.model(textConnection(modelstring), data=data, inits=init)
update(model, n.iter=10000)
fancyfixed <- coda.samples(model=model,
                         variable.names=c("mu", "tau", "theta", "beta", "resid"),
                         n.iter=100000, thin=50)
plot(fancyfixed)
print(summary(fancyfixed))


## Look at resdiuals.
residMat <- fancyfixed[[1]][,substr(colnames(fancyfixed[[1]]), start=1, stop=3)=="res"]
hist(apply(residMat, 2, mean))
qqnorm(apply(residMat, 2, mean))
qqline(apply(residMat, 2, mean))


## Retrieve the draws for the variables of interest.
muDraws <- as.vector(fancyfixed[[1]][,"mu"])
thetaDraws <- as.matrix(fancyfixed[[1]][,substr(colnames(fancyfixed[[1]]), start=1, stop=5)=="theta"])
betaDraws <- as.matrix(fancyfixed[[1]][,substr(colnames(fancyfixed[[1]]), start=1, stop=4)=="beta"])


## Prediction interval for overall average.
quantile(muDraws, c(0.025, 0.975))
mean(muDraws)
## PI is about (0.4876, 0.5594)


## For effect of bat house vs. cave.
bathouseEffDraws <- muDraws + thetaDraws[,1]
caveEffDraws <- muDraws + thetaDraws[,2]
## Look at the differences between cave and bathouses.
quantile(bathouseEffDraws - caveEffDraws, c(0.025, 0.975))
## PI is about (-0.1052, 0.0364)


## For total effect at each location.
bathouseSiteEffDraws <- bathouseEffDraws + betaDraws[,1:2]
caveSiteEffDraws <- caveEffDraws + betaDraws[,3:8]
## Look at variances for each cave.
tauDraws <- as.matrix(fancyfixed[[1]][,substr(colnames(fancyfixed[[1]]), start=1, stop=3)=="tau"])

## Look at intervals for each bat house site.
apply(bathouseSiteEffDraws, 2, quantile, c(0.025, 0.975))
## Look at intervals for each cave site.
apply(caveSiteEffDraws, 2, quantile, c(0.025, 0.975))

## Climax Cave - Florida Caverns Old Indian Cave
quantile(caveSiteEffDraws[,1]-caveSiteEffDraws[,3], c(0.025, 0.975))
## (-0.2459, -0.1331) or  -0.1895 +/- 0.0564
## Climax Cave - Judge's Cave
quantile(caveSiteEffDraws[,1]-caveSiteEffDraws[,5], c(0.025, 0.975))
## (-0.2718, -0.1443) or -0.2080 +/- 0.0637

## Suwanee NWR Bat House - UF Gainesville Bat House 
quantile(bathouseSiteEffDraws[,1]-bathouseSiteEffDraws[,2], c(0.025, 0.975))
## (0.2637, 0.4826) or 0.3732 +/- 0.1094
## ##########

#############################################






## #############################################
## Get confidence interval for all measurements taken together.

with(allDF, hist(Mercury))
with(allDF, t.test(Mercury))
## (0.4903519, 0.5708774)
with(allDF, mean(Mercury))
with(allDF, qt(0.975, df=nrow(allDF)-1)*sd(Mercury)/sqrt(nrow(allDF)))
## 0.5306147 +/- 0.04026272

## Just for reference purposes, how would this confidence interval
## change if we used only surface measurements (not the lower portion
## of the cores)?
## with(subset(allDF, distFromSurface==0), t.test(Mercury))
## (0.4605022, 0.6010963)
## nSurface <- nrow(subset(allDF, distFromSurface==0))
## with(subset(allDF, distFromSurface==0), mean(Mercury))
## with(subset(allDF, distFromSurface==0),
##     qt(0.975, df=nSurface-1)*sd(Mercury)/sqrt(nSurface))
## 0.5307993 +/- 0.07029703
## rm(nSurface)
## #############################################



## #############################################
## Get confidence interval for each mean separately.

## For caves:
caveDF <- subset(allDF, CaveOrHouse=="cave")
with(caveDF, t.test(Mercury))
## (0.5032717, 0.5914671)
with(caveDF, mean(Mercury))
with(caveDF, qt(0.975, df=nrow(caveDF)-1)*sd(Mercury)/sqrt(nrow(caveDF)))
## 0.5473694 +/- 0.0440977
rm(caveDF)


## For bat houses:
bathouseDF <- subset(allDF, CaveOrHouse=="bat house")
with(bathouseDF, t.test(Mercury))
##  (0.3414909, 0.5324796)
with(bathouseDF, mean(Mercury))
with(bathouseDF, qt(0.975, df=nrow(bathouseDF)-1)*sd(Mercury)/sqrt(nrow(bathouseDF)))
## 0.4369853 +/- 0.09549435
rm(bathouseDF)
## #############################################



## #############################################
## Assess whether there is a difference in average Hg concentration
## for caves vs. bat houses.

## Make a figure with side-by-side boxplots for caves vs. bat houses.
library("figdim")
## Set up two colors of gray, one for boxplots with cave data and one
## for boxplots with bat house data.  Will use these colors in
## subsequent plots.
myColors <- c(gray(0.8), gray(0.45))
names(myColors) <- c("bat house", "cave")


## Set up dimensions of the graphics file.
init.fig.dimen(file="boxplot_caves_vs_houses.pdf", height=3.0, width=3.0,
               cex.axis=0.75, cex.lab=0.75, cex.main=0.75, cex.sub=0.75,
               mai=c(0.6, 0.5, 0.1, 0.1), tcl=-0.2)
boxplot(Mercury ~ CaveOrHouse, data=allDF,
        xlab="Location type", ylab="Mercury concentration (ppm)",
        col=myColors, outcex=0.6)
abline(h=seq(0, 2, by=0.25), col="lightgray", lty=3)
## Call boxplot for second time so that we can see the boxes on top of
## the reference lines.
boxplot(Mercury ~ CaveOrHouse, data=allDF,
        xlab="Location type", ylab="Mercury concentration (ppm)",
        col=myColors, outcex=0.6, add=T)
with(subset(allDF, CaveOrHouse=="bat house"),
     text(1, 1.35, paste("n =", sum(!is.na(Mercury))), cex=0.7 ) )
with(subset(allDF, CaveOrHouse=="cave"),
     text(2, 1.35, paste("n =", sum(!is.na(Mercury))), cex=0.7 ) )
dev.off()



## Now, perform t test to see whether the means are significantly
## different.
t.test(Mercury ~ CaveOrHouse, data=allDF)
## p-value = 0.03765
## 95% CI for mean of bat houses - caves: (-0.213934425, -0.006833758)
diff(tapply(allDF$Mercury, allDF$CaveOrHouse, mean))
## -0.1103841 +/- 0.1035503
## #############################################



## #############################################
## Test whether the mean concentrations are different among the caves.

## Build a subset with just the caves (no bat houses).
caveDF <- subset(allDF, CaveOrHouse=="cave")

## Visualize the measurements by cave.
ggplot(caveDF, aes(x=Place, y=Mercury, color=coreID)) +
  geom_jitter(width=0.3) +
  ## geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

## We only look at caves which have at least 3 measurements.
enoughObsDF <- subset(caveDF, Place %in% names(table(caveDF$Place))[table(caveDF$Place) > 2])
rm(caveDF)

## Visualize the retained measurements by cave.
ggplot(enoughObsDF, aes(x=Place, y=Mercury, color=coreID)) +
  geom_jitter(width=0.2) +
  ## geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))


## Make boxplots of measurements by cave name.
init.fig.dimen(file="boxplot_by_cavename.pdf", height=3.5, width=5.5,
               cex.axis=0.75, cex.lab=0.75, cex.main=0.75, cex.sub=0.75,
               mai=c(1.1, 0.5, 0.1, 0.1), tcl=-0.2)
## Save the parameters of the boxplot as you plot it.
paramsPlot <- boxplot(Mercury ~ Place, data=enoughObsDF,
        xlab=NA, ylab="Mercury concentration (ppm)",
        col=myColors["cave"], outcex=0.6, xaxt="n")
## Put on reference lines.
abline(h=seq(0, 2, by=0.25), col="lightgray", lty=3)
## The cave names are so long that the x-axis has to be done
## separately, with carriage returns in the naming strings.
paramsPlot$names  ## See the names
caveNmPlot <- c("Climax Cave",
               "Cottondale",
               "Florida Caverns \nOld Indian Cave",
               "Jerome's \nBat Cave",
               "Judge's Cave",
               "Snead's Cave")
axis(1, las=2, at=1:length(paramsPlot$names), labels=caveNmPlot)
## Re-plot the boxes so that they appear on top of the reference lines.
paramsPlot <- boxplot(Mercury ~ Place, data=enoughObsDF,
        xlab=NA, ylab="Mercury concentration (ppm)",
        col=myColors["cave"], outcex=0.6, xaxt="n", add=TRUE)
## Add sample size for each box.
text(1:length(paramsPlot$n), 1.35, paste("n =", paramsPlot$n), cex=0.7)
dev.off()
rm(paramsPlot, caveNmPlot)



## Check whether variances are significantly different between caves.
fligner.test(Mercury ~ as.factor(Place), data=enoughObsDF)
library("car")
leveneTest(Mercury ~ as.factor(Place), data=enoughObsDF)
## The above tests indicate that variances are likely unequal, so we
## run Welch's ANOVA.
oneway.test(Mercury ~ as.factor(Place), data=enoughObsDF, var.equal=FALSE)
## Use the Games-Howell test.
library("userfriendlyscience")
oneway(y=enoughObsDF$Mercury, x=as.factor(enoughObsDF$Place), posthoc="games-howell")
resDF <- posthocTGH(y=enoughObsDF$Mercury, x=as.factor(enoughObsDF$Place), method="games-howell")[["output"]][["games.howell"]]
## Significant differences between:
## Florida Caverns Old Indian Cave-Climax Cave:
##   (0.1140023, 0.2800839)  or 0.197043095 +/- 0.0830408
## Judge's Cave-Climax Cave:
##   (0.1037451, 0.3123842)  or 0.208064649 +/- 0.1043195


## To use standard ANOVA, with Tukey's multiple comparison technique:
## myAOV <- aov(Mercury ~ Place, data=enoughObsDF)
## summary(myAOV)
## TukeyHSD(myAOV)
## par(mar=c(5, 25, 4, 1))
## plot(TukeyHSD(myAOV), las=1)
## Significant differences between:
## Climax Cave and Florida Caverns Old Indian Cave
## Climax Cave and Jerome's Bat Cave
## Climax Cave and Judge's Cave

rm(enoughObsDF)
## #############################################





## #############################################
## Test whether the mean concentrations are different among the bat
## houses.

## Build a subset with just the bat houses (no caves).
bathouseDF <- subset(allDF, CaveOrHouse=="bat house")

## Visualize the measurements by bat house.
ggplot(bathouseDF, aes(x=Place, y=Mercury, color=coreID)) +
  geom_jitter(width=0.1) +
  ## geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))



## Make boxplots of concentrations by bat house.
init.fig.dimen(file="boxplot_by_bathousename.pdf", height=3.5, width=3.0,
               cex.axis=0.75, cex.lab=0.75, cex.main=0.75, cex.sub=0.75,
               mai=c(1.1, 0.5, 0.1, 0.1), tcl=-0.2)
## Save the parameters of the boxplot as you plot it.
paramsPlot <- boxplot(Mercury ~ Place, data=bathouseDF,
        xlab=NA, ylab="Mercury concentration (ppm)",
        col=myColors["bat house"], outcex=0.6, xaxt="n",
        ylim=c(0, 1))
## Put on reference lines.
abline(h=seq(0, 2, by=0.25), col="lightgray", lty=3)
## The cave names are so long that the x-axis has to be done
## separately, with carriage returns in the naming strings.
paramsPlot$names  ## See the names
houseNmPlot <- c("Lower Suwannee \nNWR Bat House",
               "UF Gainesville \nBat House")
axis(1, las=2, at=1:length(paramsPlot$names), labels=houseNmPlot)
## Re-plot the boxes so that they appear on top of the reference lines.
paramsPlot <- boxplot(Mercury ~ Place, data=bathouseDF,
        xlab=NA, ylab="Mercury concentration (ppm)",
        col=myColors["bat house"], outcex=0.6, xaxt="n",
        ylim=c(0, 1), add=TRUE)
## Add sample size for each box.
text(1:length(paramsPlot$n), 0.93, paste("n =", paramsPlot$n), cex=0.7)
dev.off()
rm(paramsPlot, houseNmPlot)



t.test(Mercury ~ Place, data=bathouseDF)
## p-value = 0.001358
## CI for Suwanee Bat House - UF Gainesville Bat House
## (0.2248231, 0.5059352)


## What about using the Wilcoxon-Mann-Whitney test, since the samples
## here are small?
library("coin")
wilcox_test(Mercury ~ as.factor(Place), data=bathouseDF, conf.level=0.95, distribution="exact")
## p-value = 0.0003232
## For asymptotic version of the test:
## wilcox_test(Mercury ~ as.factor(Place), data=bathouseDF, conf.level=0.95)
## p-value = 0.001565

rm(bathouseDF)
## #############################################



## #############################################
## Make boxplots of differences in organic matter between bat houses
## and caves.

ggplot(allDF, aes(x=OM, y=Mercury, color=Region)) +
    geom_point(size=2) +
    facet_wrap(~CaveOrHouse)
ggsave(file="Mercury_vs_OM_by_loctype.pdf", dev="pdf")

## #############################################





## #############################################
ggplot(allDF, aes(x=Place, y=Mercury, color=coreID)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  facet_wrap(~CaveOrHouse)

fullLM <- lm(Mercury ~ CaveOrHouse + Place, data=allDF)
## #############################################




## #############################################
## Try fitting a relationship between Mercury and OM, accounting for
## region.

ggplot(allDF, aes(x=OM, y=Mercury, color=Region)) +
    geom_point(size=2) +
    facet_wrap(~CaveOrHouse)

boxplot(OM ~ CaveOrHouse, data=allDF)

regLM <- lm(Mercury ~ OM + I(OM^2), data=subset(allDF, CaveOrHouse=="cave"))

sqrtLM <- lm(sqrt(Mercury) ~ + OM, data=subset(allDF, CaveOrHouse=="cave"))
logLM <- lm(log(Mercury) ~ + OM, data=subset(allDF, CaveOrHouse=="cave"))
with(subset(allDF, CaveOrHouse=="cave"), plot(OM, Mercury))

## #############################################


## #############################################
## Use trellis graphics to visualize the relationship
## between mercury concentration and organic matter.

library("lattice")

xyplot(OM ~ Mercury | as.factor(Region), data=allDF,
       group = CaveOrHouse, pch=1:2, col=1:2,
       key = list(space="right",
                  points = list(pch=1:2, col=1:2),
                  text = list(c("cave", "bat house"))
                  )
       )
my.lm <- lm(OM ~ Mercury + I(Mercury^2), data=allDF)


xyplot(Mercury ~ OM | as.factor(Region), data=allDF,
       group = CaveOrHouse, pch=1:2, col=1:2,
       key = list(space="right",
                  points = list(pch=1:2, col=1:2),
                  text = list(c("cave", "bat house"))
                  )
)
## #############################################




