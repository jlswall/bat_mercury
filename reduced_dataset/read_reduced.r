## #############################################
## Read in the data from the Excel file.
library("openxlsx")
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
housesDF$CaveOrHouse <- "house"
allDF <- rbind(cavesDF, housesDF)
rm(cavesDF, housesDF)
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

## Initialize coreID and coreOrder variables:
allDF$coreID <- "not core"
allDF$coreOrder <- NA


## For Climax Cave's core:
allDF$coreID[11:20] <- "ClimaxCore"
allDF$coreOrder[11:20] <- 1:10

## For Cottondale's core:
allDF$coreID[27:32] <- "CottondaleCore"
allDF$coreOrder[27:32] <- 1:6

## For Florida Caverns Old Indian Cave, there appear to be 2 8-inchcores:
allDF$coreID[33:40] <- "OldIndianCore1"
allDF$coreOrder[33:40] <- 1:8
allDF$coreID[41:48] <- "OldIndianCore2"
allDF$coreOrder[41:48] <- 1:8

## For Judge's Cave, there are 1 11-inch core and 1 8-inch core:
allDF$coreID[66:76] <- "JudgesCore1"
allDF$coreOrder[66:76] <- 1:11
allDF$coreID[77:84] <- "JudgesCore2"
allDF$coreOrder[77:84] <- 1:8


## For UF Gainseville Bat House, there are also 2 cores:
allDF$coreID[101:106] <- "UFCore1"
allDF$coreOrder[101:106] <- 1:6
allDF$coreID[107:112] <- "UFCore2"
allDF$coreOrder[107:112] <- 1:6
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



## #############################################
## Use trellis graphics to visualize the relationship
## between mercury concentration and organic matter.

library("lattice")
histogram(~Mercury | as.factor(Region),
          data=subset(allDF, CaveOrHouse="cave"))
## #############################################




## #############################################
## Make a plot showing the mercury concentration vs. position
## within the core (1 is at the surface).

## Subset to rows that represent core measurements.
coreDF <- subset(cavesDF, !is.na(coreID))
with(coreDF, plot(coreOrder, Mercury, type="n"))
namesCores <- unique(coreDF$coreID)
for (i in 1:length(namesCores)){
  subDF <- subset(coreDF, coreID==namesCores[i])
  with(subDF, lines(coreOrder, Mercury, col=subDF$Region))
}
rm(i, subDF)
## Fit a linear model of mercury vs. core position.
full.lm <- lm(Mercury ~ coreOrder*as.factor(Region), data=coreDF)
med.lm <- lm(Mercury ~ coreOrder + as.factor(Region), data=coreDF)
simple.lm <- lm(Mercury ~ coreOrder, data=coreDF)
rm(namesCores)
## #############################################



Take an exploratory look at the relationship between mercury concentration and orgnaic matter.
```{r, fig.height=3.5}
with(cavesDF, plot(OM, Mercury, ylab="Hg conc", xlab="organic matter", type="n"))
mycolors <- c("black", "blue", "darkorange")
mysymbols <- c(1, 15, 6)
for (i in 1:3){
  subDF <- subset(cavesDF, Region==i)
  with(subDF, points(OM, Mercury, pch=mysymbols[i], col=mycolors[i]))
}
rm(i)
```


Boxplots of mercury concentrations vs. region number, with sample sizes above each boxplot:
```{r, fig.height=3.5}
ctsByRegion <- table(toplayerDF$Region)
boxplot(MercuryConc ~ Region, data=toplayerDF, xlab="Region", ylab="Hg conc.")
for (iNm in names(ctsByRegion)){
  text(as.numeric(iNm), 1.9, ctsByRegion[iNm], col="blue", cex=0.6)
}
```

These boxplots indicate a wide variability in the top layer mercury concentrations among the various regions.  The histograms of mercury concentrations also show that the shapes of the distributions aren't normal.  In fact, the distributions seem to be quite different from one another.  I tried the log and square root transformations (not shown), but they didn't adequately address these issues.
```{r, fig.height=6.0}
par(mfrow=c(2,2))
my.xlim <- c(0, max(toplayerDF$MercuryConc) + 0.25)
for (i in unique(toplayerDF$Region)){
  subDF <- subset(toplayerDF, Region==i)
  hist(subDF$MercuryConc, xlim=my.xlim, main=paste("Region", i, sep=""))
}
rm(subDF, my.xlim, i)

par(mfrow=c(1,1))
```

It seems like we might want to test the hypothesis that the average mercury concentrations are equal for regions 1-3.  To do this, we'll have to choose a nonparameteric approach.  Here, I've written some code to do a permutation test that is based on the sum of squares for the treatment effect; this is similar to the strategy used in a typical ANOVA approach, but without the distributional assumptions.
```{r}
permute1WayAnova <- function(x, grp, numPermutations = 1000){

    ## Calculate overall mean, which is the same, regardless of
    ## what groups the observations are in.
    overallMean <- mean(x)
    ## The number of observations per group also stays the same.
    grpN <- table(grp)
    
    ## Calculate the test stat for the original grouping.
    origSSTr <- calcSSTrt(x, grp, overallMean, grpN)
  
    permuteSSTr <- NULL
    ## Permute the order of the data and recalculate the test stat.
    for (i in 1:numPermutations){
        permuteX <- sample(x, size=length(x), replace=FALSE)
        permuteSSTr <- c(permuteSSTr,
                         calcSSTrt(permuteX, grp, overallMean, grpN))
    }
  
    ## Approx. p-value by calculating what percentage of statistics from
    ## the permutations exceed this test statistic calculated from the
    ## original data.
    approxPval <- sum(permuteSSTr >= origSSTr)/numPermutations
    return(list(approxPval=approxPval, origSSTr=origSSTr, permuteSSTr=permuteSSTr))
}

calcSSTrt <- function(x, grp, overallMean, grpN){

    ## Calculate group means.
    grpMeans <- tapply(x, grp, mean)
    ## Calculate sum of squares associated with the treatment groups.
    sstrt <- sum( grpN * ( (grpMeans - overallMean)^2 ) )
    
    return(sstrt)
}

## Run this permutation test on our data.
testRes <- permute1WayAnova(toplayerDF$MercuryConc, toplayerDF$Region, numPermutations=20000)
hist(testRes[["permuteSSTr"]], prob=T)
abline(v=testRes[["origSSTr"]], col="red")
print(testRes[["approxPval"]])
```
This gives p-value just about the 5% level.  I also got a similar result from a canned routine in the R package "coin".  The code is below.
```{r}
library("coin")
independence_test(MercuryConc ~ as.factor(Region), data=toplayerDF, teststat="quadratic", distribution="approximate")
independence_test(MercuryConc ~ as.factor(Region), data=toplayerDF, teststat="quadratic", distribution="asymptotic")
```

In the following plot, I tried to look at the measurements that were taken at intervals along the same core.  On the x-axis, I have the order the measurements were taken from the core (assuming again that 1 is the top 1 inch, 2 is the concentration from the next inch down, etc.).  I don't see a clear relationship.  **Do you think I'm intrepreting this correctly?**
```{r, fig.height=3.5}
## Find all rows with "intervals" in the Notes section.
subDF <- cavesDF[!is.na(cavesDF$coreName),]
## Divide into groups according to which core the measurements came from.
splitDF <- split(subDF, subDF$coreName)
my.col <- c("black", "blue", "orange", "magenta", "darkgreen", "cyan", "darkred", "gray")
y.rng <- range(subDF[,"MercuryConc"])
plot(c(1, max(subDF$coreOrder)), y.rng, type="n", xlab="One inch increments", ylab="Hg conc.")
legLabels <- NULL
for (i in 1:length(splitDF)){
  with(splitDF[[i]], lines(coreOrder, MercuryConc, col=my.col[i]))
  legLabels <- c(legLabels, names(splitDF)[i])
}
legend("bottomright", legend=legLabels, lty=1, col=my.col, cex=0.4)
```


I also make a plot of the mercury concentrations vs. the organic matter.  I thought these might be related, but I don't have a good understanding of what kind of measurement is represented by the organic matter column.  Should there be a relationship?
```{r, fig.height=4.0}
par(mfrow=c(1,1))
plot(MercuryConc ~ OrganicMatter, data=cavesDF, xlab="Org. matter", ylab="Hg conc.", type="n")
my.col = c("black", "blue", "orange", "magenta")
my.pch = c(1, 16, 15, 17)
for (i in 1:4){
  subDF <- subset(cavesDF, Region==i)
  points(MercuryConc ~ OrganicMatter, data=subDF, col=my.col[i], pch=my.pch[i])
}
legend("topleft", legend=1:4, cex=0.7, pch=my.pch, col=my.col)
```

