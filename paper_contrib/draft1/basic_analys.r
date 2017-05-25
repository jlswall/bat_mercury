## ###########################################################
## - READ IN DATA FROM EXCEL SPREADSHEET.
## - MARK WHICH OBSERVATIONS ARE FROM CORES (AND THE ORDER
##   SUCH MEASUREMENTS WERE TAKEN FROM CORES).
## - MARK WHICH OBSERVATIONS CAME FROM BAT HOUSES.  (I've
##   placed the observation from the I-75 underpass with those
##   from the bat houses, because it was populated by "TABR"
##   species, like some of the bat houses.)


## I saved the first worksheet ("All guano Data") from the Excel
## workbook (Hg_Guano Only.xlsx) into a separate file. Here,
## read in this data.
library("openxlsx")
fileWpath = "../explore/all_guano_w_species.xlsx"
rawDF <- read.xlsx(xlsxFile = fileWpath, colNames=TRUE, rowNames=FALSE)
## Replace all strings "N/A" with "NA" (which R understands).
rawDF[rawDF=="N/A"] <- NA



## Some of these observations are taken in 1 inch segments from the
## same core; we identify these observations based on the entries in
## the "SampleID" and "Notes" columns, and we give these more readable
## names.  The code also introduces a column which contains the order
## in which the samples were taken from each core (or NA if not from a
## core).  In general, notation such as CLM C1 means that this is the
## top one inch from the core (i.e., this is the 1 inch segment that
## was most recently deposited).  CLM C2 is then the next inch down,
## etc.
idPrefix <- c("CLM C", "COT C1 ", "FCCI C1 ", "FCCI C2 ", "JUD C1 ",
              "JUD C2 ", "UFBH C1-", "UFBH C2-")
names(idPrefix) <- c("GSS 36 core", "FCS 872 core", "FCS 555 core 1",
                     "FCS 555 core 2", "FCS 556 core 1", "FCS 556 core 2",
                     "UFBH core 1", "UFBH core 2")
## Start with the raw data frame we read in, and add 2 more columns.
workDF <- rawDF
workDF[,"coreName"] <- "not from core"
workDF[,"coreOrder"] <- NA
for (i in 1:nrow(workDF)){
  for (j in 1:length(idPrefix)){
    ## Use 1:20, because we have at most 11 samples from a single core.
    which.match <- which(paste0(idPrefix[j], 1:20) == workDF[i, "SampleID"])
    if (length(which.match) == 1){ #Match was found
      workDF[i, "coreName"] <- names(idPrefix[j])
      workDF[i, "coreOrder"] <- which.match
    }
  }
}
rm(i, j, which.match, idPrefix)



## The bat houses are Suwannee NWR Bat House, UF Gainesville
## Bat House, and I-75 underpass.
namesBatHouses <- c("Suwannee NWR Bat House",
                    "UF Gainesville Bat House",
                    "Interstate 75 underpass")
## By default, start by assuming all locations are caves.
workDF[,"loctype"] <- "cave"
## Mark the bat houses/underpass differently.
workDF[workDF$CaveOrHouse %in% namesBatHouses, "loctype"] <- "batHouse"
workDF[,"loctype"] <- as.factor(workDF[,"loctype"])



## Remove the 2 concentrations for region 4; they aren't sufficient to
## draw conclusions about that region.
excl4DF <- subset(workDF, Region!=4)
## Treat region as a factor for later analyses (otherwise, the numbers
## will be treated as though they are quantitative measurements).
excl4DF[,"Region"] <- as.factor(excl4DF[,"Region"])



## ##########################
## CREATE SEVERAL DIFFERENT VERSIONS OF THE DATASET (all without
## region 4).

## Create an additional version of this dataset, with the
## outliers removed.
noExtrDF <- subset(excl4DF, MercuryConc < 1.2)

## Here's an additional version, with just top layer measurements
## included.
toplayerDF <- subset(excl4DF, (coreName=="not from core") | (coreOrder==1))

## Build dataset with core measurements averaged.
library("dplyr")
avgdCoreDF <- as.data.frame(excl4DF %>%
    group_by(Region, loctype, CaveOrHouse, ObsLocID) %>%
    select(Region, loctype, CaveOrHouse, ObsLocID, MercuryConc) %>%
    summarize(
        avgConc = mean(MercuryConc)
    )
)
detach("package:dplyr")
## ##########################
## ###########################################################



## ###########################################################
## Boxplot of data by region.

library("figdim")
init.fig.dimen(file="all_hg_by_reg.pdf", width=3.0, height=4.5)
## Boxplots for combined data, plus broken down by region.
with(workDF,
     plot(c(1.0, 6.5), c(0, max(MercuryConc)), type="n", xaxt="n", yaxt="n",
          xlab="Region", ylab="Mercury concentration (ppm)",
          cex.lab=0.8)
     )
## Make boxplot for all regions combined.
boxplot(workDF[,"MercuryConc"], at=1.5, add=TRUE, col="gray", axes=F) ## width=0.8)
text(1.5, 1.05, paste0("n=", nrow(workDF)), cex=0.7, col=gray(0.4))
## Break down by region, plot each region separately.
for (i in 1:4){
  subDF <- subset(workDF, as.numeric(Region)==i)
  with(subDF, boxplot(MercuryConc, at=2+i, add=TRUE, axes=F)) ## width=rep(0.8, 4)))
  text(2+i, 1.05, paste0("n=", nrow(subDF)), cex=0.7, col=gray(0.4))
}
axis(1, at=c(1.5, 2+c(1:4)), labels=c("Combined", c(1:4)),
     cex.lab=0.8, cex.axis=0.8)
axis(2, cex.lab=0.8, cex.axis=0.8)
dev.off()

rm(subDF, i)
## ###########################################################




## ###########################################################
## TASK 1: Find the overall mean.
## a. For both caves/bat houses and
## b. For just caves, for comparison with a previous study

## ##########################
## a. For both caves and bat houses.

## Using all data.
t.test(workDF[,"MercuryConc"])  ## 95% CI: (0.4966267, 0.5874153)
## The above answer is almost the same if you exclude region 4 data:
## t.test(excl4DF[,"MercuryConc"])

## Excluding outliers.
t.test(subset(workDF, MercuryConc < 1.2, "MercuryConc"))
## 95% CI: (0.4827059, 0.5449289)
## The above answer is almost the same if you exclude region 4 data:
## t.test(noExtrDF[,"MercuryConc"])
## ##########################


## ##########################
## For only caves:

## Using data from all regions, outliers intact:
t.test(subset(workDF, loctype=="cave", "MercuryConc"))
## 95% CI: (0.5117379, 0.6120298)
## The above answer is almost the same if you exclude region 4 data:
t.test(subset(excl4DF, loctype=="cave", "MercuryConc"))

## Excluding outliers.
t.test(subset(workDF, (loctype=="cave") & (MercuryConc < 1.2), "MercuryConc"))
## 95% CI: (0.4969033, 0.5618405)
## The above answer is similar if you exclude region 4 data:
t.test(subset(noExtrDF, loctype=="cave", "MercuryConc"))
## ##########################
## ###########################################################




## ###########################################################
## Task 2: Determine if there's a difference in the mean between
## regions.  Use only caves (not bat houses), because the species of
## bats inhabiting these locations are different and because there are
## no bat houses for comparison outside of region 3.


## #####################################
## Permutation test for differences in regional means, using
## - only caves (not bat houses) and
## - using **all core measurements**


## Exclude the bat houses (and I-75 underpass), due to the differences
## in species inhabiting these areas (not to mention the habitat
## difference).
cavesOnlyDF <- subset(excl4DF, loctype=="cave")


## Find the number of caves per region.  This will be preserved
## in the permutations later.
library("dplyr")
nCavesPerRegTmp <- as.data.frame(cavesOnlyDF %>%
                                 distinct(Region, CaveOrHouse) %>%
                                 group_by(Region) %>%
                                 summarize( count = n() )
                                 )
nCavesPerReg <- nCavesPerRegTmp[,"count"]
names(nCavesPerReg) <- nCavesPerRegTmp[,"Region"]
detach("package:dplyr")


## Find the test statistic for the original data.
orig.aov <- aov(MercuryConc ~ as.factor(Region) + as.factor(CaveOrHouse), data=cavesOnlyDF)
orig.anova <- anova(orig.aov)
origMSEratio <- orig.anova[["Mean Sq"]][1] / orig.anova[["Mean Sq"]][2]

## Make a list of caves to be permuted among regions 1-3.
caveVec <- unique(cavesOnlyDF[,"CaveOrHouse"])
## ##########
## Find data frame of the 792 (12!/(7!5!)) ways to choose 7 (number of
## caves in regions 1 and 2) from all 12 caves.
combo1Mat <- t(combn(caveVec, nCavesPerReg["1"]+nCavesPerReg["2"]))
rm(caveVec)

## Step through these combinations, and break them down a bit further.
permutedMSEratio <- NULL
for (i in 1:nrow(combo1Mat)){

  ## Take the 7 caves in combination i, and find the 21 (7!/(5!2!))
  ## ways to choose 5 caves out of these 7.
  combo2Mat <- t(combn(combo1Mat[i,], 5))

  ## Loop through these combinations.
  for (j in 1:nrow(combo2Mat)){

    ## Build temporary data frame and assigned permuted versions of
    ## the region label according to row j of combo2Mat.
    tmpDF <- cavesOnlyDF[,c("MercuryConc", "CaveOrHouse")]
    ## At the beginning assume all caves are in region 3.
    tmpDF[,"permuteReg"] <- 3
    ## Now assign region 2 label to all 7 caves (in combo1Mat)
    ## that were chosen in our first listing of combinations.
    tmpDF[tmpDF$CaveOrHouse %in% combo1Mat[i,], "permuteReg"] <- 2
    ## Lastly, assign region 1 label to all 5 caves that were
    ## chosen in our second list (ways to choose 5 caves out of 7).
    tmpDF[tmpDF$CaveOrHouse %in% combo2Mat[j,], "permuteReg"] <- 1
      
    ## Use ANOVA to compute the sums of squares we need using caves
    ## permuted to different regions.
    permute.anova <- anova(aov(MercuryConc ~ as.factor(permuteReg) + as.factor(CaveOrHouse), data=tmpDF))
    permutedMSEratio <- c(permutedMSEratio, permute.anova[["Mean Sq"]][1] / permute.anova[["Mean Sq"]][2])
  }
}
rm(i, j, combo1Mat, combo2Mat, tmpDF, permute.anova)

## Get the approximate p-value by comparing the original MSE ratio the
## various ratios calculated from the permutations.
sum(permutedMSEratio > origMSEratio)/length(permutedMSEratio)
## #####################################




## #####################################
## Permutation test for differences in regional means, using
## - only caves (not bat houses) and
## - using **top layer measurements only**


## Exclude the bat houses (and I-75 underpass), due to the differences
## in species inhabiting these areas (not to mention the habitat
## difference).
topOnlyDF <- subset(toplayerDF, loctype=="cave")


## Find the number of caves per region.  This will be preserved
## in the permutations later.
library("dplyr")
nCavesPerRegTmp <- as.data.frame(topOnlyDF %>%
                                 distinct(Region, CaveOrHouse) %>%
                                 group_by(Region) %>%
                                 summarize( count = n() )
                                 )
nCavesPerReg <- nCavesPerRegTmp[,"count"]
names(nCavesPerReg) <- nCavesPerRegTmp[,"Region"]
detach("package:dplyr")


## Find the test statistic for the original data.
orig.aov <- aov(MercuryConc ~ as.factor(Region) + as.factor(CaveOrHouse), data=topOnlyDF)
orig.anova <- anova(orig.aov)
origMSEratio <- orig.anova[["Mean Sq"]][1] / orig.anova[["Mean Sq"]][2]

## Make a list of caves to be permuted among regions 1-3.
caveVec <- unique(topOnlyDF[,"CaveOrHouse"])
## ##########
## Find data frame of the 792 (12!/(7!5!)) ways to choose 7 (number of
## caves in regions 1 and 2) from all 12 caves.
combo1Mat <- t(combn(caveVec, nCavesPerReg["1"]+nCavesPerReg["2"]))
rm(caveVec)

## Step through these combinations, and break them down a bit further.
permutedMSEratio <- NULL
for (i in 1:nrow(combo1Mat)){

  ## Take the 7 caves in combination i, and find the 21 (7!/(5!2!))
  ## ways to choose 5 caves out of these 7.
  combo2Mat <- t(combn(combo1Mat[i,], 5))

  ## Loop through these combinations.
  for (j in 1:nrow(combo2Mat)){

    ## Build temporary data frame and assigned permuted versions of
    ## the region label according to row j of combo2Mat.
    tmpDF <- topOnlyDF[,c("MercuryConc", "CaveOrHouse")]
    ## At the beginning assume all caves are in region 3.
    tmpDF[,"permuteReg"] <- 3
    ## Now assign region 2 label to all 7 caves (in combo1Mat)
    ## that were chosen in our first listing of combinations.
    tmpDF[tmpDF$CaveOrHouse %in% combo1Mat[i,], "permuteReg"] <- 2
    ## Lastly, assign region 1 label to all 5 caves that were
    ## chosen in our second list (ways to choose 5 caves out of 7).
    tmpDF[tmpDF$CaveOrHouse %in% combo2Mat[j,], "permuteReg"] <- 1
      
    ## Use ANOVA to compute the sums of squares we need using caves
    ## permuted to different regions.
    permute.anova <- anova(aov(MercuryConc ~ as.factor(permuteReg) + as.factor(CaveOrHouse), data=tmpDF))
    permutedMSEratio <- c(permutedMSEratio, permute.anova[["Mean Sq"]][1] / permute.anova[["Mean Sq"]][2])
  }
}
rm(i, j, combo1Mat, combo2Mat, tmpDF, permute.anova)

## Get the approximate p-value by comparing the original MSE ratio the
## various ratios calculated from the permutations.
sum(permutedMSEratio > origMSEratio)/length(permutedMSEratio)
## #####################################



## #####################################
## Try to use a mixed effects model.  These models don't fit very
## well, but they do seem to confirm that there is a significant
## difference in the regional means.

library("lme4")

## When we fit a mixed model without region 4, but with the outliers,
## our residuals don't really follow a normal distribution.  The
## variance in the residuals is very different from region to region,
## with some distinct outliers.
fullmixedMod <- lmer(MercuryConc ~ as.factor(Region) + as.factor(loctype) + (1|CaveOrHouse) + (1|ObsLocID), data=excl4DF)
boxplot(residuals(fullmixedMod) ~ as.factor(excl4DF$Region))

## When the bat houses are removed, we still have problems with the residuals.
onlyCavesMixedMod <- lmer(MercuryConc ~ as.factor(Region) + (1|CaveOrHouse) + (1|ObsLocID), data=cavesOnlyDF)
boxplot(residuals(onlyCavesMixedMod) ~ as.factor(cavesOnlyDF$Region))

rm(fullmixedMod, onlyCavesMixedMod)
## #####################################
## ###########################################################




## ###########################################################
## Permutation test for differences between caves and bat houses (and
## overpasses).
## - only region 3 (other regions have no bat houses)
## - using **all core measurements**


## Limit to just region 3, which is the only region with any bat
## houses (and I-75 underpass).
reg3DF <- subset(excl4DF, Region==3)


## ##############################
## Show boxplots for caves vs. bat houses in region 3.

## Create a temporary data frame with a new column with printable
## names "bat houses" vs. "caves".
tmpDF <- reg3DF
tmpDF[,"loctypename"] <- ifelse(tmpDF$loctype=="cave", "caves", "bat houses/overpass")
library("figdim")
init.fig.dimen(file="caves_vs_bathouses.pdf", width=3.0, height=4.5)
## Boxplots for combined data, plus broken down by region.
with(tmpDF, boxplot(MercuryConc ~ loctypename, xlab=NA, ylab="Mercury concentration (ppm)", cex.lab=0.8, cex.axis=0.8))
text(c(1, 2), rep(1, 2), paste0("n=", table(tmpDF[,"loctypename"])), cex=0.7, col=gray(0.4))
dev.off()

rm(tmpDF)
## ##############################


## ##############################
## Find the number of caves per location type (in terms of caves
## vs. bat houses).  This will be preserved in the permutations later.

library("dplyr")
nCavesPerLocTypeTmp <- as.data.frame(reg3DF %>%
                              distinct(loctype, CaveOrHouse) %>%
                              group_by(loctype) %>%
                              summarize( count = n() )
                              )
nCavesPerLocType <- nCavesPerLocTypeTmp[,"count"]
names(nCavesPerLocType) <- nCavesPerLocTypeTmp[,"loctype"]
rm(nCavesPerLocTypeTmp)
detach("package:dplyr")


## Find the test statistic for the original data.
orig.aov <- aov(MercuryConc ~ as.factor(loctype) + as.factor(CaveOrHouse), data=reg3DF)
orig.anova <- anova(orig.aov)
origMSEratio <- orig.anova[["Mean Sq"]][1] / orig.anova[["Mean Sq"]][2]

## Make a list of caves to be permuted.
caveVec <- unique(reg3DF[,"CaveOrHouse"])
## Find data frame of all 56 (=8!/(5!3!)) combinations.  Transpose it
## so that rows represent each combination.  There are then 3 columns
## for each row, which represent the 3 caves that should be assigned
## to the bat house category.
comboMat <- t(combn(caveVec, nCavesPerLocType["batHouse"]))
rm(caveVec)

## Step through each possible combination and recalculate the test
## statistic.
permutedMSEratio <- NULL
for (i in 1:nrow(comboMat)){
  ## Build temporary data frame and assign permuted versions of
  ## the batHouse/cave label according to row i of comboMat.
  tmpDF <- reg3DF[,c("MercuryConc", "CaveOrHouse")]
  ## At the beginning, assume all locations are caves.
  tmpDF[,"permuteLocType"] <- "cave"
  ## Now, assign label of"batHouse" to the caves in row i.
  tmpDF[tmpDF$CaveOrHouse %in% comboMat[i,], "permuteLocType"] <- "batHouse"
  
  ## Use ANOVA to compute the sums of squares we need using caves
  ## permuted to different regions.
  permute.anova <- anova(aov(MercuryConc ~ as.factor(permuteLocType) + as.factor(CaveOrHouse), data=tmpDF))
  permutedMSEratio <- c(permutedMSEratio, permute.anova[["Mean Sq"]][1] / permute.anova[["Mean Sq"]][2])
}
## Find the p-value.
sum(permutedMSEratio > origMSEratio)/length(permutedMSEratio)

rm(i, tmpDF, permute.anova, comboMat)
## ##############################

## ###########################################################








## We test the hypothesis that the average Hg concentrations are equal
## for regions 1-3.  To do this, we'll have to choose a nonparameteric
## approach, probably using a permutation procedure.
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




## ###########################################################
## Try a mixed effects model.
library("lme4")

## These 2 models use all the core samples.
reducedM <- lmer(MercuryConc ~ Region + loctype + (1 | CaveOrHouse), data=noExtrDF)
fullM <- lmer(MercuryConc ~ Region + loctype + (1 | CaveOrHouse) + (1 | ObsLocID), data=noExtrDF)


## The next model only uses the top layer (not the whole core).
toplayerDF <- subset( noExtrDF, (is.na(coreOrder)) | (coreOrder==1) )
noCoreM <- lmer(MercuryConc ~ Region + loctype + (1 | CaveOrHouse), data=toplayerDF)
## ###########################################################



## ###########################################################
## First try an analysis of variance model.

## Fit a model accounting for region and for cave or house.
almostFull.aov <- aov(MercuryConc ~ Region + loctype + loctype/CaveOrHouse, data=noExtrDF)
## ###########################################################




## ###########################################################
## We look at boxplots by region and broken down by core name (or
## noncore status).

## Figure out how many combos of cores and not core exist for each
## region.
ctCombosMat <- with(excl4DF, table(Region, coreName))
numCombos <- sum( as.vector( ctCombosMat > 0 ) )


library("figdim")

my.colors <- c("cyan", "darkgreen", "orange", "magenta")
init.fig.dimen(file="boxplots_regions_cores.pdf", width=4, height=7,
               mai=c(0.85, 0.3, 0.05, 0.1))
plot(c(1, nrow(ctCombosMat)-1 + numCombos),
     range(excl4DF[,"MercuryConc"]), type="n", xaxt="n",
     xlab=NA, ylab="Hg conc.")

xLabels <- NULL
xCoords <- NULL
for (i in 1:nrow(ctCombosMat)){

  if (i==1)
    tmpxcoords <- seq(1, sum(as.vector(ctCombosMat[1,] > 0)))
  else
    tmpxcoords <- seq(nextGrpX, nextGrpX+sum(as.vector(ctCombosMat[i,] > 0))-1)

  ## Subset to region i.
  tmpDF <- subset(excl4DF, Region==i)
  boxplot(MercuryConc ~ coreName, data=tmpDF, at=tmpxcoords,
          names=NA, add=TRUE, tcl=-0.01, col=my.colors[i])

  ## Counts for each core name for this region.
  coreCts <- tapply(tmpDF[,"MercuryConc"], tmpDF[,"coreName"], length)
  ## Put these counts well above the "box" part of each boxplot.
  boxtops <- tapply(tmpDF[,"MercuryConc"], tmpDF[,"coreName"], quantile, 0.9)
  text(tmpxcoords, boxtops, coreCts, cex=1.1, col="blue")

  ## Save the coords and labels for drawing the x-axis at the end.
  xCoords <- c(xCoords, tmpxcoords)
  xLabels <- c(xLabels, names(coreCts))
  ## Where on x-axis to plot beginning of next group.
  nextGrpX <- max(tmpxcoords) + 2
}
axis(1, at=xCoords, labels=xLabels, las=2, cex.axis=0.6, tcl=-0.01)
dev.off()

rm(i, nextGrpX, tmpxcoords, xCoords, xLabels, tmpDF, coreCts, boxtops)
## ###########################################################




## ###########################################################
## We look at boxplots by cave/bat house and broken down by
## core name (or noncore status).

library("figdim")
library("ggplot2")

## To keep the same y-range for all plots, we need to find the
## maximum possible range of Hg concnentrations.
rngConc <- range(excl4DF[,"MercuryConc"])


## init.fig.dimen(file="boxplots_caves_cores.pdf", width=4, height=7,
##                mai=c(0.85, 0.3, 0.05, 0.1))

for (i in 1:4){

  tmpDF <- subset(excl4DF, Region==i)
  ggplot(tmpDF, aes(x=coreName, y=MercuryConc)) +
    stat_boxplot() + 
    facet_wrap(~ CaveOrHouse) +
    ## coord_cartesian(ylim=rngConc) +
    ggtitle(paste0("Region ", i)) +
    labs(x=NULL, y="Mercury conc.") +
    theme(axis.text.x=element_text(angle=-90, hjust=0))

  ggsave(file=paste0("region", i, "_boxplots_caves_cores.pdf"))
}
rm(i, tmpDF, rngConc)



## ############################
## Try to put sample size on.
give.n <- function(x){
   return(c(y = mean(x), label = length(x)))
}

for (i in 1:4){

  tmpDF <- subset(excl4DF, Region==i)
  ggplot(tmpDF, aes(x=coreName, y=MercuryConc)) +
    stat_boxplot() +
    stat_summary(fun.data = give.n, geom="text") +
    facet_wrap(~ CaveOrHouse) +
    ## coord_cartesian(ylim=rngConc) +
    ggtitle(paste0("Region ", i)) +
    labs(x=NULL, y="Mercury conc.") +
    theme(axis.text.x=element_text(angle=-90, hjust=0))

  ggsave(file=paste0("region", i, "_boxplots_withn_caves_cores.pdf"))
}
rm(i, tmpDF, rngConc)

## ############################
## ###########################################################




## ###########################################################
## MAKE BOXPLOTS FOR EACH CAVE/BAT HOUSE WHICH HAS A CORE.

my.colors <- c("cyan", "darkgreen", "orange", "magenta")

## Find names of all caves/bat houses which have cores.
wCoresDF <- subset(excl4DF, coreName != "not from core")
caveNames <- unique(wCoresDF[,"CaveOrHouse"])

for (iCave in caveNames){

  init.fig.dimen(file=paste0("boxplots_", iCave, ".pdf"),
                 width=2, height=5,
                 mai=c(0.8, 0.2, 0.15, 0.1))

  pdf(file=paste0("boxplots_", iCave, ".pdf"))

  tmpDF <- subset(excl4DF, CaveOrHouse==iCave)
  tmpRegion <- unique(tmpDF[,"Region"])
  boxplot(MercuryConc ~ coreName, data=tmpDF, ylab="Hg conc.",
          col=my.colors[tmpRegion], las=2, cex.axis=0.7)
  title(main=iCave)

  ## Counts for each core name for this region.
  coreCts <- tapply(tmpDF[,"MercuryConc"], tmpDF[,"coreName"], length)
  ## Put these counts well above the "box" part of each boxplot.
  boxtops <- tapply(tmpDF[,"MercuryConc"], tmpDF[,"coreName"], quantile, 0.85)
  text(1:length(unique(tmpDF[,"coreName"])), boxtops, coreCts,
       cex=1.1, col="blue")

  dev.off()
}

rm(iCave, tmpDF, tmpRegion, wCoresDF, caveNames, coreCts, boxtops)
## ###########################################################




## ###########################################################
## Write a function to try to assess how different core measurements
## are from the non-core measurements.

## Take random samples of the same size as the core sample from the
## all the region's measurements.
testCoreCorr <- function(allDF, chosenCore, nReps=500){
  
  ## First, subset data frame to just those observations
  ## associated with the chosen core.
  coreDF <- subset(allDF, coreName==chosenCore)
  ## Find SD and sample size associated with the core sample.
  sdCore <- sd(coreDF[,"MercuryConc"])
  nCore <- nrow(coreDF)
  
  ## Figure out which region this is.
  chosenRegion <- unique(coreDF[,"Region"])
  if (length(chosenRegion) > 1)
    stop(paste0("Core is associated with ", length(chosenRegion),
                " regions."))
  ## Subset the data frame to observations from the chosen core and
  ## the non-core observations for this region.
  compareDF <- subset( allDF, (Region==chosenRegion) & (coreName %in% c(chosenCore, "not from core")) )


  ## Now, take random samples from this region using a sample
  ## of the same size as that of the chosen core.
  sdPermute <- NULL
  for (i in 1:nReps){
    
    ## Generate a random sample from the region's observations,
    ## including core and non-core observations.
    sampPermute <- sample(compareDF[,"MercuryConc"], size=nCore, replace=FALSE)
    sdPermute <- c(sdPermute, sd(sampPermute))
  }

  ## How extreme is the SD from the chosen core sample compared
  ## to the various combos of core and non-core observations?
  corepval <- sum(sdPermute >= sdCore)/length(sdPermute)

  return(list(p=corepval, sdCore=sdCore, sdPermute=sdPermute))
}




## Take random samples of the same size as the core sample from the
## all the region's measurements.
testCoreCorrIQR <- function(allDF, chosenCore, nReps=500){
  
  ## First, subset data frame to just those observations
  ## associated with the chosen cave.
  coreDF <- subset(allDF, coreName==chosenCore)
  ## Find IQR and sample size associated with the core sample.
  iqrCore <- IQR(coreDF[,"MercuryConc"])
  nCore <- nrow(coreDF)
  
  ## Figure out which cave this is, and make sure all rows for
  ## this core agree that they come from the same cave.
  chosenCave <- unique(coreDF[,"CaveOrHouse"])
  if (length(chosenCave) > 1)
    stop(paste0("Core is associated with ", length(chosenCave),
                " caves."))
  ## Make sure there are other measurements for this cave,
  ## besides those from the chosen core.
  if (length( unique( subset(allDF, CaveOrHouse==chosenCave)[,"coreName"] ) ) == 1)
    stop("For this cave/bat house, we don't have any other observations to compare with those from this core.")

  
  ## Subset the data frame to observations from the chosen core and
  ## the non-core observations for this region.
  compareDF <- subset( allDF, CaveOrHouse==chosenCave)


  ## Now, take random samples from this region using a sample
  ## of the same size as that of the chosen core.
  iqrPermute <- NULL
  for (i in 1:nReps){
    
    ## Generate a random sample from the region's observations,
    ## including core and non-core observations.
    sampPermute <- sample(compareDF[,"MercuryConc"], size=nCore, replace=TRUE)
    iqrPermute <- c(iqrPermute, IQR(sampPermute))
  }

  ## How extreme is the IQR from the chosen core sample compared
  ## to the various combos of core and non-core observations?
  corepval <- sum(iqrPermute >= iqrCore)/length(iqrPermute)

  return(list(p=corepval, iqrCore=iqrCore, iqrPermute=iqrPermute))
}

## ###########################################################




## ###########################################################
## There is reason to assume that the concentrations collected from
## a single core are not independent.  I'd like to try to compare Hg
## concentrations that are likely to have been deposited most recently,
## which are:
## (a) those that were not collected as part of a core
## or
## (b) those that were collected in the top 1 inch of the core.
## Also, we exclude region 4 because it only has 2 observations, which
## is not sufficient to compare with the other regions. 
toplayerDF <- subset( excl4DF, ( is.na(coreOrder) | coreOrder==1 ) & (Region!=4) )



## Boxplots of Hg concentrations vs. region number, with sample
## sizes above each boxplot:
ctsByRegion <- table(toplayerDF[,"Region"])
boxplot(MercuryConc ~ Region, data=toplayerDF, xlab="Region",
        ylab="Hg conc.")
for (iNm in names(ctsByRegion)){
  text(as.numeric(iNm), 1.9, ctsByRegion[iNm], col="blue", cex=0.6)
}
rm(iNm)



## These boxplots indicate a wide variability in the top layer Hg
## concentrations among the various regions.  The histograms of Hg
## concentrations also show that the shapes of the distributions aren't
## normal.  In fact, the distributions seem to be quite different from
## one another.  I tried the log and square root transformations (not
## shown), but they didn't adequately address these issues.
par(mfrow=c(2,2))
my.xlim <- c(0, max(toplayerDF$MercuryConc) + 0.25)
for (i in unique(toplayerDF$Region)){
  subDF <- subset(toplayerDF, Region==i)
  hist(subDF$MercuryConc, xlim=my.xlim, main=paste("Region", i, sep=""))
}
rm(subDF, my.xlim, i)



## Based on the above boxplots, it doesn't look like we meet the
## assumptions (normal distributions, equal variances) for traditional
## ANOVA methods.  However, we run the model anyway, to get a sense of
## what it indicates.
my.aov <- aov(MercuryConc ~ as.factor(Region), data=toplayerDF)
anova(my.aov)



## We test the hypothesis that the average Hg concentrations are equal
## for regions 1-3.  To do this, we'll have to choose a nonparameteric
## approach, probably using a permutation procedure.
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
testRes <- permute1WayAnova(toplayerDF$MercuryConc, toplayerDF$Region)
## ###############################################





## ###############################################

## We take a look at the measurements that were taken at intervals
## along the same core.  On the x-axis, I've ordered the measurements
## taken from the cores (assuming again that 1 is the top 1 inch,
## 2 is the concentration from the next inch down, etc.).  I don't
## see a clear relationship. 

## Find all rows with ordered measurements from core samples.
subDF <- excl4DF[!is.na(excl4DF$coreOrder),]
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
legend("bottomright", legend=legLabels, lty=1, col=my.col, cex=0.8)



## We look at Hg concentrations vs. the organic matter.  I thought
## these might be related, but I don't have a good understanding
## of what kind of measurement is represented by the organic matter
## column.  Should there be a relationship?
par(mfrow=c(1,1))
plot(MercuryConc ~ OrganicMatter, data=excl4DF, xlab="Org. matter", ylab="Hg conc.", type="n")
my.col = c("black", "blue", "orange", "magenta")
my.pch = c(1, 16, 15, 17)
for (i in 1:4){
  subDF <- subset(excl4DF, Region==i)
  points(MercuryConc ~ OrganicMatter, data=subDF, col=my.col[i], pch=my.pch[i])
}
legend("topleft", legend=1:4, cex=0.7, pch=my.pch, col=my.col)


