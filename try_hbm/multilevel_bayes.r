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
fileWpath = "../../explore/all_guano_w_species.xlsx"
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
## workDF[,"loctype"] <- as.factor(workDF[,"loctype"])



## Remove the 2 concentrations for region 4; they aren't sufficient to
## draw conclusions about that region.
excl4DF <- subset(workDF, Region!=4)
## Treat region as a factor for later analyses (otherwise, the numbers
## will be treated as though they are quantitative measurements).
## excl4DF[,"Region"] <- as.factor(excl4DF[,"Region"])



## ##########################
## CREATE SEVERAL DIFFERENT VERSIONS OF THE DATASET (all without
## region 4, which only had 2 concentrations).

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
## Scatterplots by concentration by cave, with separate plots for each
## region.

## Set the same range for all regions.
concRng <- range(excl4DF$MercuryConc)

## Symbols for caves is (default, symbol 1) open circle, for bat
## houses it is open triangle (symbol 2).
mySymbols <- c(1, 2)
names(mySymbols) <- c("cave", "batHouse")

## List of colors.
myColors <- c("black", "blue", "orange", "green", "magenta", "cyan", "gray")


## ##########################
par(mfrow=c(1,3))
## For each region reg.i:
for (reg.i in 1:3){

  reg1DF <- subset(excl4DF, Region==reg.i)
  reg1DF$loctype <- as.character(reg1DF$loctype)

  ## List of caves in the region.
  caveNms <- unique(reg1DF$CaveOrHouse)

  ## List of cores (or "not from core") in the region.
  coreNms <- unique(reg1DF$coreName)
  coreColors <- myColors[1:length(coreNms)]
  names(coreColors) <- coreNms


  plot(c(1, length(caveNms)), concRng, type="n", xlab="Cave/house", ylab="Hg conc")
  title(main=paste("Region", reg.i), line=1, cex.main=0.8)
  abline(h=seq(0.2, 2.0, by=0.2), col="lightgray", lty=2)
  ## For each cave:
  for (j in 1:length(caveNms)){
    caveDF <- subset(reg1DF, CaveOrHouse==caveNms[j], c("CaveOrHouse", "Region", "coreName", "coreOrder", "loctype", "MercuryConc"))
    for (k in 1:nrow(caveDF))
      points(jitter(j, 0.5), caveDF$MercuryConc[k], col=coreColors[caveDF$coreName[k]], pch=mySymbols[caveDF$loctype[k]])
  }
}
rm(caveNms, coreNms, caveDF, reg.i, j, k)
## ##########################
## ###########################################################




## ###########################################################
## NEWER MODEL: alphas modeled with a prior mean of beta.

## Remove bat houses, leaving only caves.
cavesOnlyDF <- subset(excl4DF, loctype=="cave")
## Sort the data by cave name.
caveOrderDF <- cavesOnlyDF[order(cavesOnlyDF$CaveOrHouse),]


## ##########################
## Build design matrices matching observations to caves and caves to
## regions.

## First, deal with caves.  Since there's no overall mean, we
## include an effect for each cave.
namesCaves <- unique(caveOrderDF$CaveOrHouse)
numCaves <- length(namesCaves)
caveDesignMat <- matrix(0, nrow=nrow(caveOrderDF), ncol=numCaves)
for (i in 1:numCaves)
  caveDesignMat[caveOrderDF$CaveOrHouse==namesCaves[i], i] <- 1


## Then, deal with matching caves to regions.
numRegions <- length(unique(caveOrderDF$Region))
matchCaveRegionMat <- matrix(0, nrow=numCaves, ncol=numRegions)
for (i in 1:numCaves){

  ## Subset to just the data for this cave.
  subDF <- subset(caveOrderDF, CaveOrHouse==namesCaves[i])

  ## Make sure that the dataset is consistent, so that only one
  ## region is associated with each cave.  If not, show error.
  whichRegion <- unique(subDF$Region)
  if (length(whichRegion) > 1)
    stop(paste0("Cave ", namesCaves[i], " assoc. with > 1 region "))

  ## For row i (associated with this cave), put 1 in the column
  ## associated with this region number.
  matchCaveRegionMat[i, whichRegion] <- 1
}
rm(i, subDF, whichRegion)
## ##########################


## ##########################
## Functions for updating the region effect and the cave effect.

updateCaveEffect <- function(y, tau, beta, u, Z, D){

  ## y: observation vector
  ## tau: precision for y's likelihood
  ## beta: vector of region effects
  ## u: precision associated with cave effects
  ## Z: design matrix matching observations to caves
  ## D: desgin matrix matching caves to regions

  
  ## Needed dimension.
  numCaves <- ncol(Z)

  ## Calculate variance of full conditional.
  condPrec <- tau * (t(Z) %*% Z) + diag(u, numCaves, numCaves)
  condVar <- solve(condPrec)

  ## Calculate mean of full conditional.
  condMean <- condVar %*% ( (tau * (t(Z) %*% y)) + (u * (D %*% beta)) )
  
  ## Return updated vector of effects generated using this full conditional.
  return( as.vector(rmvnorm(n=1, mean=condMean, sigma=condVar)) )
}



updateRegionEffect <- function(alpha, u, v, D){

  ## alpha: vector of cave effects
  ## u: precision associated with cave effects
  ## v: precision associated with region effects
  ## D: desgin matrix matching caves to regions

  
  ## Needed dimension.
  numRegions <- ncol(D)

  ## Calculate variance of full conditional.
  condPrec <- u * (t(D) %*% D) + diag(v, numRegions, numRegions)
  condVar <- solve(condPrec)

  ## Calculate mean of full conditional.
  condMean <- condVar %*% (u * (t(D) %*% alpha))
  
  ## Return updated vector of effects generated using this full conditional.
  return( as.vector(rmvnorm(n=1, mean=condMean, sigma=condVar)) )
}



updateErrorPrec <- function(y, alpha, a, b, Z){

  ## y: observation vector
  ## alpha: vector of cave effects
  ## Z: design matrix matching observations to caves
  ## a: prior shape parameter for the prior distribution
  ## b: prior rate parameter for the prior distribution
  
  ## Needed dimension.
  numObs <- length(y)

  ## Calculate alpha for full conditional.
  condAlpha <- (0.5*numObs) + a

  ## Calculate beta for full conditional.
  obsMinusEffects <- y - (Z %*% alpha)
  condBeta <- (0.5 * (t(obsMinusEffects) %*% obsMinusEffects)) + b

  ## Return updated precision using this full conditional.
  return( rgamma(n=1, condAlpha, condBeta) )
}


updateCaveErrorPrec <- function(alpha, beta, a, b, D){

  ## alpha: vector of cave effects
  ## a: prior shape parameter for the prior distribution
  ## b: prior rate parameter for the prior distribution
  ## D: desgin matrix matching caves to regions

  ## Needed dimension.
  numCaves <- nrow(D)

  ## Calculate alpha for full conditional.
  condAlpha <- (0.5*numCaves) + a

  ## Calculate beta for full conditional.
  cavesMinusEffects <- alpha - (D %*% beta)
  condBeta <- (0.5 * (t(cavesMinusEffects) %*% cavesMinusEffects)) + b

  ## Return updated precision using this full conditional.
  return( rgamma(n=1, condAlpha, condBeta) )
}
## ##########################



## ##########################
## Build vectors to hold the data, regional effect (beta), cave effect
## (alpha), and the error variance.

## Large variance on the prior distributions for region effects.
priorPrecRegion <- 1

## Prior distribution of the precision has a large variance.
priorAlphaPrec <- 1
priorBetaPrec <- 0.005
## ##########################


## ##########################
## Write the loop for the MCMC.

## Load this library so that we can generate random deviates from the
## multivariate normal distribution.
library("mvtnorm")

## How many iterations?
numBurnIters <- 5000
numKeepIters <- 10000
## How many iterations to skp before saving.
numSkip <- 10

## Set the starting values based loosely on the results from traditional model:
## myLM <- lm(MercuryConc ~ as.factor(Region) + as.factor(CaveOrHouse), data=caveOrderDF)
## coefficients(myLM)
regionEffect <- c(0.52, 0.34, 0.73)
caveEffect <- rep(0.0, ncol(caveDesignMat))
errorPrec <- 15.20
caveErrorPrec <- 1.0

## Set seed.
set.seed(4314120)
## Start MCMC.
for (i in 1:(numBurnIters+numKeepIters)){

  ## Next, update the region effects.
  regionEffect <- updateRegionEffect(caveEffect, caveErrorPrec,
                                     priorPrecRegion, matchCaveRegionMat)

  ## Update the cave effects.
  caveEffect <- updateCaveEffect(caveOrderDF$MercuryConc, errorPrec,
                                 regionEffect, caveErrorPrec,
                                 caveDesignMat, matchCaveRegionMat)

  ## Now, update the error precision.
  errorPrec <- updateErrorPrec(caveOrderDF$MercuryConc, caveEffect,
                               priorAlphaPrec, priorBetaPrec, caveDesignMat)

  ## Now, update the error precision for the cave effects.
  caveErrorPrec <- updateCaveErrorPrec(caveEffect, regionEffect,
                                       priorAlphaPrec, priorBetaPrec,
                                       matchCaveRegionMat)


  ## Lastly, we need to add the updated value to the results files.
  if ( (i>numBurnIters) && (i%%numSkip==0) ){
    ## For regional effects:
    write(regionEffect, file="multlev_regional_effects.out", ncol=length(regionEffect), append=TRUE)
    ## For cave effects:
    write(caveEffect, file="multlev_cave_effects.out", ncol=length(caveEffect), append=TRUE)
    ## For error precision:
    write(errorPrec, file="multlev_error_precision.out", ncol=1, append=TRUE)
    ## For cave error precision:
    write(caveErrorPrec, file="multlev_cave_error_precision.out", ncol=1, append=TRUE)
  }
}
## ##########################



## ##########################
## Read results in and check them out.

regEffectSimul <- read.table("multlev_regional_effects.out")
caveEffectSimul <- read.table("multlev_cave_effects.out")
errorPrecSimul <- scan("multlev_error_precision.out")
caveErrorPrecSimul <- scan("multlev_cave_error_precision.out")
## ##########################

## ###########################################################
