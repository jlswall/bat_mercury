library("ggplot2")
library("readxl")
library("dplyr")
library("readr")


## #############################################
## Read in the data from the Excel file.
fileWpath = "Hg_Data_Cave_Mercury_2018-10-16.xlsx"
## Read in dataset, giving more usable names to the various columns.
## Note that we won't be using columns 7 and 8.
allT <- read_xlsx(path=fileWpath)
allT <- allT[,1:6]
colnames(allT) <- c("origCaveName", "region", "notes", "sampleType", "OM", "mercury")

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
allT$coreID <- "not core"
allT$distFromSurface <- 0

## For Climax Cave, we have a 10-in core:
allT$coreID[34:43] <- "core 1"
allT$distFromSurface[34:43] <- 0:9

## For Cottondale, we have a 6-in core:
allT$coreID[53:58] <- "core 1"
allT$distFromSurface[53:58] <- 0:5

## For Florida Caverns Old Indian Cave, we have two 8-in cores:
allT$coreID[61:68] <- "core 1"
allT$distFromSurface[61:68] <- 0:7
allT$coreID[69:76] <- "core 2"
allT$distFromSurface[69:76] <- 0:7

## For Judge's Cave, we have an 11-inch core and a 8-inch core:
allT$coreID[125:135] <- "core 1"
allT$distFromSurface[125:135] <- 0:10
allT$coreID[136:143] <- "core 2"
allT$distFromSurface[136:143] <- 0:7
## #############################################



## #############################################
## The original cave names are quite long.  Merge in the shortened
## names for use when plotting.

## First read in spreadsheet with list of original names and shortened names.
caveNamesT <- read_csv("orig_and_short_cave_names.csv")

## Merge in the data frame giving the correspondence between the
## original names and shortened versions of those names.
allT <- allT %>% inner_join(caveNamesT)
rm(caveNamesT)

## Here's a plot showing all the observations by region and by cave.
ggplot(allT, aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=sampleType, shape=coreID), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")


## For the core measurements, here's a plot of mercury vs. distance
## from the surface.
ggplot(subset(allT, coreID!="not core"),
       aes(x=distFromSurface, y=mercury)) +
  geom_point(aes(color=coreID)) +
  facet_wrap(~cave)
## #############################################



## #############################################
## Make another version of the dataset with the core measurements
## summarized by the average for each core.

wCoreAvgsT <- bind_rows(
    allT %>% filter(coreID!="not core") %>% group_by(region, cave, coreID, sampleType) %>% summarize(mercury=mean(mercury)),
    allT %>% select(region, cave, coreID, sampleType, mercury) %>% filter(coreID=="not core")
) %>% arrange(region, cave, sampleType)


ggplot(wCoreAvgsT, aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=sampleType), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")


ggplot(wCoreAvgsT, aes(x=cave, y=sqrt(mercury))) +
  geom_jitter(aes(color=sampleType), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="sqrt(Mercury)")


ggplot(wCoreAvgsT, aes(x=cave, y=log10(mercury))) +
  geom_jitter(aes(color=sampleType), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="log10(Mercury)")
## #############################################



## #############################################
## Try JAGS.
library("rjags")

## Re-order the data frame so that it is sorted by region, then by
## cave, and then by sample type (sediment before guano).
allT <- allT %>% arrange(region, cave, desc(sampleType))

## We'll have a fixed effect for guano (vs. sediment).  The type
## variable is "1" when the observation comes from guano and "0" when
## it comes from sediment.
codeType <- c(1:length(unique(allT$sampleType)))
names(codeType) <- unique(allT$sampleType)
type <- as.numeric(codeType[allT$sampleType])
indicGuano <- ifelse(allT$sampleType=="G", 1, 0)

## We'll have fixed effects for the various caves, so we need to
## assign a number to each cave.
codeCave <- c(1:length(unique(allT$cave)))
names(codeCave) <- unique(allT$cave)
cave <- as.numeric(codeCave[allT$cave])


## ##########
## This model includes:
##   fixed effects for regions
##   fixed effect for guano (as opposed to sediment)
##   random effects for individual caves
##   separate variances sediment vs. guano
##   a normal likelihoood.

## For reproducibility:
set.seed(631492)

data <- list(y=allT$mercury, region=allT$region, type=type, cave=cave, indicGuano=indicGuano, N=nrow(allT), numRegion=3, numType=length(codeType), numCave=length(codeCave))
init <- list(mu=rep(1, 3), beta=rep(0, 3), theta=rep(0, length(codeCave)),
             tau=rep(1, data$numType), tauTheta=10)
modelstring="
  model {
    for (i in 1:N){
      y[i] ~ dnorm(yhat[i], tau[type[i]])
      ## y[i] ~ dt(yhat[i], tau[type[i]], 4)
      yhat[i] = mu[region[i]] + (indicGuano[i]*beta[region[i]]) + theta[cave[i]]
      resid[i] = y[i] - yhat[i]
    }

    ## Each region has a fixed effect for mean sediment concentration.
    for (j in 1:numRegion) {
      mu[j] ~ dnorm(0, 0.0001)
    }

    ## Each region also has a fixed effect for guano.
    for (j in 1:numRegion) {
      beta[j] ~ dnorm(0, 0.0001)
    }

    ## Each cave has a random effect.
    for (j in 1:numCave) {
      theta[j] ~ dnorm(0, tauTheta)
    }
    tauTheta ~ dgamma(1, 0.005)

    ## We have separate variances for sediment vs. guano observations.
    for (k in 1:numType){
      tau[k] ~ dgamma(1, 0.005)
    }
}
"
normMdl <- jags.model(textConnection(modelstring), data=data, inits=init)
update(normMdl, n.iter=20000)
fancyfixed <- coda.samples(model=normMdl,
                         variable.names=c("mu", "beta", "tau", "tauTheta"),
                         n.iter=50000, thin=50)
plot(fancyfixed)
print(summary(fancyfixed))


## #############################################



## #############################################
## Do some exploratory analysis with each group of core measurements
## summarized by taking an average.

## For all the measurements that come from cores, find the average for
## each separate core.  Remove "notes" column for each observation,
## since they won't make sense after averaging.
coreAvgT <- allT %>%
  filter(coreID!="not core") %>%
  group_by(cave, region, sampleType, coreID) %>%
  summarize(mercury=mean(mercury))

## Now, combine these with all the non-core measurements ("notes"
## column) also left out).
useAvgT <- bind_rows(allT %>%
                     filter(coreID=="not core") %>%
                     select(cave, region, sampleType, coreID, mercury),
                     coreAvgT)
rm(coreAvgT)



ggplot(useAvgT, aes(x=as.factor(region), y=mercury, fill=sampleType)) +
  geom_boxplot() +
  xlab("Region") +
  ylab("Mercury")

ggplot(useAvgT, aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=sampleType), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")
## #############################################



## #############################################
## Pull out only caves which have core measurements.

cavesWcoreT <- allT %>% inner_join(allT %>% filter(coreID!="not core") %>% distinct(cave))
ggplot(cavesWcoreT %>% filter(sampleType=="G"),
       aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=coreID), width=0.2) +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")
## #############################################
