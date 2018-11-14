library("ggplot2")
library("readxl")
library("dplyr")
library("readr")
library("coin")


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

ggplot(useAvgT %>% filter(region==1),
       aes(x=cave, y=mercury)) +
  geom_boxplot() +
  facet_wrap(~sampleType) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Mercury")

ggplot(useAvgT %>% filter(region==2),
       aes(x=cave, y=mercury)) +
  geom_boxplot() +
  facet_wrap(~sampleType) +
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Mercury")

ggplot(useAvgT, aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=sampleType), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")

ggplot(allT, aes(x=cave, y=mercury)) +
  geom_jitter(aes(color=sampleType, shape=coreID), width=0.2) +
  facet_wrap(~region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  labs(y="Mercury")


## #############################################








## #############################################
## Look at the relationship among core measurements.  Do they have
## less variability than the non-core measurements?  Are there signs
## of strong correlation within each core?

## Get list of all caves which have core samples.
cavesWcoresV <- unique(allT %>% filter(coreID!="not core") %>% pull(cave))

ggplot(subset(allT, (cave %in% cavesWcoresV) & (sampleType=="G")),
              aes(x=coreID, y=mercury, color=cave)) +
  scale_shape_identity() +
  geom_jitter(mapping=aes(shape=48+distFromSurface), size=3, width=0.2) +
  facet_wrap(~cave)


with(allT, boxplot(mercury))
boxplot(mercury ~ as.factor(sampleType), data=allT)
boxplot(mercury ~ region, data=allT)



## #############################################
## Compare sediments between Caves.

sedimentT <- subset(cavesT, SampleType=="S")
with(sedimentT, table(Cave))
with(sedimentT, tapply(Mercury, Cave, summary))

## ##########
## This compares all the sediments in Climax Cave with all those in
## the Glory Holl Cave.

ggplot(sedimentT, aes(Cave, Mercury))+
  geom_jitter(width=0.2) +
  labs(title="Mercury concentration in sediments", x=NULL, y="Mercury concentration (ppm)") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(size=11))
ggsave(filename="dotplot_compare_sediment.pdf", dev="pdf", width=4, height=4, units="in", dpi="print")

## ## The following plot puts the MapID numbers on the plot instead of
## ## points, so that we can identify the outliers more easily.
## ggplot(sedimentT, aes(Cave, Mercury)) +
##   geom_text(aes(label=MapID)) +
##   labs(title="Mercury concentration in sediments", x=NULL, y="Mercury concentration (ppm)")

## The mercury level between the sediments in Climax Cave vs. those in
## Glory are not significantly different.
oneway_test(Mercury ~ as.factor(Cave), data=sedimentT, distribution="exact")
## p-value = 0.6754

## ## Just out of curiousity, I tested whether the variances are
## ## significantly different.
## fligner.test(Mercury ~ as.factor(Cave), data=sedimentT)
## ## p-value = 0.2689
## ##########


## ##########
## I'm not sure  that bats are getting past the  Barrel Room in Climax
## Cave.  What  if we compare  the sediments up  to that point  in the
## cave with those in the other cave (where there are no bats)?

reducedSedimentT <- bind_rows(sedimentT %>% filter(Cave=="Glory Hole Cave"),
                              sedimentT %>% filter(Cave=="Climax Cave") %>%
                              filter(!(MapID %in% c("14", "15", "16", "17")))
                          )
reducedSedimentT$Cave[reducedSedimentT$Cave=="Climax Cave"] <- "Climax Cave\n (1st portion only)"

ggplot(reducedSedimentT, aes(Cave, Mercury))+
  geom_jitter(width=0.2) +
  labs(title="Mercury concentration in sediments", x=NULL, y="Mercury concentration (ppm)") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5), axis.text.x = element_text(size=11))
ggsave(filename="dotplot_compare_sediment_reducedClimaxCave.pdf", dev="pdf", width=4, height=4, units="in", dpi="print")

## The mercury level between the sediments in the first part of Climax
## Cave vs. those in Glory are not significantly different.
oneway_test(Mercury ~ as.factor(Cave), data=reducedSedimentT, distribution="exact")
## p-value = 0.2581
## ##########

rm(reducedSedimentT, sedimentT)
## #############################################



## #############################################
## Climax Cave has some guano measurements and some sediment
## measurements.  (The other cave only has sediment.)  Compare guano
## and sediment in just Climax Cave.

climaxT <- subset(cavesT, Cave=="Climax Cave")
with(climaxT, tapply(Mercury, SampleType, summary))

## ##################
## List the areas the samples came from.

climaxT$area <- NA
climaxT$area[climaxT$MapID %in% c(2,3,13,20)] <- "entrance chimneys"
climaxT$area[climaxT$MapID %in% c(4,5)] <- "tee pee room"
climaxT$area[climaxT$MapID %in% c(11,12)] <- "keyhole passage"
climaxT$area[climaxT$MapID==14] <- "devil's dining room"
climaxT$area[climaxT$MapID==15] <- "old formation room"
climaxT$area[climaxT$MapID %in% c(16,17)] <- "cenagosa passage"
climaxT$area[climaxT$MapID %in% c(6,7,8,9,10)] <- "barrel room"
climaxT$area[climaxT$MapID %in% paste("C", 1:10, sep="")] <- "barrel room"
climaxT$area[climaxT$MapID=="CC2"] <- "barrel room"
## ##################


## ##################
## Mercury concentrations by area of the cave, with different colors
## for guano or sediment samples.

ggplot(climaxT, aes(area, Mercury)) +
  geom_jitter(aes(color=SampleType), width=0.2) +
  labs(title="Mercury concentrations in Climax Cave", x=NULL, y="Mercury concentration (ppm)", color="Type") +
  ## theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        legend.position=c(0.9, 0.85),
        axis.text.x = element_text(angle=90, hjust=1, size=11))
ggsave(filename="dotplot_climaxcave_by_area.pdf", dev="pdf", width=4, height=4.5, units="in", dpi="print")
## ##################


## ##################
## What happens if we treat the core measurements as independent from
## each other?

boxplot(Mercury ~ SampleType, data=climaxT)
oneway_test(Mercury ~ as.factor(SampleType), data=climaxT, distribution="exact")
## p-value = 2.252e-05
## ##################


## ##################
## What happens if we average the core measurements together, and use
## their average (rather than treating these core measurements as
## independ from each other)?

## First start with all non-core observations.
treatCoreAvgT <- climaxT %>% filter(coreID=="not core") %>%
  select(-MapID, -Notes, -distFromSurface)
## Now, average the others.
rowAvgCoreT <- climaxT %>% filter(coreID=="core 1") %>%
  group_by(Cave, Date, SampleType, coreID) %>%
  summarize(Mercury=mean(Mercury))
## Now, bind these together.
treatCoreAvgT <- bind_rows(treatCoreAvgT, rowAvgCoreT)

## boxplot(Mercury ~ SampleType, data=treatCoreAvgT)
oneway_test(Mercury ~ as.factor(SampleType), data=treatCoreAvgT, distribution="exact")
## p-value = 0.004068

rm(rowAvgCoreT)
## ##################
## #############################################



## #############################################
## Look at just the observations taken from the barrel room.

barrelT <- climaxT %>% filter(area=="barrel room")

## They're all of type "G" (guano).
with(barrelT, table(SampleType))

boxplot(Mercury ~ coreID, data=barrelT)

barrelT$color <- "black"
barrelT$color[barrelT$coreID=="core 1"] <- "blue"
plot(1:nrow(barrelT), barrelT$Mercury, type="n")
text(1:nrow(barrelT), barrelT$Mercury, 1:nrow(barrelT), col=barrelT$color)

oneway_test(Mercury ~ as.factor(coreID), data=barrelT, distribution="exact")

## To test whether the variances of core vs. non-core measurement are
## significantly different.
fligner.test(Mercury ~ as.factor(coreID), data=barrelT)
## p-value = 0.1229
## #############################################



## #############################################
## We had 20 obs for Climax Cave in last paper, but we only have 17
## guano estimates now, and 10 sediments.
with(cavesT, table(Cave, SampleType))


## Read in old dataset to see what's missing.
oldT <- read_csv("old_data_climax_cave.csv")
oldT <- oldT %>% select(-Place, -Species, -Region, -OM, -CaveOrHouse)


## Find observations in old dataset that are not in the new one.
notinnewT <- oldT %>%
  anti_join(cavesT %>%
            filter(SampleType=="G" & Cave=="Climax Cave"),
            by = c("Mercury", "Date")
            )

## Find observations in old dataset that are not in the new one.
notinoldT <- cavesT %>%
  filter(SampleType=="G" & Cave=="Climax Cave") %>%
  anti_join(oldT, by = c("Mercury", "Date"))
## #############################################
