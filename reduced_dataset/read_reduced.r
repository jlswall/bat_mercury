library("ggplot2")


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
## Look at relationship among core measurements.

library("ggplot2")

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
## #############################################



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
## Climax Cave and Florida Caverns Old Indian Cave (0.1140023, 0.2800839)
## Climax Cave and Judge's Cave (0.1037451, 0.3123842)


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
houseNmPlot <- c("Suwannee NWR \nBat House",
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
wilcox_test(Mercury ~ as.factor(Place), data=bathouseDF, conf.level=0.95)

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




