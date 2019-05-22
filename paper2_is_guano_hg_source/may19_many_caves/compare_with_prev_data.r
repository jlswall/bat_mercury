library("readxl")
library("dplyr")
library("ggplot2")
library("openxlsx")


## Read in data from May. 2019.
newT <- read_xlsx(path="all_cave_hg_data_2019-05-21.xlsx")
names(newT) <- c("Place", "Region", "Mercury")


## ##########################################
## Compare with the most recent dataset Amy sent me, which appears to
## be almost the same as this one.

## Read in data from Oct. 2018.
oldT <- read_xlsx(path="../many_caves/Hg_Data_Cave_Mercury_2018-10-16.xlsx")
oldT <- oldT[,c(1:4, 6)]
names(oldT) <- c("Place", "Region", "notes", "sampleType", "Mercury")
## In the older table, change "Warren's Cave" to "Warren Cave", since
## the latter name is used in the newer table.
oldT$Place[oldT$Place=="Warren's Cave"] <- "Warren Cave"

## Notice that the old table has 1 fewer observation than the new table.
dim(oldT)
dim(newT)

## The new table has 2 observations corresponding to Falling Waters
## State Park, which don't appear in the older table.
newT %>% anti_join(oldT)

## The older table has an observation corresponding to Altercation,
## which doesn't appear in the newer table.
oldT %>% anti_join(newT)

rm(oldT)
## ##########################################



## ##########################################
## Compare with the dataset used in our recent publication.

## Read in the data from the Excel file.
fileWpath = "../../paper1_hgconc_guano_caves_bathouses/reduced_dataset/Hg_DATA_ActaChirp.xlsx"

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
rm(fileWpath)


## Now combine the caves and bat houses into one data frame, adding a
## column to denote whether the observation is from a cave or a bat
## house.
cavesDF$CaveOrHouse <- "cave"
housesDF$CaveOrHouse <- "bat house"
allDF <- rbind(cavesDF, housesDF)
rm(cavesDF, housesDF)

## Re-name "Jerome's Bat Cave" to "Jerome's Cave" to match the way the
## cave is named in the newer data set.
allDF$Place[allDF$Place=="Jerome's Bat Cave"] <- "Jerome's Cave"

## ##########################################
