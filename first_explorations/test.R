library("openxlsx", lib.loc="C://Users//jenise//Documents//R//win-library//3.3")
fileWpath = "C://Users//jenise//Google Drive//amy_bat_hg//explore//all_guano_w_species.xlsx"
rawDF <- read.xlsx(xlsxFile = fileWpath, colNames=TRUE, rowNames=FALSE)
## Replace all strings "N/A" with "NA" (which R understands).
rawDF[rawDF=="N/A"] <- NA


idPrefix <- c("CLM C", "COT C1 ", "FCCI C1 ", "FCCI C2 ", "JUD C1 ",
              "JUD C2 ", "UFBH C1-", "UFBH C2-")
names(idPrefix) <- c("GSS 36 core", "FCS 872 core", "FCS 555 core 1",
                     "FCS 555 core 2", "FCS 556 core 1",
                     "FCS 556 core 2", "UFBH core 1", "UFBH core 2")
rawDF[,"coreName"] <- NA
rawDF[,"coreOrder"] <- NA
for (i in 1:nrow(rawDF)){
  for (j in 1:length(idPrefix)){
    which.match <- which(paste0(idPrefix[j], 1:20) == rawDF[i, "SampleID"])
    if (length(which.match) == 1){ #Match was found
      rawDF[i, "coreName"] <- names(idPrefix[j])
      rawDF[i, "coreOrder"] <- which.match
    }
  }
}
rm(i, j, which.match)

toplayerDF <- subset( rawDF, ( is.na(coreName) | coreOrder==1 ) & (Region!=4) )


permute1WayAnova <- function(x, grp, numPermutations = 10000){

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
testRes <- permute1WayAnova(toplayerDF$MercuryConc, toplayerDF$Region, numPermutations=50000)
