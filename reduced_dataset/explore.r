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
oldfileWpath = "../explore/all_guano_w_species.xlsx"
oldrawDF <- read.xlsx(xlsxFile = oldfileWpath, colNames=TRUE, rowNames=FALSE)

## Compare new dataset.
newfileWpath = "./data_amy_collected.xlsx"
newrawDF <- read.xlsx(xlsxFile = newfileWpath, colNames=TRUE, rowNames=FALSE)




## Replace all strings "N/A" with "NA" (which R understands).
rawDF[rawDF=="N/A"] <- NA


