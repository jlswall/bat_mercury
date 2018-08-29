library("openxlsx")
library("ggmap")


## Read in id, longitude, latitude, cave or bat house name.
fileWpath <- "jls_edited_cavegps.xlsx"
locsDF <- read.xlsx(xlsxFile = fileWpath)
locsDF$type <- factor(locsDF$type, c("Cave (>=3 obs)", "Bat house", "Cave (<3 obs)"))

## Find bounding box for the map.
llCorner <- c(min(locsDF$long)-0.05, min(locsDF$lat)-0.05)
urCorner <- c(max(locsDF$long)+0.05, max(locsDF$lat)+0.05)

## Set width, then determine height from width in proportion.
my.width <- 6
my.height <- my.width * (urCorner[2]-llCorner[2])/(urCorner[1]-llCorner[1])

mymap <- get_map(location=c(llCorner, urCorner), maptype="toner-lite", color="bw", crop=FALSE)
## Add locations to map.
mapPoints <- ggmap(mymap, extent="device") + geom_point(aes(x=long, y=lat, colour=type, shape=type), size=1.25, data=locsDF)
## Adjust the legend.
mapPoints <- mapPoints + theme(legend.position=c(0.2, 0.2), legend.title=element_blank(), legend.text=element_text(size=rel(0.6)))
## Plot finished map.
mapPoints
## Save map.
ggsave(filename="fig1_locations_ggmap.pdf", dev="pdf", width=my.width, height=my.height, units="in", dpi=900)

dev.off()
