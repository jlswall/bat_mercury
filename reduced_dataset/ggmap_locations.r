library("openxlsx")
library("ggmap")


## Read in id, longitude, latitude, cave or bat house name.
fileWpath <- "cavegps.xlsx"
locsDF <- read.xlsx(xlsxFile = fileWpath)
colnames(locsDF) <- c("id", "long", "lat", "name")

## Designate caves vs. bat houses. The only bat houses are at Univ. of
## Florida and Lower Suwannee NWR.
locsDF$sitetype <- "cave"
locsDF$sitetype[c(2, 5)] <- "bat house"
locsDF$sitetype <- as.factor(locsDF$sitetype)


## Remove sites which had fewer than 3 observations.
locsDF <- locsDF[c(11, 10, 7, 9, 8, 6, 5, 2),]

mymap <- get_map(location=c(long=-83.86, lat=30.12), zoom=8, maptype="toner-lite")
                                  ##long = 153.27, lat = -27.48), zoom = 9  )
mapPoints <- ggmap(mymap) + geom_point(aes(x=long, y=lat, colour=name), data=locsDF)
mapPoints 


## Make a plot with caves as circles and bat houses as squares.

mypch <- c(1, 15)
mycolors <- c("black", "blue")


## Set plot region width in inches.
widthPlotRegion <- 7
with(locsDF,
     aspect.ratio.plot(long, lat, file="locations_zoomed_in.pdf",
                       f.type="pdf", type="n", width=widthPlotRegion,
                       mai=rep(0, 4), bty="n", axes=FALSE,
                       xlab=NA, ylab=NA)
     )
map("state", add=TRUE)
with(subset(locsDF, type=="cave"), points(long, lat, pch=mypch[1], col=mycolors[1]))
with(subset(locsDF, type=="bat house"), points(long, lat, pch=mypch[2], col=mycolors[2]))
legend("bottomleft", legend=c("cave", "bat house"), pch=mypch, col=mycolors, bty="n")
## Label as Georgia.
text(-83.8, 30.9, "Georgia", cex=1.5)
## Label as Florida.
text(-83.8, 30.2, "Florida", cex=1.5)
dev.off()



## Work with this map to include an area bigger than where our caves and
## bat houses are, so that the state outlines can be seen more
## clearly.
xrng <- c(-86.25, -81.25)
yrng <- c(24.9, 34.9)
init.fig.dimen(file="location_map.pdf", height=6.0, width=3.0,
               mai=rep(0, 4), bty="n")
with(locsDF, plot(long, lat, type="n", axes=FALSE, xlab=NA, ylab=NA,
                  xlim=xrng, ylim=yrng))
map("state", add=TRUE)
with(subset(locsDF, type=="cave"), points(long, lat, pch=mypch[1], col=mycolors[1]))
with(subset(locsDF, type=="bat house"), points(long, lat, pch=mypch[2], col=mycolors[2]))
legend("bottomleft", legend=c("cave", "bat house"), pch=mypch, col=mycolors, bty="n")
dev.off()
