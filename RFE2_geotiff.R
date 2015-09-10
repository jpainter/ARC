# noaa RFE20 geotiff climate data from http://www.cpc.ncep.noaa.gov/products/fews/data.shtml

# example script from http://creativemorphometrics.co.vu/blog/2014/03/27/extracting-climate-data-in-r/

library(raster)
library(sp)
library(rgdal)

jul05 = raster("noaa/africa_rfe.20150705.tif")
jul06 = raster("noaa/africa_rfe.20150706.tif")
jul07 = raster("noaa/africa_rfe.20150707.tif")
jul08 = raster("noaa/africa_rfe.20150708.tif")
jul09 = raster("noaa/africa_rfe.20150709.tif")
jul10 = raster("noaa/africa_rfe.20150710.tif")

# plot tiff
plot(jul05)

# add country boundaries
bounds <- readOGR(dsn=paste0(getwd(),"/TM_WORLD_BORDERS-0.3"), 
                  layer= "TM_WORLD_BORDERS-0.3") #import borders

plot(bounds, add = TRUE)


# plot series
mtStack <- stack(jul05, jul06, jul07, jul08, jul09, jul10)
library(rasterVis)
levelplot(mtStack) #may take long time to plot
histogram(mtStack)

