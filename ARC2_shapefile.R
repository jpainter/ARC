# noaa ARC2 geotiff climate data from http://www.cpc.ncep.noaa.gov/products/fews/AFR_CLIM/afr_clim.shtml
# downloads at http://www.cpc.ncep.noaa.gov/products/fews/data.shtml
# see: http://www.cpc.ncep.noaa.gov/products/fews/AFR_CLIM/arc2_201303_final.pdf
# example script from http://creativemorphometrics.co.vu/blog/2014/03/27/extracting-climate-data-in-r/


# packages  ####
library(Hmisc)  # for monthDays function
library(data.table)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(maptools)
require(rleafmap)
library(dplyr)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(ggthemes)

# country boundaries ####
# bounds <- readOGR(dsn="../TM_WORLD_BORDERS-0.3", 
#                   layer= "TM_WORLD_BORDERS-0.3") #import borders

# world = getMap()
world = getMap() %>% # transform to arc file datum
   spTransform(CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs")) 
# world < - subset( world, (world$continent %in% "Antarctica"))
summary(world)
plot(world)
spplot(world, z="GEO3major")

# world = getMap() %>% spTransform(CRS("+proj=wintri"))
# plot(world)

# modify for ggplot (as done for malaria_atlas)
atlas <- fortify(world) %>% data.table()
atlas <- atlas[id!='Antarctica',]

# highlight PMI and label
library(rgeos)
centroids = gCentroid(world, byid=TRUE)  # calculate center of mass
# centroids <- spTransform(centroids, CRS("+proj=wintri")) 
centroids =  as.data.frame(centroids)
centroids$country = rownames(centroids)


pmi = c( "Angola", "Benin", "Democratic Republic of the Congo", 
        "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", 
        "Nigeria", "Rwanda", "Senegal", "United Republic of Tanzania", "Uganda", "Zambia", "Zimbabwe" )

pmi_abrev = c( "Ang", "Ben", "DRC", 
        "Eth", "Gha", "Gui", "Ken", "Lib", "Mad", "Mal", "Mali", "Moz", 
        "Nig", "Rwa", "Sen", "Tanz", "Uga", "Zam", "Zim" )

pmi_world =  atlas %>% filter(id %in% pmi) %>%
   mutate( abrev = pmi_abrev[match(id, pmi)])

# add country abbreviation to centroids data frame
centroids =  centroids %>% 
   mutate( abrev = pmi_abrev[match(country, pmi)] )

# download from ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/shp ####

library(RCurl)
url <- "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/shp/"
filenames <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
filenames = unlist(strsplit(filenames, "\n" ))
file_list = strsplit(filenames, "[.]")
library(lubridate) # extract date
file_dates = unlist(round_date(ymd(sapply(file_list, "[[", 2))))

# most recent ####
#      most.recent <- filenames[length(filenames)]
#      bin <- getBinaryURL(paste0(url, most.recent), ssl.verifypeer=FALSE)
#      zip_file = "arc.zip"
#      con <- file(zip_file, open = "wb")
#      writeBin(bin, con)
#      close(con)
#      unzip(zip_file)
#      arc_file = strsplit(most.recent, ".zip")[[1]]

# last week ####
#      num_files = length(filenames)
#      last.week <- filenames[ (num_files-6):num_files ]
#      
#      for (i in 1:7){
#           # TODO add progress bar and/or print i
#           zip_file = "arc.zip"
#           con <- file(zip_file, open = "wb")
#           bin <- getBinaryURL(paste0(url, last.week[i]), ssl.verifypeer=FALSE)
#           writeBin(bin, con)
#           close(con)
#           unzip(zip_file)
#      }
   

# weekly rain totals ####
#      rain_total = numeric(601551)
#      
#      for (i in 1:7){  
#           arc_file = strsplit(last.week[i], ".zip")[[1]] 
#           arc = readGDAL(arc_file, silent = TRUE)
#           rain = arc@data$band1
#           print(summary(rain))
#           rain_total = rain_total + rain
#      }
#      
#      summary(rain_total)
#      arc_week = arc
#      arc_week@data$band1 = rain_total
#      arc_week_image = image(arc_week)  # looks like red box with yellow lines--not good!
#      plot(bounds, add = TRUE)

# monthly rain totals ####

monthly_arc_data = function( year, month){
   folder_name = paste0("shapefiles/", year, month)
   start_date = paste0(year, month, "01")
   month_days = monthDays(ymd(start_date))
   end_date = paste0(year, month, month_days)
   month_interval = new_interval(ymd(start_date), ymd(end_date))
   days_in_interval = file_dates %within% month_interval
   table( days_in_interval )
   files_in_interval = filenames[days_in_interval]
   num_files = length(files_in_interval)
        
   # download daily files  
     for (i in 1:num_files){
         cat( paste(i, files_in_interval[i]), sep = "\n") ;flush.console()
          
          if (!dir.exists(folder_name)) dir.create(folder_name) 
          if (file.exists( paste0(folder_name, "/",
                                  unlist(strsplit(files_in_interval[i], ".zip" )) )
                           )) {
             next
          }
          zip_file = paste0(folder_name, "/arc.zip")
          con <- file(zip_file, open = "wb")
          
          cat(" downloading file...\n") ;flush.console()
          
          repeat{
             success = TRUE
             bin <- tryCatch(
                {
                getBinaryURL(paste0(url, files_in_interval[i]), ssl.verifypeer=FALSE)
                },
             
             error = function(cond){
                success = FALSE
                print("download error")
                return(success)
             }
             )
            if (success == TRUE)   break
          }
          
          cat("  writing file...\n") ;flush.console()
          writeBin(bin, con)
          close(con)
          
          cat("   unzipping file...\n") ;flush.console()
          unzip(zip_file, exdir = folder_name)
          cat("done \n \n") ;flush.console()
     }
     
    # combine daily files
   library(spatstat)
   library(shapefiles)
   
   # TODO : figure out way to combine shapefiles.
     rain_total = integer(4720)
     for (i in 1:2){  # num_files
         cat( paste(i, files_in_interval[i]), sep = "\n") 
          arc_file = unlist(strsplit(files_in_interval[i], ".shp.zip" )) 
          
          arc = readOGR(folder_name, arc_file) 
          gIsValid(arc) 
            # if fails, see which rings failed
             which(!gIsValid(arc, byid=TRUE)) 
                # test when re-run with fixed buffer
                which(!gIsValid(gBuffer(arc,byid=TRUE,width=0),byid=TRUE))
                   # if reports all the polygons as being valid. 
                   # then, fixing them with 
                   arc = gBuffer(arc,width=0,byid=TRUE) 
             
          # arc = read.shapefile(paste0( folder_name, "/", arc_file) )
          
         if (i == 1){
            arc_period = arc
         } else {
            arc_period = union( arc_period, arc)
         }

          rain = arc.df$Contour
          cat(summary(rain)); cat("\n")
          rain_total = rain_total + rain
     }
     
     cat(summary(rain_total))
     arc_period = arc
     arc_period@data$Contour = rain_total
     
     # write summary to file
     period_file_name = paste0("africa_arc", year, month)
     save( arc_period, file = paste0( folder_name, "/", period_file_name) )
}

# function to download all data files in a month
download_arc_data = function( year, month){
   folder_name = paste0("shapefiles/", year, month)
   start_date = paste0(year, month, "01")
   month_days = monthDays(ymd(start_date))
   end_date = paste0(year, month, month_days)
   month_interval = new_interval(ymd(start_date), ymd(end_date))
   days_in_interval = file_dates %within% month_interval
   table( days_in_interval )
   files_in_interval = filenames[days_in_interval]
   num_files = length(files_in_interval)
        
   # download daily files  
     for (i in 1:num_files){
         cat( paste(i, files_in_interval[i]), sep = "\n") ;flush.console()
          
          if (!dir.exists(folder_name)) dir.create(folder_name) 
          if (file.exists( paste0(folder_name, "/",
                                  unlist(strsplit(files_in_interval[i], ".zip" )) )
                           )) {
             next
          }
          zip_file = paste0(folder_name, "/arc.zip")
          con <- file(zip_file, open = "wb")
          
          cat(" downloading file...\n") ;flush.console()
          
          repeat{
             success = TRUE
             bin <- tryCatch(
                {
                getBinaryURL(paste0(url, files_in_interval[i]), ssl.verifypeer=FALSE)
                },
             
             error = function(cond){
                success = FALSE
                print("download error")
                return(success)
             }
             )
            if (success == TRUE)   break
          }
          
          cat("  writing file...\n") ;flush.console()
          writeBin(bin, con)
          close(con)
          
          cat("   unzipping file...\n") ;flush.console()
          unzip(zip_file, exdir = folder_name)
          cat("done \n \n") ;flush.console()
     }
}

# get all months from 2009 (first year available) up to most recent...
for (year in 2009:year(now())){
   year = as.character(year)
   for (i in 1:12){
      month = c("01","02","03","04","05","06","07","08","09","10","11","12")
      cat( month[i], year, "\n")
      monthly_arc_data(year, month[i])
   }
   
#      period_file = paste0("africa_arc", year, month, ".tif")
#      con <- file(period_file, open = "wb")
#      writeBin(period_file, con)
#       close(con)
   
}



# test ####

arc.points = fortify(arc, region = "id")
arc@data$id = rownames(arc@data)
arc.df = merge(arc.points, arc@data, by="id")
          
arc.df$monthly_rain = cut(arc.df$Contour, 
                                    breaks = c(0,50,100,200,400, Inf),
                                    include.lowest = TRUE)
          
          gg +
              geom_polygon(data = arc.df, aes(long,lat,group=group, alpha= monthly_rain ), fill = "blue") +
              geom_path(color="white") +
              coord_equal() +
              scale_alpha_manual(name = "Rainfall (mm)", 
                        values = c(0, .1, .30, .60, .90)) + 
             xlim(-20.05, 55.05) +
             ylim(-40.05, 40.05) 
     
#      arc_week_image = image(arc_week)  # looks like red box with yellow lines--not good!
#      plot(bounds, add = TRUE)

# load data ####
year = "2014"
month = "05"
period_file_name = paste0("africa_arc", year, month)
# assign(period_file_name, local(get(load(period_file_name))) )
arc_period = local(get(load(period_file_name))) 

# plot as spatial lines  (base R)     ####    
ac = as.image.SpatialGridDataFrame(arc_period)
Dcl <- contourLines(ac, nlevels = 8)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl)  # convert to SpatialLinesDataFrame

proj4string(SLDF) = CRS("+proj=longlat")
s = spTransform(SLDF, CRS("+proj=wintri"))
plot(s)

plot(SLDF, col = terrain.colors(8))
plot(world, add = TRUE)    # TODO: not working with rworldmaps      

# plot as spatial points  (ggplot)   SLOW!   ####     
ac = as.image.SpatialGridDataFrame(arc_week)
acw = as.ppp.SpatialPoints(arc_week) %>% as.data.frame() %>% mutate(z = as.numeric(ac$z))
x = fortify(acw)
ggplot() + geom_point( data=acw, aes( x=x, y=y, color=z) ) # SLOW!   

# ggplot spatialGridDataFrame as tiles ####
 # from https://rpubs.com/tmieno2/68747:
   # As a SpatialGridDataFrame is not compatible with mapping using ggplot( ), 
   # we need to convert it into a data frame, 
   # in which each observation is assigned the longitude and latitude 
   # of the centroid of its corresponding grid. 
   # We can achieve this with the following code 
   # (If you would like to know the inner works of the function, 
   # please run the code line by line),

library(data.table)
sgdf_transform = function(sgdf){
  dim <- sgdf@grid@cells.dim
  bbox <- sgdf@bbox
  r <- raster(xmn=bbox[1,1], xmx=bbox[1,2], ymn=bbox[2,1], ymx=bbox[2,2], 
              ncols=dim[1], nrows=dim[2])
  r <- setValues(r,matrix(sgdf@data$band1, nrow = dim[1], ncol = dim[2]) %>% t()) 
  data <- rasterToPoints(r) %>% data.table()
  return(data)
}

arc_df = sgdf_transform(arc_period)

# assign levels to data ####
arc_df$z = cut(arc_df$layer, 
                        breaks = c(0,50,100,200, 400, Inf), 
                        include.lowest = TRUE)

ggplot() + geom_tile(data= arc_df, aes(x, y, fill=z)) +
   theme(
        legend.position = 'bottom',
        legend.key.size = unit(1, "cm")
        )

# # using grid2Polygons package is too slow 
# install.packages("Grid2Polygons")
# library(Grid2Polygons)


# ac = as.image.SpatialGridDataFrame(arc_data)

# assign levels to data ####
arc_period@data$z = cut(arc_period@data$band1, 
                        breaks = c(0,50,100,200, 400, Inf), 
                        include.lowest = TRUE)

# convert grid to polygons
acw = arc_period %>%
   # spTransform(CRS("+proj=wintri")) %>%
   fortify()
#    as.ppp.SpatialPoints() %>% 
   # as.data.frame() 

# assign levels to data
acw$z = cut(acw$band1, breaks = c(0,50,100,200, 400, Inf), include.lowest = TRUE)
table(acw$z, useNA = "always")

# quick version
ggplot() + 
   geom_tile( data = acw, aes( x= x, y= y, fill = z)) +
   scale_fill_brewer() +
   geom_path(data = atlas, aes( x=long, y=lat, group = group))

# complete version
gg <- ggplot() + 
   geom_map(data=atlas, map=atlas,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill="grey", alpha = .75, size=0.25) +
   
   geom_map(data = pmi_world, map = pmi_world,
                    aes(x=long, y=lat, map_id=id),
                    color="black", fill="white", alpha = 1 , size=0.25) + 
   
   geom_text(data = centroids, 
                    aes(x=x, y=y, label = abrev),
                    color="black", size= 2) + 
   
   coord_equal() + 
   scale_x_continuous(expand=c(0,0)) + 
   scale_y_continuous(expand=c(0,0)) +
   theme_map() +
   theme(
      legend.position = c(0,0),
      legend.background = element_rect(fill=alpha('white', 0))
   ) 

gg + 
   xlim(-20, 55) +
   ylim(-40, 40) 

# see color ramp
# colorRampPalette(brewer.pal(9,"Blues"))(6)

gg +    
   # geom_tile(data= arc_df, aes(x, y, fill=z), alpha = .75) +
   scale_fill_manual(name = "Rainfall (mm)", 
                     values = c(NA, "#CFE1F2", "#93C4DE", "#4A97C9", "#1664AB", "#08306B")) + 
   theme(
        legend.position = c(0,0),
        legend.key.size = unit(1, "cm")
        )+ 
   xlim(min(arc_df$x), max(arc_df$x)) +
   ylim(min(arc_df$y), max(arc_df$y)) +
   coord_equal()
 

# LOAD and PLOT

plot_arc=  function( year = "2014", month = "05"){
   
   period_file_name = paste0("africa_arc", year, month)
   arc_period = local(get(load( paste0('shapefiles/', year, month, '/', period_file_name) ) ) )
   
   sgdf_transform = function(sgdf){
      dim <- sgdf@grid@cells.dim
      bbox <- sgdf@bbox
      r <- raster(xmn=bbox[1,1], xmx=bbox[1,2], ymn=bbox[2,1], ymx=bbox[2,2],
      ncols=dim[1], nrows=dim[2])
      r <- setValues(r,matrix(sgdf@data$band1, nrow = dim[1], ncol = dim[2]) %>% t())
      data <- rasterToPoints(r) %>% data.table()
      return(data)
   }
   arc_df = sgdf_transform(arc_period)
   
   # assign levels to data ####
   arc_df$z = cut(arc_df$layer, 
                           breaks = c(0, 5, 50,100, 200, 400, Inf), 
                           include.lowest = TRUE)
   
   # base plot ####
   gg <- ggplot() + 
      geom_map(data=atlas, map=atlas,
                       aes(x=long, y=lat, map_id=id),
                       color="black", fill="grey", alpha = .75, size=0.25) +
      
      geom_map(data = pmi_world, map = pmi_world,
                       aes(x=long, y=lat, map_id=id),
                       color="black", fill="white", alpha = 1 , size=0.25) + 
      
      geom_text(data = centroids, 
                       aes(x=x, y=y, label = abrev),
                       color="black", size= 2) + 
      
      coord_equal() + 
      theme_map() +
      theme(
         legend.position = c(0,0),
         legend.background = element_rect(fill=alpha('white', 0))
      ) + 
      scale_fill_manual(name = "Rainfall (mm)", 
                        values = c(NA, "#CFE1F2", "#93C4DE", "#4A97C9", "#1664AB", "#08306B")) +
      scale_alpha_manual(name = "Rainfall (mm)", 
                        values = c(0, .1, .20, .40, .60, .80)) + 
      
      theme(
           legend.position = c(0,0),
           legend.key.size = unit(1, "cm")
           )+ 
      xlim(min(arc_df$x), max(arc_df$x)) +
      ylim(min(arc_df$y), max(arc_df$y)) +
      coord_equal() 
   # final plot ####
   
   print( gg + 
             geom_tile( data= arc_df, aes(x, y, alpha = z), fill = "blue") +
             ggtitle( paste("Rainfall (mm) during", month, "month", year))
      )
}

plot_arc()

# Animation ####
library(animation)

oopt <- animation::ani.options(interval = 1)

FUN2 <- function() {
 for (i in 1:12){
   month = c("01","02","03","04","05","06","07","08","09","10","11","12")
   year = "2005"
   # cat( month[i], year, "\n")
   plot_arc(year, month[i])
 }
}


saveHTML(FUN2(), autoplay = FALSE, loop = FALSE, verbose = FALSE, 
         outdir = "animation",
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")

saveLatex(FUN2(), 
          img.name = "arc", ani.opts = "controls,loop,width=0.95\\textwidth", 
    latex.filename = ifelse(interactive(), "rainfall2005.tex", ""), 
    interval = 0.1, nmax = 12, ani.dev = "pdf", ani.type = "pdf", ani.width = 7, 
    ani.height = 7, documentclass = paste("\\documentclass{article}", 
        "\\usepackage[papersize={7in,7in},margin=0.3in]{geometry}", 
        sep = "\n"))

## the PDF graphics output is often too large because it is
## uncompressed; try the option ani.options('pdftk') or
## ani.options('qpdf') to compress the PDF graphics; see ?pdftk or
## ?qpdf and ?ani.options

# poster ####
library(gridExtra)
poster = arrangeGrob(FUN2(), ncol = 12)
ggsave("arc_poster.pdf", poster, dpi = 300, height=12, width = 64)
