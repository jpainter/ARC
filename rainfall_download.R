
# noaa ARC2 geotiff climate data from http://www.cpc.ncep.noaa.gov/products/fews/data.shtml
# see: http://www.cpc.ncep.noaa.gov/products/fews/AFR_CLIM/arc2_201303_final.pdf
# example script from http://creativemorphometrics.co.vu/blog/2014/03/27/extracting-climate-data-in-r/

library(tidyverse)
library(RCurl)

# downloads list of daily files
get_file_list = function( source = c("arc", "chirps" ), period = "daily"){

# set internet source and download file locations  ####

if (tolower(source) == "arc" ){ 
   
   # ARC2 ####
   # download from ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/ 
   
   url <- "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/geotiff/" 
   filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
   filenames_split = unlist(strsplit(filenames, "\n" ))
   filenames_split = unlist(strsplit( filenames_split, "\r"))
   
   library(lubridate) # extract date
   file_list = strsplit(filenames_split, "[.]")
   file_dates = ymd(sapply(file_list, "[[", 2)) 
   
} 

if (tolower(source) == "chirps" ){ 
   
   # CHIRPS ####
   # monthly: ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/
   # decad: 
   # daily: ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/
   
   if ( period == "monthly"){
      
      url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/" 
      filenames <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
      filenames_split = unlist(strsplit(filenames, "\n" ))
      file_list = strsplit(filenames_split, "[.]")
      library(lubridate) # extract date
      file_dates = dmy(
         paste0( "01", sapply(file_list, "[[", 4), sapply(file_list, "[[", 3))
      )
   } else {
      # TODO: allow download from each--need to loop through folder directory
      if ( period == "daily"){ # 2015 only...
         url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/2015/" 
         filenames <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
         filenames_split = unlist(strsplit(filenames, "\n" ))
         file_list = strsplit(filenames_split, "[.]")
         library(lubridate) # extract date
         file_dates = ymd(
            paste0( sapply(file_list, "[[", 3), 
                    sapply(file_list, "[[", 4), 
                    sapply(file_list, "[[", 5)
            )
         )
      }
   } 
}
  
   file_list_df = data_frame( date = file_dates, file = filenames_split)
   return( list( url = url, files = file_list_df ) )
} 

# test
# file_list = get_file_list( "arc")
# file_list = get_file_list( "chirps" )


## downloads a month of daily geotiff files and aggregates them into one monthly file

download_years_rain_geotiff = function(
   source = "arc" , 
   period = "daily" ,
   remove = FALSE ,  # TODO: option to remove daily files
   replace = FALSE , # TODO: option to overwrite; skip download if exists
   year = 2016 ,
   file_list = NULL
   ){


# packages  ####
library(tidyverse)
library(stringr)
library(Hmisc)  # for monthDays function
library(data.table)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(maptools)
require(rleafmap)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(ggthemes)
library(RCurl)
library(R.utils)

# file list ####
   
   if (is.null( file_list)) file_list = get_file_list( source = source, period = period )
   
   start_date = paste0( year, "01" , "01")
   
   end_date = paste0( year, '12' , '31')
   
   date_interval = interval( ymd(start_date), ymd(end_date) )
   
   files_to_download = file_list$files %>% filter( date %within% date_interval )
   num_files = nrow(files_to_download)
   
   url = file_list$url
   

# download files   #### 
   for (i in 1:num_files){
      
      file_date = files_to_download[i,]$date
      
      folder_name = paste0("data/", source, "/" , year(file_date), month(file_date))
      
      cat( paste(i, files_to_download[i, 'file']), sep = "\n") ;flush.console()
      
      remote_file =  files_to_download[i, 'file']
      
      download_file = paste0(folder_name, "/", remote_file ) 
      
      if (!dir.exists(folder_name)) dir.create(folder_name) 
      
      # Redownload or not
      already_downloaded = file.exists( download_file )
      if ( already_downloaded & replace ){ 
         print( 'file already downloaded')
         next } # do not redownload 
      
      
      con <- file(download_file, open = "wb")
      
      cat(" downloading file...\n") ;flush.console()
      
      repeat{
         success = TRUE
         
         bin <- tryCatch(
            {
               getBinaryURL( paste0( url, remote_file ), ssl.verifypeer=FALSE)
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
      
      # cat("   unzipping file...\n") ;flush.console()
      # unzip(zip_file, exdir = folder_name)
      cat("done \n \n") ;flush.console()
      
   }
}


# downloaded datsets ####
for ( year in 2010:2017 ){
   
   download_years_rain_geotiff( year = year, source = "arc")
   
}

aggregate_geotiff_monthly = function( 
   source = "arc" , 
   period = "daily" ,
   remove = FALSE ,  # TODO: option to remove daily files
   replace = FALSE , # TODO: option to overwrite; skip download if exists
   year = NULL ,
   month = NULL ,
   file_list = NULL
   ){
   
   # packages  ####
   library(tidyverse)
   library(stringr)
   library(Hmisc)  # for monthDays function
   library(data.table)
   library(lubridate)
   library(raster)
   library(sp)
   library(rgdal)
   library(scales)
   library(RColorBrewer)
   library(ggthemes)
   library(maptools)
   require(rleafmap)
   library(rworldmap) # this pkg has waaaay better world shapefiles
   library(ggthemes)
   library(RCurl)
   library(R.utils)
   
   # file list ####
   
   if (is.null( file_list)) file_list = get_file_list( source = source, period = period )
   
   start_date = paste( year, month , 1, sep = '-')
   
   end_date = paste( year, month, days_in_month( ymd(start_date )) , sep = '-' )
   
   date_interval = interval( ymd(start_date), ymd(end_date) )
   
   files_to_download = file_list$files %>% filter( date %within% date_interval )
   
   num_files = nrow(files_to_download)
   
   file_date = files_to_download$date
   
   folder_name = unique( paste0("data/", source, "/" , year(file_date), month(file_date)) )
   
   period_file_name = paste0("africa_", source, year, month)
   
   # Skip if aggregate already exists #### 
   if ( file.exists( paste0( folder_name, "/", period_file_name)  ) & replace == FALSE ){ return( "Aggregate file alread exists \n") }
   
   # matrix of monthly values ####
   
   if ( source == "chirps"){
      rain = matrix(, 2400000, num_files)
   } else{
      if (source == "arc")
         rain = matrix(, 601551, num_files) 
   }
   
   
   colnames(rain) = paste0( "day", 1:num_files)
   
   # combine daily files into monthly ####
   if ( period == 'daily'){ 
      for (i in 1:num_files){  
         
         remote_file =  files_to_download[i, 'file']
         
         download_file = paste0( folder_name, "/", remote_file ) 
         
         cat("   unzipping", download_file, "...\n") ;flush.console()
         if ( source == "arc"){
            
            tiff_file = unzip( download_file , exdir = tempdir()) # don't write uncompressed file
         
            } else {
               
            if ( source == "chirps"){
               tiff_file = gunzip( download_file , 
                                   temporary = TRUE, overwrite = TRUE,
                                   remove = remove) # keep original files while testing...
            }
         }
         
         
         d = try(
            readGDAL(tiff_file, silent = TRUE)
         ) 
         
         if ( class(d) == "try-error" ){
            
            cat("file not found, or something...\n" )
            next
         }
         
         # put values in matrix with column corresponding to day
         rain[, paste0("day", i)] = d@data$band1
         
         cat(summary(rain[, paste0("day", i)])); cat("\n")
         
      }
   }
   
   #bind matrix of rain totals to spatial polygon
   d@data = cbind( d@data, rain )
   
   # remove original 'band1' column
   d@data = d@data[, -1]
   
   # remove these from chirps datafile...
   d@data[ d@data == -9999 ] <- NA 
   
   rain_total = rowSums( d@data, na.rm = TRUE )
   cat( "MONTHLY TOTAL",  summary(rain_total) ); flush.console()
   
   # Add total rain data 
   
   d@data$month = rain_total # add in total
   
   # write summary to file ####
   cat("writing file"); flush.console()
   save( d, file = paste0( folder_name, "/", period_file_name) )
}


# make monthly summary file  ####
for ( year in 2010:2017 ){
   for ( month in 1:12 ){
      
      aggregate_geotiff_monthly(source = 'arc', year = year, month = month)
   }
}


# test
# aggregate_geotiff_monthly(source = 'arc', year = 2016, month = 2 )
# yearly_rain_geotiff( source = 'chirps', "2014")




# AFter downloading and aggregating, need to create summary file with country adm2: ####
# RUN ARC_total_rainfall_adm2.R


# load data ####
year = "2015"
month = "01"
source = "chirps"
folder_name = paste0("data/", year, month)
period_file_name = paste0("africa_", source, year, month)
file = paste0( folder_name, "/", period_file_name)
# assign(period_file_name, local(get(load(period_file_name))) )
period = local(get(load(file))) 


# country boundaries ####
# bounds <- readOGR(dsn="../TM_WORLD_BORDERS-0.3", 
#                   layer= "TM_WORLD_BORDERS-0.3") #import borders

# world = getMap()
world = getMap() %>% # transform to arc file datum
   spTransform(CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs"))
# world = getMap() %>% spTransform(CRS("+proj=wintri"))
# plot(world)

# modify for ggplot (as dome for malaria_atlas)
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

# QUICK plot as spatial lines  (base R)     ####    
ac = as.image.SpatialGridDataFrame(period)
Dcl <- contourLines(ac, nlevels = 8)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl)  # convert to SpatialLinesDataFrame

proj4string(SLDF) = CRS("+proj=longlat")
s = spTransform(SLDF, CRS("+proj=wintri"))
plot(s)

plot(SLDF, col = terrain.colors(8))
plot(world, add = TRUE)    # TODO: not working with rworldmaps      

# plot as spatial points  (ggplot)   SLOW!   ####     
# ac.grid = as.image.SpatialGridDataFrame(arc_period) 
ac.points = as.ppp.SpatialPoints(period) %>% as.data.frame() %>% 
   mutate(z = as.numeric(period@data$month))
plot(ac.points)

# filter to smaller region
acp = ac.points %>% filter( findInterval(x, 17:18) == 1, findInterval(y, 0:1) == 1 )
ggplot() + geom_point( data=acp, aes( x=x, y=y, color=z) ) # SLOW!

# ggplot spatialGridDataFrame as tiles ####
# from https://rpubs.com/tmieno2/68747:
# As a SpatialGridDataFrame is not compatible with mapping using ggplot( ), 
# we need to convert it into a data frame, 
# in which each observation is assigned the longitude and latitude 
# of the centroid of its corresponding grid. 
# We can achieve this with the following code 
# (If you would like to know the inner works of the function, 
# please run the code line by line),

sgdf_transform = function(sgdf){
   dim <- sgdf@grid@cells.dim
   bbox <- sgdf@bbox
   r <- raster(xmn=bbox[1,1], xmx=bbox[1,2], ymn=bbox[2,1], ymx=bbox[2,2], 
               ncols=dim[1], nrows=dim[2])
   r <- setValues(r,matrix(sgdf@data$month, nrow = dim[1], ncol = dim[2]) %>% t()) 
   data <- rasterToPoints(r) %>% data.table()
   return(data)
}

period_df = sgdf_transform(period) %>%
   # assign levels to data ####
mutate(
   z = cut(layer, 
           breaks = c(10, 50, 100, 200, Inf), 
           include.lowest = FALSE) 
) 

# for speed, select area... (roughly DRC)
# filter( findInterval( x, c(15,30) ) == 1, findInterval( y, c(-10, 0) ) == 1 ) 

# quick version
ggplot() + 
   geom_raster( data = arc_df, aes( x= x, y= y, fill = z)) +
   scale_fill_brewer() +
   geom_path(data = atlas, aes( x=long, y=lat, group = group)) +
   xlim(-20, 55) + ylim(-40, 40) 

# complete version
gg <- ggplot() + 
   # base map: atlas
   geom_map(data=atlas, map=atlas,
            aes(x=long, y=lat, map_id=id),
            color="grey", fill="grey", alpha = .75, size=0.25) +
   
   geom_map(data = pmi_world, map = pmi_world,
            aes(x=long, y=lat, map_id=id),
            color="black", fill="white", alpha = 1 , size=0.25) + 
   
   geom_text(data = centroids, 
             aes(x=x, y=y, label = abrev),
             color="black", size= 2) + 
   # rainfall
   geom_raster( data = arc_df, aes( x= x, y= y, fill = z), alpha = 0.5) +
   # scale_fill_brewer() +
   scale_fill_manual(name = "Rainfall (mm)", 
                     values = c("#e0f3f8", "#ffffbf", "#b8e186", "#4dac26", "#008837")) + 
   # see color ramp
   # colorRampPalette(brewer.pal(9,"Blues"))(6)
   
   # theme
   coord_equal() + 
   theme_map() +
   # xlim(min(arc_df$x), max(arc_df$x)) +
   # ylim(min(arc_df$y), max(arc_df$y)) +
   theme(
      legend.key.size = unit(1, "cm"),
      legend.position = c(0,0),
      legend.background = element_rect(fill=alpha('white', 0))
   ) 

gg + xlim(-20, 55) + ylim(-40, 40) 




# FUNCTION for LOAD and PLOT ####

plot_period=  function( year = "2015", month = "01", source = "chirps"){
   
   period_file_name = paste0("africa_", source, year, month) 
   
   period = local(get(load( paste0('data/', year, month, '/', period_file_name) ) ) )
   projection(period) <- CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs")
   
   sgdf_transform = function(sgdf){
      dim <- sgdf@grid@cells.dim
      bbox <- sgdf@bbox
      r <- raster(xmn=bbox[1,1], xmx=bbox[1,2], ymn=bbox[2,1], ymx=bbox[2,2],
                  ncols=dim[1], nrows=dim[2])
      
      # note choice of @data column (e.g. month)
      r <- setValues(r, matrix(sgdf@data$month, nrow = dim[1], ncol = dim[2]) %>% t())
      data <- rasterToPoints(r) %>% data.table()
      return(data)
   }
   
   period_df = sgdf_transform(period)
   
   # arc_df = fortify(arc_period)
   
   # assign levels to data 
   period_df$z = cut(period_df$layer, 
                     breaks = c(0, 50, 100, 200, Inf), 
                     include.lowest = TRUE)
   
   # base plot 
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
                         values = c(0, .1, .30, .60, .90)) + 
      
      theme(
         legend.position = c(0,0)
         ,
         legend.key.size = unit(.3, "cm")
      )+ 
      xlim(-20.05, 55.05) +
      ylim(-40.05, 40.05) 
   
   # final plot 
   
   gg = gg + 
      geom_tile( data = period_df, aes(x, y, alpha = z), fill = "blue") +
      ggtitle( paste("Rainfall (mm) during", month, "month", year))
   
   return(gg)
}

plot_period(source = 'chirps')

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
.year = "2011"
pdf( paste0("arc_poster_", .year, ".pdf"), height=12, width = 8.5)
grid.arrange(
   plot_arc(year = .year, month = "01") ,
   plot_arc(year = .year, month = "02") ,
   plot_arc(year = .year, month = "03") ,
   plot_arc(year = .year, month = "04") ,
   plot_arc(year = .year, month = "05") ,
   plot_arc(year = .year, month = "06") ,
   plot_arc(year = .year, month = "07") ,
   plot_arc(year = .year, month = "08") ,
   plot_arc(year = .year, month = "09") ,
   plot_arc(year = .year, month = "10") ,
   plot_arc(year = .year, month = "11") ,
   plot_arc(year = .year, month = "12") ,
   ncol = 3
)
dev.off()
