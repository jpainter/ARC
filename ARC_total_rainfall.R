# sum rainfall in each month
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

pmi_world =  world[world$ADMIN.1 %in% pmi,] 


# ####
total_rainfall = NA

for (year in 2005:2014){
   year = as.character(year)
   for (i in 1:12){
      months = c("01","02","03","04","05","06","07","08","09","10","11","12")
      cat( months[i], year, "\n")
      
      period_file_name = paste0("africa_arc", year, months[i])
      arc_period = local(get(load( paste0('data/', year, months[i], '/', period_file_name) ) ) )
      projection(arc_period) <- CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs")
   
      # convert arc from raster to points
      arc_period_points = as(arc_period, "SpatialPointsDataFrame")
      
      arc_over_pmi = over( pmi_world, arc_period_points, fn = sum)
      arc_over_pmi_mean = over( pmi_world, arc_period_points, fn = mean)
      arc_over_pmi_n = over( pmi_world, arc_period_points, fn = length)
      
      rain = sum(arc_period$band1, na.rm = TRUE)
      rain_pmi = sum(arc_over_pmi$band1, na.rm = TRUE)
      
      for (each in 1:nrow(arc_over_pmi_mean)){ 
         
            tmp = data.frame( country = rownames(arc_over_pmi_mean)[each],
                              year = year, month = month(i), 
                              rain_pmi = arc_over_pmi[each, 'band1'], 
                              rain_pmi_mean = arc_over_pmi_mean[each, 'band1'])
            
            if (!is.data.frame(total_rainfall)){ 
               total_rainfall = tmp
               } else {
               total_rainfall = rbind( total_rainfall, tmp)
            }
         }
      
   }
}

head(total_rainfall)
save(total_rainfall, file = 'total_rainfall.rda')
load('total_rainfall.rda')

#    ggplot( data = total_rainfall, aes(color = year, y = rain, x = month)) +
#       geom_line()

# use country abbreviations
total_rainfall$abrev =  pmi_abrev[match(total_rainfall$country, pmi)]


# monthly rainfall
   monthly = 
      ggplot( data = total_rainfall, aes(color = year, 
                                         fill = year, 
                                         y = rain_pmi_mean, 
                                      x = factor(month)) ) +
      geom_bar(stat = 'identity') + labs( x = 'Month') +
      ylabs("Mean Monthly Rainfall (mm) / 11 sq km") +
      ggtitle("Mean Rainfall in PMI Countries") +
      facet_grid(abrev ~ ., scales = "free")
   
   ggsave("monthly_rainfall.pdf", dpi=300, height=9, width = 6)

# yearly rainfall
   ggplot( data = total_rainfall %>% group_by(country, abrev, year) %>% 
              summarize( yearly = sum(rain_pmi_mean)), 
           aes(y = yearly, x = year)) +
      geom_line( aes(group = country)) +
      ylab("Mean Yearly Rainfall (mm) / 11 sq km") +
      ggtitle("Mean Rainfall in PMI Countries") +
      # geom_bar(stat = 'identity') +
      facet_grid(abrev ~ .)
   
   ggsave("yearly_rainfall.pdf", dpi=300, height=11, width = 6)

