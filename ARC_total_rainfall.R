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


# Rainfall by pmi country ####
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

# CHART 


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

# rainfall by adm1, africa ####
   load("../malaria_atlas/gadm/adm1.africa")
   load("../malaria_atlas/gadm/adm1.africa.gg.s")
   load("../malaria_atlas/gadm/adm1.africa.s")
   load("../malaria_atlas/gadm/adm1.africa.centroids")

   total_rainfall_adm1 = NA #initialize object for rainfall summaary
   first = NA # marker for first record, used ot calculate map over function (slow)
   
   for (year in 2000:2015){
   year = as.character(year)
   for (i in 1:12){
      first = sum(first, 1, na.rm = TRUE)
         
      months = c("01","02","03","04","05","06","07","08","09","10","11","12")
      cat( months[i], year, "\n")
      
      period_file_name = paste0("africa_arc", year, months[i])
      arc_period = local(get(load( paste0('data/', year, months[i], '/', period_file_name) ) ) )
      
      # USe first rain map to determine relationship with adm1 
      # subsequent maps should have same geometry
      if ( first ==  1){ 
         # convert to same crs as adm1 map
         projection(arc_period) <- projection(adm1.africa)
         
         arc_over_africa = over( adm1.africa, arc_period , returnList = TRUE)
         regions =  names(arc_over_africa) 
         nregions = length(regions)
         
         # function to split rownames into country and admin1
         vectorStringSplit = function(names = region, part){
            unname(sapply(names, function(x) unlist(strsplit(x, "@"))[part]))
         }
         country = vectorStringSplit(part = 1)
         adm1 = vectorStringSplit(part = 2)
         
         # see: rownames(arc_over_africa)[is.na(arc_over_africa)]
         # TODO: How to deal with missing areas. is adm1 too small for arc?
         
         getARCrows = function(x) as.numeric(rownames(arc_over_africa[[x]]))
      }
      
      # claculate summary while applying geometry/translation from over to rain map
      
      t = sapply( 1:nregions, function(x){
         if (nrow( arc_over_africa[[x]])>0){
            sum( arc_period@data[ getARCrows(x), ] , na.rm = TRUE )
         } else { NA }
      }
         )
      
      n = sapply( 1:nregions, function(x){
         if (nrow( arc_over_africa[[x]])>0){
            sum(!is.na( arc_period@data[ getARCrows(x), ] ))
         } else { NA }
      }
         )
      
      mean = sapply( 1:nregions, function(x){
         if (nrow( arc_over_africa[[x]])>0){
            mean( arc_period@data[ getARCrows(x), ] , na.rm = TRUE )
         } else { NA }
      }
         )
      
      var = sapply( 1:nregions, function(x){
         if (nrow( arc_over_africa[[x]])>0){
            var( arc_period@data[ getARCrows(x), ] , na.rm = TRUE )
         } else { NA }
      }
         )
      
            
            tmp = data.frame( country = country , 
                              adm1 = adm1,
                              year = year, 
                              month = month(i) , 
                              rain_total = t , 
                              rain_n = n ,
                              rain = mean,
                              rain_var = var,
                              stringsAsFactors = FALSE
            )
            
            if (!is.data.frame(total_rainfall_adm1)){ 
               total_rainfall_adm1 = tmp
               } else {
               total_rainfall_adm1 = rbind( total_rainfall_adm1, tmp)
            }
      
   }
}

head(total_rainfall_adm1)
save(total_rainfall_adm1, file = 'total_rainfall_adm1.rda')

# CHART
load('total_rainfall_adm1.rda')

tr1 = total_rainfall_adm1 %>%
   mutate(
      rain.class = cut(rain, 
              breaks = c(0, 1, 10, 25, 100, 200, Inf), 
              include.lowest = TRUE),
      country = as.character(country), #convert from factor
      adm1 = as.character(adm1)
   )  %>% inner_join(
      adm1.africa.gg.s, by = c("country" = "NAME_0", "adm1" = "NAME_1")
   ) 


.year = 2015 
.month = 6 
.country = "Kenya"

d = tr1 %>%
         filter(
            year == .year, 
            month == .month
            ) 

if (!is.na(.country)){  
   d = d %>% filter( country %in% .country )
   }

# ggplot ####
ggplot() + 
   geom_polygon( 
      data = d , 
      aes( x= long, y= lat, group = group,
                                  fill = rain.class), 
                 colour="black", size=0.2, alpha = 0.75) +
 
      scale_fill_manual(name = "Avg.\nRainfall\n(mm)", 
                     values = c(NA, "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837"),
                     drop = FALSE) + 

      coord_equal() + 
      theme_map() +
      ggtitle( paste(.month, .year)) +
      theme(
         legend.key.size = unit(.5, "cm"),
         legend.position = "bottom" , # c(0,0),
         legend.background = element_rect(fill=alpha('white', 0))
      ) 
   

## ggvis ####
library(ggvis)
labels = c( "0", "1-10", "10-25", "25-100", "100-200", "200+")
colors = c(NA, "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837")

centroids = adm1.africa.centroids %>%
   filter(NAME_0 %in% .country)

hover_detail <- function(x){
 row <- d[d$id==x$id,]  %>%
     select(adm1, rain, rain_n, rain_var)  %>% unique
 paste0("<b>", row[,"adm1"], "</b><br>",
                row[,"rain"], "<br>",
                row[,"rain_n"], "<br>",
                row[,"rain_var"]
                )}   

d %>% 
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(strokeOpacity:=0.7, 
                  stroke:="#7f7f7f",
                  opacity := 0.9,
                  fill = ~rain.class
                  ) %>%
      scale_ordinal('fill', range = colors) %>%
      add_legend(scales = 'fill', values = labels, title = "Avg. Rain (mm)") %>%
      layer_text(data = centroids,
             x = ~x + 0.05, y = ~y,
             text := ~NAME_1,
             baseline:="middle", fontSize:=8) %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      set_options(keep_aspect=TRUE, height = 700, width = 700) %>%
      add_tooltip(hover_detail, "hover")

   