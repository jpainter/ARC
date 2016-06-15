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
library(countrycode)



   # rainfall by adm2, africa ####
   load("../malaria_atlas/gadm/adm2.africa")
   load("../malaria_atlas/gadm/adm2.africa.gg.s")
   load("../malaria_atlas/gadm/adm1.africa.gg.s")
   load("../malaria_atlas/gadm/adm1.africa.centroids")

   total_rainfall_adm2 = NA #initialize object for rainfall summaary
   first = NA # marker for first record, used to calculate map over function (slow)
   
   for (year in 2000:year( now() )){
   # NB: the first month is slow, then it moves quite quickly
      
      year = as.character(year)
      for (i in 1:12){
         first = sum(first, 1, na.rm = TRUE)
         
         if ( year == as.character( year( now() )) && i>= month( now() ) ){ next }
         
         months = c("01","02","03","04","05","06","07","08","09","10","11","12")
         cat( months[i], year, "\n")
         
         period_file_name = paste0("africa_arc", year, months[i])
         arc_period = local(get(load( paste0('data/', year, months[i], '/', period_file_name) ) ) )
         
         # USe first rain map to determine relationship with adm1 (slow)
         # subsequent maps should have same geometry
         if ( first ==  1){ 
            # convert to same crs as adm1 map
            projection(arc_period) <- projection(adm2.africa)
            
            arc_over_africa = over( adm2.africa, arc_period , returnList = TRUE)
            regions =  names(arc_over_africa) 
            nregions = length(regions)
            
            # function to split rownames into country and admin1
            vectorStringSplit = function(names = regions, part){
               unname(sapply(names, function(x) unlist(strsplit(x, "@"))[part]))
            }
            country = vectorStringSplit(part = 1)
            adm1 = vectorStringSplit(part = 2)
            adm2 = vectorStringSplit(part = 3)
            
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
                                 adm2 = adm2,
                                 year = year, 
                                 month = month(i) , 
                                 rain_total = t , 
                                 rain_n = n ,
                                 rain = mean,
                                 rain_var = var,
                                 stringsAsFactors = FALSE
               )
               
               if (!is.data.frame(total_rainfall_adm2)){ 
                  total_rainfall_adm2 = tmp
                  } else {
                  total_rainfall_adm2 = rbind( total_rainfall_adm2, tmp)
               }
      
   }
}

head(total_rainfall_adm2)

# TODO : checkout one entry with NEGATIVE rain
# see View(tr2[ tr2$rain<0 & !is.na(tr2$rain),])

save(total_rainfall_adm2, file = 'total_rainfall_adm2.rda')
save(total_rainfall_adm2, file = 'RainOerMe/total_rainfall_adm2.rda')

# Calculate change in rainfall from previous year, and baseline years, for each adm.  ####

load('total_rainfall_adm2.rda') # rainfall by month for each adm.

rain.baseline = # average for 2000-2006
   total_rainfall_adm2 %>% 
   filter( rain >= 0 ) %>%
   filter( year %in% 2000:2006 ) %>%
   group_by(country, adm1, adm2, month) %>%
   summarise( base.rain_total = sum( rain_total, na.rm = TRUE),
              base.rain_n = sum( rain_n, na.rm = TRUE)
              ) %>%
   mutate(
      base.rain = ifelse( base.rain_n > 0, base.rain_total / base.rain_n, NA)
   )

tr2 = total_rainfall_adm2 %>% 
   filter( rain >= 0 ) %>%
   mutate( year = as.numeric(year),
           iso3 = countrycode(country, "country.name", "iso3c")
           ) %>%
   arrange(country, adm1, adm2, month, year) %>%
   group_by(country, adm1, adm2, month) %>%
   
   mutate(
      
      prev.year = lag(rain, 1 , order_by = year )  ) %>% as.data.frame()  %>%
   
   mutate(
      
      year.change = ifelse( prev.year > 0 , 
                       rain  / prev.year,
                       rain
      )   ,
      
      rain.class = cut(rain,
              breaks = c(0, 1, 10, 25, 100, 200, Inf),
              ordered = TRUE, 
              include.lowest = TRUE)  ,
      
      year.change.class = cut(year.change,
              breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
              ordered = TRUE, 
              include.lowest = TRUE)

   )  %>%   
   
   left_join( rain.baseline ) %>%
   
   mutate(
      base.change = ifelse( base.rain > 0 , 
                       rain  / base.rain,
                       rain
      ),
      
      base.change.class = cut(base.change,
              breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
              ordered = TRUE, 
              include.lowest = TRUE)
      
   ) 

# View(tr2[1:200000,])

save(tr2, file = "tr2.rda")
save(tr2, file = "RainOerMe/tr2.rda")


# Same at country level for continent scale map  ####

   rain.baseline.country = # average for 2000-2006
      total_rainfall_adm2 %>% 
      filter( rain >= 0 ) %>%
      filter( year %in% 2000:2006 ) %>%
      group_by(country, month) %>%  
   summarise( base.rain_total = sum( rain_total, na.rm = TRUE),
              base.rain_n = sum( rain_n, na.rm = TRUE)
              ) %>%
   mutate(
      base.rain = ifelse( base.rain_n > 0, base.rain_total / base.rain_n, NA)
   )

   load("tr2.rda")
   tr2a = tr2 %>% 
               group_by( country, year, month ) %>%
               summarise(
                  rain_total = sum( rain_total,  na.rm = TRUE),
                  rain_n = sum( rain_n,  na.rm = TRUE)
               ) %>%
               left_join( rain.baseline.country ) %>%
               mutate(  rain = ifelse(rain_n>0, rain_total / rain_n, 0),
                        
                        prev.year = lag(rain, 1), 
                        
                        year.change = ifelse( prev.year > 0 , 
                                              rain  / prev.year,
                                              rain),
                        
                        rain.class = cut(rain,
                                         breaks = c(0, 1, 10, 25, 100, 200, Inf),
                                         include.lowest = TRUE),
                        
                        year.change.class = cut(year.change,
                                                breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
                                                include.lowest = TRUE),
                        
                        base.change = ifelse( base.rain > 0 , 
                                              rain  / base.rain,
                                              rain),
                        
                        base.change.class = cut(base.change,
                                                breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
                                                ordered = TRUE, 
                                                include.lowest = TRUE)

                        )  %>% as.data.frame()

   save(tr2a, file = "tr2a.rda")
   save(tr2a, file = "RainOerMe/tr2a.rda")


# CHARTs #####      
   # join with adm1 map
   load("../malaria_atlas/gadm/adm0.africa.gg.s")
   
   # TODO -  this version takes very long to print as map.
   # tr2a.map = tr2a %>%
   #    left_join(
   #       adm0.africa.gg.s, 
   #       by = c("country" = "NAME_0")
   #    ) %>% as.data.frame()
   # 
   # save(tr2a.map, file = "tr2a.map.rda")   

.year = 2015 
.month = 6 
.country = "Kenya"

d = tr2 %>%
         filter(
            year == .year, 
            month == .month
            ) 

if (!is.na(.country)){  
   d = d %>% filter( country %in% .country )
}

d.map = d  %>% 
   left_join(
      adm2.africa.gg.s, 
      by = c("country" = "NAME_0", "adm1" = "NAME_1", "adm2" = "NAME_2")
   ) 


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
   

## ggvis RAIN ####
library(ggvis)
labels = c( "0", "1-10", "10-25", "25-100", "100-200", "200+")
colors = c(NA, "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837")

centroids = adm1.africa.centroids %>%
   filter(NAME_0 %in% .country)

hover_detail <- function(x){
 row <- d[d$id==x$id,]  %>%
     select(adm2, rain, rain_n, rain_var)  
 paste0("<b>", row[,"adm2"], "</b><br>",
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
      layer_paths( data = adm1.africa.gg.s %>% 
                      filter( NAME_0 %in% .country ) %>% 
                      group_by(group, id), 
            strokeOpacity:=0.9, stroke:= "grey"
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

# CHANGE: ####
change.colors  = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
labels = c( "0-25%", "25-75%", "75-100%", "100-150%", "150-500%", "500% +")

hover_detail <- function(x){
 row <- d.map[d.map$id==x$id,]  %>%
     select( adm2, rain, prev.year, year.change, base.rain, base.change) %>% unique
 paste0("<b>", row[,"adm2"], "</b><br>",
                "This year: ", format(row[,"rain"], digits = 2), " mm<br>",
                "Previous year: ", format(row[,"prev.year"], digits = 2), " mm<br>",
                "<b>% of previous year: ", percent(row[,"year.change"]), "</b><br>",
                "Baseline, 2000-2006: ", format(row[,"base.rain"], digits = 2), " mm<br>",
                "<b>% of baseline: ", percent(row[,"base.change"]), "</b>"
                )}   

d.map %>% 
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(strokeOpacity:=0.7, 
                  stroke:="#7f7f7f",
                  opacity := 0.9,
                  fill = ~year.change.class
                  ) %>%
      layer_paths( data = adm1.africa.gg.s %>% 
                      filter( NAME_0 %in% .country ) %>% 
                      group_by(group, id), 
            strokeOpacity:=0.9, stroke:= "grey"
                  ) %>%
      scale_ordinal('fill', range = change.colors) %>%
      add_legend(scales = 'fill', values = labels, title = "Rain change") %>%
      layer_text(data = centroids,
             x = ~x + 0.05, y = ~y,
             text := ~NAME_1,
             baseline:="middle", fontSize:=8) %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      set_options(keep_aspect=TRUE, height = 700, width = 700) %>%
      add_tooltip(hover_detail, "hover")
