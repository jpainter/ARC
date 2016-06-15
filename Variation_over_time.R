
# Rainfall variation over time

library(countrycode)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(scales)
library(RColorBrewer)
library(maptools)
library(rworldmap)
# require(rleafmap)
library(ggplot2)
library(ggvis)
library(data.table)
library(dplyr)
# data ####

   load('tr2.rda')
   load('tr2a.rda')
   # load('tr2a.map.rda')
   countries = unique(tr2$country)
   
vari = tr2 %>% 
   mutate( 
      date = as.Date( paste(year, month, "15", sep = "-"), format = "%Y-%m-%d"),
      region = countryRegions[ match(iso3, countryRegions$ISO3)  , "IMAGE24"]
           )    %>% 
   filter( grepl(  "Africa", region) ) %>% 
   group_by( region, date ) %>%
   summarise( 
      low = 100 * sum( ifelse( as.numeric(year.change.class) < 4, 1, 0)) / n() ,
      drought = 100 * sum( ifelse( as.numeric(year.change.class) < 2, 1, 0)) / n() ,
      high = 100 * sum( ifelse( as.numeric(year.change.class) > 4, 1, 0)) / n() ,
      flood = 100 * sum( ifelse( as.numeric(year.change.class) > 6, 1, 0)) / n() ,
      
      low.base = 100 * sum( ifelse( as.numeric(base.change.class) < 4, 1, 0)) / n() ,
      drought.base = 100 * sum( ifelse( as.numeric(base.change.class) < 2, 1, 0)) / n() ,
      high.base = 100 * sum( ifelse( as.numeric(base.change.class) > 4, 1, 0)) / n() ,
      flood.base = 100 * sum( ifelse( as.numeric(base.change.class) > 6, 1, 0)) / n() 
           ) 
   
str(vari)
 
ggplot(vari, aes(x = date, y = drought.base)) +
   geom_point( ) + 
   geom_smooth( color = "red") +
   labs(x = "Month", y = "Percent of months with rainfall < 85% year before") +
   scale_x_date(labels = date_format("%m-%Y")) +
   facet_grid( region ~ .)
ggsave("lowRain.png", width = 10, height = 5)

data = vari %>% mutate( year = year(date)) %>%
   filter(  year >2006 , year < 2016) %>%
   group_by( region, year ) 

labels = vari %>% mutate( year = year(date)) %>%
   filter( year >2006 , year < 2016) %>%
   group_by( region, year ) %>%
   summarise( mean = round( mean(drought.base), digits = 1 ))

   ggplot( data,  aes(drought.base)) +
      geom_histogram(  fill = "red" , color = 'red', binwidth = 5) +
      geom_text( x = 50, y = 4,  aes(label = mean), data = labels) +
      labs(x = "Count", x = "Percent of months with rainfall < 85% year before") +
      facet_grid( region ~ year)

ggsave("lowRain.png", width = 10, height = 5)


vari_m =  tr2 %>% 
   mutate( 
      date = as.Date( paste(year, month, "15", sep = "-"), format = "%Y-%m-%d"),
      region = countryRegions[ match(iso3, countryRegions$ISO3)  , "IMAGE24"]
   )    %>% 
   filter( grepl(  "Africa", region), year(date) >2006  ) %>% 
   group_by( region, date ) %>%
   arrange( date ) %>%
   mutate( index = row_number() )

library(rethinking)
library(BEST)
# lM
mlm = glm( base.change ~ index*region , family = gaussian, data = vari_m)
summary(mlm)
post <- extract.samples( mlm )
eff = (1 - exp(post$index)) * 100
plotPost(eff)


ggplot(vari, aes(x = date, y = flood.base)) +
   geom_point( ) + 
   geom_smooth(color = "green" ) +
   labs(x = "Month", y = "Percent of months with rainfall > 115% year before") +
   scale_x_date(labels = date_format("%m-%Y")) +
   facet_grid( region ~ .)
ggsave("highRain.png", width = 10, height = 5)

data = vari %>% mutate( year = year(date)) %>%
   filter(  year >2006 , year < 2016) %>%
   group_by( region, year ) 

labels = vari %>% mutate( year = year(date)) %>%
   filter( year >2006 , year < 2016) %>%
   group_by( region, year ) %>%
   summarise( mean = round( mean(flood.base), digits = 1 ))

ggplot( data,  aes(flood.base)) +
   geom_histogram(  fill = "green" , color = 'green', binwidth = 5) +
   geom_text( x = 20, y = 6,  aes(label = mean), data = labels) +
   labs(x = "Count", x = "Percent of months with rainfall > 115% year before") +
   facet_grid( region ~ year)
   