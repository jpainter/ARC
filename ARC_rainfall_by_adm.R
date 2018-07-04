# sum rainfall in each month
# packages  ####

library(data.table)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(countrycode)
library(tidyverse)

# rainfall by adm0, africa ####

adm_rainfall = function( adm = 0 , years = 2010:2018 ){
   
   # Load base maps
   base.filename = "../malaria_atlas/gadm/"
   
   adm.file.names = c(".africa" , ".africa.gg.s" , ".africa.s" ,".africa.centroids")

   # load adm maps 
   for ( .x in seq_along(adm.file.names) ){
      file.suffix  = adm.file.names[.x]
      obj = paste0( "adm",  file.suffix )
      assign(  obj , 
              readRDS( paste0( base.filename, "adm", adm,  file.suffix, ".RDS") ) 
              )
      }

   
   
   tmp = list()
   first = NA # marker for first record, used ot calculate map over function (slow)

   for ( year in years ){
      year = as.character(year)
      for (month in 1:12){
         
         cat( "adm-", adm , "month-" , year, month , "\n")
         
         first = sum(first, 1, na.rm = TRUE)
         
         period_file_name = paste0("africa_arc", year, month)
         
         if (file.exists( paste0('data/arc/', year, month, '/', period_file_name)  )){
            
            arc_period = local( 
               get(
                  load( 
                     paste0('data/arc/', year, month, '/', period_file_name) 
                  ) 
               ) 
            )
         } else { next }
   
         
         # USe first rain map to determine relationship with adm 
         # subsequent maps should have same geometry
         if ( first ==  1 ){ 
            # convert to same crs as adm map
            projection( arc_period ) <- projection( adm.africa )
            
            arc_over_africa = over(  adm.africa, arc_period , returnList = TRUE)
            regions =  names( arc_over_africa ) 
            nregions = length(regions)
            
            # function to split rownames into country and admin1
            vectorStringSplit = function(names = regions, part){
               unname(sapply(names, function(x) unlist(strsplit(x, "@"))[part]))
            }
            
            country = vectorStringSplit(part = 1)
            if ( adm >= 0 ) name1 = vectorStringSplit(part = 1)
            if ( adm >= 1 ){ name2 = vectorStringSplit(part = 2) } else { name1 = NA }
            if ( adm >= 2 ){  name3 = vectorStringSplit(part = 3) } else { name2 = NA }
            
            # see: rownames(arc_over_africa)[is.na(arc_over_africa)]
            # TODO: How to deal with missing areas. is adm0 too small for arc?
            
            getARCrows = function(x) as.numeric(rownames(arc_over_africa[[x]]))
         }
         
         # claculate summary while applying geometry/translation from over to rain map
         
         t = sapply( 1:nregions, function(x){
            if (nrow( arc_over_africa[[x]])>0){
               sum( arc_period@data[ getARCrows(x), 'month' ] , na.rm = TRUE )
            } else { NA }
         }
         )
         
         n = sapply( 1:nregions, function(x){
            if (nrow( arc_over_africa[[x]])>0){
               sum(!is.na( arc_period@data[ getARCrows(x), 'month' ] ))
            } else { NA }
         }
         )
         
         mean = sapply( 1:nregions, function(x){
            if (nrow( arc_over_africa[[x]])>0){
               mean( arc_period@data[ getARCrows(x), 'month' ] , na.rm = TRUE )
            } else { NA }
         }
         )
         
         var = sapply( 1:nregions, function(x){
            if (nrow( arc_over_africa[[x]])>0){
               var( arc_period@data[ getARCrows(x), 'month'  ] , na.rm = TRUE )
            } else { NA }
         }
         )
         
         index = paste0(year, month )
         
         
         tmp[[ index ]] = data.frame( 
            country = country , 
            iso3 = countrycode( country, "country.name" , "iso3c" ) ,
            NAME_0 = name0 ,
            NAME_1 = name1 , 
            NAME_2 = name2 , 
            year = rep( year, length(country) ) , 
            month = rep( month, length(country) )  , 
            total_rain = t , 
            rain_observations = n ,
            mean_rain = mean,
            rain_variance = var,
            stringsAsFactors = FALSE
         )
         
      }
      
   }
   
   arc_total_rainfall = rbindlist( tmp )
   
   print( paste0( "head arc_adm" , adm , "_rainfall.rds" ) )
   print( head( arc_total_rainfall ) )
   
   saveRDS( arc_total_rainfall , 
            file = paste0( "arc_adm", adm, "_rainfall.rds") )

}



