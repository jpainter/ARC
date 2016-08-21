# Leaflet choropleth with rain matrix data from chirps

library(rgdal)
library(rgeos)
library(mapproj)
library(leaflet)
library(dplyr)

# load polygon maps 
   # load("../malaria_atlas/gadm/adm1.africa")
   # load("../malaria_atlas/gadm/adm1.africa.centroids")

### reduce contours of adm1 file ####
   
   # adm1.africa.s <- gSimplify(adm1.africa, tol=0.001, topologyPreserve=TRUE)
   # save( adm1.africa.s, file = 'adm1.africa.s')
   load( 'adm1.africa.s')
   adm1 = adm1.africa.s
   
# TODO: FUNCTION TO DOWNLOAD DAILY FILES AND STORE IN LARGE MATRIX
# load datasets.  it will have name 'd' ####

load("data/201501/africa_chirps201501") # 9.6 MB
class(d)
str(d@data)

# size test...
x = d@data
save(x, file = "d_data_test.rda") # 30.2 MB
xx = as.matrix(d@data)
save(xx, file = "d_data_matrix_test.rda") # 30.2 MB
xxx = as.integer(xx)
save(xxx, file = "d_data_integer_test.rda") # 5.9 MB  #TODO convert to matrix

assign("x", local(get(load("d_data_integer_test.rda") )) ) 
# we will add additional data to this slot
d <- SpatialPolygonsDataFrame(adm1, data=x )
         
# TODO: load master matrix (d)
# TODO: iterate through a monthly folder that is not already in d
## # TODO: check if  will need to updated chirps after initial release
# TODO: load monthly geotiff 
# TODO: convert to polygon structure for each/any simplified map

##### Use over() to convert raster data file into data.frame by adm, 
   # then combine back with spatial polygons ####
   # code from ARC_total_rainfall_adm1.R 
            
            # projection <- projection(adm1)
            
            d_over = over( adm1, d , returnList = TRUE)
            class(d_over)   # a list for each polygon
            
            # names to use for aggregating
            
            regions =  names(d_over) 
            nregions = length(regions)
            
            # function to split rownames into country and admin1
            vectorStringSplit = function(names = regions, part){
               unname(sapply(names, function(x) unlist(strsplit(x, "@"))[part]))
            }
            
            country = vectorStringSplit(part = 1)
            adm1_name = vectorStringSplit(part = 2)
            # adm2 = vectorStringSplit(part = 3)
            
            # see: rownames(arc_over_africa)[is.na(arc_over_africa)]
            # TODO: How to deal with missing areas. is adm1 too small for arc?
            
            getARCrows = function(x) as.numeric(rownames(d_over[[x]]))
            
  
   
   # Get ggregate value for each 'region' (in this case, region = adm1) as a vector
            
  

#### calculate arrary values for each day, month, and year, data array #####
            sum_ = function(x) sum(x, na.rm = TRUE)
            mean_ = function(x) mean(x, na.rm = TRUE)
            
            fun_names = c("sum_", "length", "mean_")
            funs <- lapply( fun_names,
               function(fname) eval(substitute(function(x)f(x), list(f=as.name(fname)))))
            
            dim3 = ncol(d@data)
            dim2 = nregions
            dim1 = length(funs)
            
            # create empty array
            darr <- array(, dim = c(dim1, dim2, dim3) )
            
            pb <- txtProgressBar(min = 0, max = dim1*dim3, style = 3)
            
            for (fun in 1:dim1)
            for(column in 1:ncol(d@data)){
              
              setTxtProgressBar(pb, column*fun)
               
              darr[fun, , column] <- sapply( 1:nregions, 
                                        function(x){
                                           if (nrow( d_over[[x]])>0){
                                              funs[[fun]]( d@data[ getARCrows(x), column] )
                                              } else { NA }
         }
            )
            }
            close(pb)
            
            str(darr)
            
            # round all values--set to integer
            darr =  array(as.integer(darr), dim = c(dim1, dim2, dim3) )
            
            # set column and row names
            dimnames(darr) = list( fun_names, regions, colnames(d@data) )
            
            # create tmp file for each function
            # add country and adm for each layer
            for (fun in 1:dim1){
               assign( paste0("tmp_", fun), bind_cols( as_data_frame( darr[fun, , ]), 
                                                   data_frame( country = country), 
                                                   adm1 = data_frame( adm1 = adm1_name )
                                                   )
               )
            }
               
          # save tmp files
            library(feather)
            write_feather(tmp_1, "jan2015chirps") # 36kb
            
            save(tmp_1, tmp_2, tmp_3, file = "jan2015chirps.rda") # 60kb
            
            load("jan2015chirps.rda")
      

            
###### join this dataset to a polygon file  #####
         
         # simplified version no longer has @data slot; add back while renaming dpoly
         # we will add additional data to this slot
         dpoly = spp <- SpatialPolygonsDataFrame(adm1, data=adm1.africa@data )
         
         # tmp_1 data set has sum of indiv rain estimates
         
         dpoly@data = left_join( dpoly@data, tmp_3, by = c("NAME_0" = "country", "NAME_1" = "adm1"))

         
         
#### dynamic variable selection:  ####
         var = 'month'
         dpoly@data$z = dpoly@data[, var]
##### plot ####

         
# function to calculate number of rainy days (>0mm) in month
   rain_days = function(min=10){
      rowSums( 
         apply(
            dpoly@data %>% select( starts_with("day")), 
            2, FUN = function(x) x>min)
         ) 
   }
              
   summary(rain_days()) 
   hist(rain_days(), freq = TRUE)
   
ncolors = 8
# pal <- colorQuantile("YlGn", domain = dpoly@data$z, n = ncolors, na.color = "#bdbdbd")
pal <- colorBin("YlGn", domain = dpoly@data$z, bins = ncolors, na.color = "#bdbdbd")


popup <- paste0("<strong>", dpoly@data$NAME_0, ", ", dpoly@data$NAME_1, "</strong>", 
                      "<br><strong>Monthly Rain(mm): </strong>", 
                      dpoly@data$z,
                "<br><strong>Rainy days (>5mm): </strong>",
                rain_days(1),
                "<br><strong>Rainy days (>20mm): </strong>",
                rain_days(20),
                "<br><strong>Rainy days (>40mm): </strong>",
                rain_days(40)
)

## AFRICA 
leaflet() %>%
  addProviderTiles("Stamen.Watercolor") %>% 
   
   # "CartoDB.Positron"
   # "Stamen.Watercolor"
   # "Stamen.TonerHybrid"
   
  addPolygons( data = dpoly,
              fillColor = ~pal(z),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup) %>%
   
   addLegend( pal = pal,
              values = dpoly@data$z,
              opacity = 0.7,
              position = 'bottomright',
              title = paste(quote(rain), 'mm/month') ) 

## Uganda
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
   
  addPolygons( data = dpoly[ dpoly@data$NAME_0 %in% "Uganda", ],
              fillColor = ~pal(z),
              fillOpacity = 0.8,
              color = "#BDBDC3",
              weight = 1,
              popup = popup) %>%
   
   addLegend( pal = pal,
              values = dpoly@data$z,
              opacity = 0.7,
              position = 'bottomright',
              title = paste(quote(rain), 'mm/month') ) 



