
# make base country maps WORLD, ATLAS, AFRICA


# world = getMap()
world = getMap() %>% # transform to arc file datum
   spTransform(CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs"))
# world = getMap() %>% spTransform(CRS("+proj=wintri"))
# plot(world)

# modify for ggplot (as dome for malaria_atlas)
atlas <- fortify(world) %>% data.table()
atlas <- atlas[id!='Antarctica',]
ggplot(atlas) + geom_polygon(aes(x=long, y=lat, group=group))

africa = atlas %>% 
   mutate( iso3c = countrycode(id, "country.name", "iso3c")) %>%
   filter(iso3c %in% countrycode_data[countrycode_data$continent == "Africa", "iso3c"]) %>%
ggplot() + geom_polygon( aes(x=long, y=lat, group=group) ) + 
   xlim(-25,60) + coord_equal(ratio=1)

# try same with gadm-adm0
load("../malaria_atlas/gadm/adm0.africa.gg.s")
adm0.africa.gg.s %>%
      filter(iso3 %in% countrycode_data[countrycode_data$continent == "Africa", "iso3c"]) %>%
      ggplot() + geom_polygon(aes(x=long, y=lat, group=group)) + 
      xlim(-25,60) + coord_equal(ratio=1)
# it is much  slower [~9 versus 1 seconds]

# highlight PMI and label
library(rgeos)
centroids = gCentroid(africa, byid=TRUE)  # calculate center of mass
# centroids <- spTransform(centroids, CRS("+proj=wintri")) 
centroids =  as.data.frame(centroids)
centroids$country = rownames(centroids)