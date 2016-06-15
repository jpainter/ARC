
# CHART
load('total_rainfall_adm2.rda')

# TODO : checkout one entry with NEGATIVE rain
# see View(tr2[ tr2$rain<0 & !is.na(tr2$rain),])

rain.baseline = # average for 2000-2006
   total_rainfall_adm2 %>% 
   filter( rain >= 0 ) %>%
   filter( year %in% 2000:2006 ) %>%
   group_by(country, adm1, adm2, month) %>%
   summarise( base.rain = mean( rain ),
              base.rain.var = var( rain )
              )

tr2 = total_rainfall_adm2 %>%
   filter( rain >= 0 ) %>%
   arrange(country, adm1, adm2, month, year) %>%
   mutate(
      
      prev.year = lag(rain, 1),
      
      year.change = ifelse( prev.year > 0 , 
                       rain  / prev.year,
                       rain
      ),
      
      rain.class = cut(rain,
              breaks = c(0, 1, 10, 25, 100, 200, Inf),
              include.lowest = TRUE),
      
      year.change.class = cut(year.change,
              breaks = c(0, .25, .75, 1, 1.5, 4, Inf),
              include.lowest = TRUE)

   )  %>%   
   
   left_join( rain.baseline ) %>%
   
   mutate(
      base.change = ifelse( base.rain > 0 , 
                       rain  / base.rain,
                       rain
      ),
     base.change.class = cut(base.change,
              breaks = c(0, .25, .75, 1, 1.5, 4, Inf),
              include.lowest = TRUE)      
   ) 

save(tr2, file = "tr2.rda")

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

## ggvis RAIN ####
library(ggvis)
labels = c( "0", "1-10", "10-25", "25-100", "100-200", "200+")
colors = c(NA, "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837")

centroids = adm1.africa.centroids %>%
   filter(NAME_0 %in% .country)

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


d.map %>% 
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(strokeOpacity:=0.7, 
                  stroke:="#7f7f7f",
                  opacity := 0.9,
                  fill = input_select(
                     choices = c("rain.class", "year.change.class"),
                     label = "Parameter"
                     )
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
