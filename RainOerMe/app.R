
# libraries ####

library(shiny)
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
   load('tr1.rda')
   load('tr2a.rda')
   # load('tr2a.map.rda')
   countries = unique(tr2$country)

   load("adm2.africa.gg.s")
   load("adm1.africa.gg.s")
   load("adm1.africa.centroids")
   
   # male sure adm1 (57) only has same countries as admin2 (46)
   adm1.africa.gg.s = adm1.africa.gg.s %>% filter( NAME_0 %in% adm2.africa.gg.s[, "NAME_0"] )
   
   # load("adm0.africa")
   # proj = proj4string(adm0.africa)
   proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

   # Africa
   world = getMap() %>% # transform to adm0.africa projection
   spTransform(CRS(proj))

   africa = fortify(world) %>% 
      mutate( iso3c = countrycode(id, "country.name", "iso3c"),
              country = countrycode(iso3c, "iso3c", "country.name")
              ) %>%
      filter(iso3c %in% countrycode_data[countrycode_data$continent == "Africa", "iso3c"]) %>%
      group_by( group )
      
   min.year = as.integer( min(tr2$year) )
   max.year = as.integer( max(tr2$year) )
   
# Define UI ####

ui <- shinyUI( 
      fluidPage(
         
   # Application title
   fluidRow(
                  column( 2, titlePanel("Rain O'er Me") ),
                  
                  column( 3,
                          selectInput("country", 
                              label = "Country:", 
                              choices = c(countries, "Africa"),
                              # choices = c(countries), 
                              selected = "Angola",
                              multiple = FALSE)
                          ),
                  
                  column( 3,
                          sliderInput("year",
                              label = "Year:",
                              min = min.year ,
                              max = max.year ,
                              value = max.year - 1 ,
                              step = 1,
                              sep = "",
                              animate = animationOptions(
                                 interval = 1500
                              )
                  )),
                  column( 3,
                          sliderInput("month",
                                      label = "Month:",
                                      min = 1,
                                      max = 12,
                                      value = 3 ,
                                      step = 1,
                                      animate = animationOptions(
                                         loop = TRUE,
                                         interval = 1500
                                      ),
                                      dragRange = TRUE
                          ))
                  
      ),
   fluidRow(
      column( 2,
              selectInput("measure", 
                          label = "Measurement:", 
                          choices = c("rain","year.change","base.change"), 
                          selected = "rain")
      ),
      
      column( 7,
              checkboxInput( "show_yearly_total", "Map yearly rainfall (uncheck for monthly rainfall)" , 
                             value = TRUE)
              )
      
   )
   # )
   ,
      
      mainPanel(
         fluidRow( 
            column( 6, uiOutput("map_ui"), ggvisOutput("map"),
                    "Map shows rainfall for selected year and month", br(), 
                    "The month/year of map corresponds with the year/month highlighted in histograms", br(),
                    "When 'Map yearly rainfall' is checked, only the average annual rainfall is shown"
                    ) ,
            # column( 1, NULL) ,
            column( 6, 
                    uiOutput("year_histo_ui"), ggvisOutput("year_histo") ,
                    uiOutput("month_histo_ui"), ggvisOutput("month_histo") 
            )
         ), width = 'auto'
      )
   ))
# ))
# Define server  ####
server <- shinyServer(function(input, output) {

   tr2r = reactive({
      
         if (is.na(input$country)){ return() }
      
         if (input$country %in% "Africa"){ 
            
               # tr2a %>% mutate( adm1 = NA, adm2 = NA )
            tr1
            
            } else {
               
               tr2 %>% filter( country %in% input$country )
            }
   })
   
   d = reactive({ 
      
      if (input$show_yearly_total){ 
            
         if (input$country %in% "Africa"){ 
            
            d = tr2r() %>%
               
               filter(
                  year == input$year 
               ) %>%
               
               group_by( country, adm1, year ) 
            
         } else {
            d = tr2r() %>%
               
                  filter(
                     year == input$year 
                     ) %>%
            
               group_by( country, adm1, adm2, year ) 
            
            
         }  
         
            d = d %>%
               summarise(
                  rain = mean( rain, na.rm = TRUE),
                  prev.year = mean( prev.year, na.rm = TRUE),
                  # year.change = mean( year.change, na.rm = TRUE),
                  base.rain = mean( base.rain, na.rm = TRUE)
                  # base.change = mean( base.change, na.rm = TRUE)

               ) %>%
               mutate(
                  
                  year.change = ifelse( prev.year > 0, rain / prev.year , NA),
                  
                  base.change = ifelse( base.rain > 0, rain / base.rain , NA),
                  
                  rain.class = cut(rain,
                  breaks = c(0, 1, 10, 25, 100, 200, Inf),
                  include.lowest = TRUE),

                  year.change.class = cut(year.change,
                  breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
                  include.lowest = TRUE),

                  base.change.class = cut(base.change,
                  breaks = c(0, .25, .67, .85, 1.15, 1.5, 4, Inf),
                  include.lowest = TRUE)
               )
            
      } else {
         
         d = tr2r() %>%
                  filter(
                     year == input$year , 
                     month %in% input$month
                     )
         
      }
      
      return(d)
  })
      
d.map = reactive({
   
   if (input$country %in% "Africa"){
      
      # d.map = africa %>% as.data.frame()  %>%
      #    left_join(
      #       d() %>% 
      #          mutate(iso3 = countrycode(country, "country.name", "iso3c")), 
      #       by = c("iso3c" = "iso3")
      #    ) 
      
      d.map = d()  %>% 
         left_join(
            adm1.africa.gg.s, 
            by = c("country" = "NAME_0", "adm1" = "NAME_1")
         ) 
      
   } else {
      d.map = d()  %>% 
      left_join(
         adm2.africa.gg.s, 
         by = c("country" = "NAME_0", "adm1" = "NAME_1", "adm2" = "NAME_2")
      ) 
   }

   return(d.map)
   
})

# map ####

map_vis = reactive({

      if (input$measure == "base.change"){
      .title = "% change"
      dm = d.map() %>% 
         mutate(
            fill_var = base.change.class
         ) 
      .colors  = c('#d73027','#fc8d59','#fee08b', '#ece7f2', '#d9ef8b','#91cf60','#1a9850')
      .labels = c( "0-25%", "25-67%", "67-85%", "85-115%", "115-150%", "150-400%", "400% +")
   
      } else if (input$measure == "year.change"){
      .title = "% change"
      dm = d.map() %>% 
         mutate( 
            fill_var = year.change.class
         )
      .colors  = c('#d73027','#fc8d59','#fee08b', '#ece7f2', '#d9ef8b','#91cf60','#1a9850')
      .labels = c( "0-25%", "25-67%", "67-85%", "85-115%", "115-150%", "150-400%", "400% +")
      
      } else if (input$measure == "rain"){
      .title = "Rain (mm/month)"
      dm = d.map() %>% 
         mutate(
            fill_var = rain.class
         )
  
      .labels = c("0", "1-10", "10-25", "25-100", "100-200", "200+")
      .colors = c("#fef0d9", "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837")

      }
   
   if (input$country %in% "Africa"){
      
      centroids = adm1.africa.centroids[0,]
      
      adm.map = adm1.africa.gg.s 
      
   } else {
      
      centroids = adm1.africa.centroids %>% 
         filter( NAME_0 %in% input$country )
      
      adm.map = adm1.africa.gg.s %>%
                      filter( NAME_0 %in% input$country ) %>%
                      group_by(group, id)
   }
   
   
   # d = d.map %>% mutate( f = rain.class ) 
      
   vis = dm %>%
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(strokeOpacity:=0.5,
                  stroke:="#7f7f7f",
                  opacity := 0.9,
                  fill = ~fill_var
                  ) %>%
      layer_paths( data = adm.map,
            strokeOpacity:=0.5, stroke:= "#252525"
                  ) %>%
      scale_ordinal('fill', range = .colors ) %>%
      add_legend(scales = 'fill', values = .labels, title = .title) %>%
      layer_text(data = centroids,
             x = ~x , y = ~y,
             text := ~NAME_1,
             baseline:="middle", fontSize:=8) %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      set_options(keep_aspect=TRUE, height = 600, width = 700) 
  
   # if ( !input$country %in% "Africa"){
      vis = vis %>% add_tooltip(hover_detail, "hover")
   # }
      
   
   return(vis)
})

   hover_detail <- function(x){
   
      if( is.null(x$id)) return(NULL)

      # paste( x[1,], collapse = " , " )
   
      # pick one of many map selected map rows (all have same rain data)
      row_id = which( d.map()$id==x$id )[1]

      if( length(row_id) == 0 ) return(NULL)

      detail <- d.map()[ row_id, ]

      if (input$country %in% "Africa"){ 
            paste0("<b>", detail$adm1, "</b><br>",
                      "This year (", detail$year, "): ", format(detail$rain, digits = 2), " mm/month<br>",
                      "Previous year: ", format(detail$prev.year, digits = 2), " mm/month<br>",
                      "<b>% of previous year: ", percent( detail$year.change ), "</b><br>",
                      "Baseline, 2000-2006: ", format(detail$base.rain, digits = 2), " mm/month<br>",
                      "<b>% of baseline: ", ifelse( is.na(detail$base.change), "",
                                                         percent( detail$base.change )
                                                         ), "</b>"
            ) 
         } else {
            paste0("<b>", detail[,"adm2"], ", ", detail$adm1, "</b><br>",
                   "This year (", detail$year, "): ", format(detail$rain, digits = 2), " mm/month<br>",
                   "Previous year: ", format(detail$prev.year, digits = 2), " mm/month<br>",
                   "<b>% of previous year: ", percent( detail$year.change ), "</b><br>",
                   "Baseline, 2000-2006: ", format(detail$base.rain, digits = 2), " mm/month<br>",
                   "<b>% of baseline: ", ifelse( is.na(detail$base.change), "",
                                                 percent( detail$base.change )
                   ), "</b>"
            )
         }
      
      
   } 
   
   map_vis %>% bind_shiny("map", "map_ui")
   
 

# HISTO VIS ####
   
   histo_detail <- function(x){
      
      if(is.null(x)) return(NULL)
      paste( x[1,1], " - ", x[1,2])
      # row_id = row_idx <- which(
      #                          tr2r()$year == x[1,1] &
      #                          tr2r()$month == round(x[1,2]) + 1
      #                          )
      # 
      # data <- tr2r()[ row_id, ] %>% filter( year %in% input$year) 
      # 
      # paste0("<b>", data[,"year"], "</b><br>",
      #           format(data[,"rain"], digits = 2), " mm<br>",
      #           "Previous year: ", format(data[,"prev.year"], digits = 2), " mm<br>",
      #           "<b>% of previous year: ", percent(data[,"year.change"]), "</b><br>",
      #           "Baseline, 2000-2006: ", format(data[,"base.rain"], digits = 2), " mm<br>",
      #           "<b>% of baseline: ", percent(data[,"base.change"]), "</b>"
      # )
   }  

   year_histo_vis = reactive({
   
      vis = tr2r %>% 
         filter( year < year(now()) ) %>%
         group_by( year ) %>%
         summarise(
            rain_total = sum( rain_total, na.rm = TRUE),
            rain_n = sum( rain_n, na.rm = TRUE) 
         ) %>%
         mutate(
            rain = ifelse(rain_n>0 , rain_total / rain_n, 0),
            selected = ifelse( year %in% input$year, 1, 0)
         ) %>% 
         group_by( selected ) %>%
         ggvis(~ year, ~ rain) %>%
         layer_bars( fill = ~ selected  ) %>%
         add_axis("y", title = "Mean ARC2 rainfall (mm/month)") %>%
         add_axis("x", title= "Year",  format="####") %>%
         hide_legend( "fill") %>%
         add_tooltip( histo_detail, "hover") %>%
         set_options( width = "auto", height = "300px" )
   
   return(vis)
})

   year_histo_vis %>% bind_shiny("year_histo", "year_histo_ui")
   
   month_histo_vis = reactive({
   
      vis = tr2r %>% 
         filter( year %in% input$year) %>%
         group_by( month ) %>%
         summarise(
            rain_total = sum( rain_total, na.rm = TRUE),
            rain_n = sum( rain_n, na.rm = TRUE)
         ) %>%
         mutate(
            rain = ifelse(rain_n>0, rain_total / rain_n, 0),
            selected = ifelse( month %in% input$month, 1, 0)
         ) %>% 
         group_by( selected ) %>%
         ggvis(~ month, ~ rain) %>%
         layer_bars( fill = ~selected ) %>%
         add_axis("y", title = "Mean ARC2 rainfall (mm/month)") %>%
         add_axis("x", title = "Month") %>%
         hide_legend( "fill") %>%
         add_tooltip( histo_detail, "hover") %>%
         set_options( width = "auto", height = "300px" )
   
   return(vis)
})

   month_histo_vis %>% bind_shiny("month_histo", "month_histo_ui")   
   
})

# Run the application  ####
shinyApp(ui = ui, server = server)

