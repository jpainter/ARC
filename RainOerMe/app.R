
# libraries ####

library(shiny)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(scales)
library(RColorBrewer)
library(maptools)
require(rleafmap)
library(ggvis)
library(dplyr)
# data ####

   load('tr2.rda')
   load('tr2a.rda')
   # load('tr2a.map.rda')
   countries = unique(tr2$country)

   load("../../malaria_atlas/gadm/adm2.africa.gg.s")
   load("../../malaria_atlas/gadm/adm1.africa.gg.s")
   load("../../malaria_atlas/gadm/adm1.africa.centroids")

   # Africa
   world = getMap() %>% # transform to arc file datum
   spTransform(CRS("+proj=longlat +a=6371000 +b=6371000 +no_defs"))
   africa = fortify(world) %>% data.table() %>%
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
                  
                  column( 2,
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
                              label = "Month",
                              min = 1,
                              max = 12,
                              value = 6 ,
                              step = 1,
                              animate = animationOptions(
                                 loop = TRUE,
                                 interval = 1500
                                 ),
                              dragRange = TRUE
                  )),
                  
                  column( 2,
                          selectInput("measure", 
                              label = "Measurement:", 
                              choices = c("rain","year.change","base.change"), 
                              selected = "rain")
                  )
      )
   # )
   ,
      
      mainPanel(
         fluidRow( 
            column( 6, uiOutput("map_ui"), ggvisOutput("map") ) ,
            column( 1, NULL) ,
            column( 5, 
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
            
               tr2a 
            
            } else {
               
               tr2 %>% filter( country %in% input$country )
            }
   })
   
   d = reactive({ 
      
            d = tr2r() %>%
                  filter(
                     year == input$year , 
                     month %in% input$month
                     )
      
      return(d)
  })
      
d.map = reactive({
   
   if (input$country %in% "Africa"){
      
      d.map = africa %>%
         left_join(
            d() %>% 
               mutate(iso3 = countrycode(country, "country.name", "iso3c")), 
            by = c("iso3c" = "iso3")
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
      .title = "Rain, change from baseline 2000-2006"
      fill_var = "base.change.class"
      .colors  = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
      .labels = c( "0-25%", "25-75%", "75-100%", "100-150%", "150-500%", "500% +")
   
      } else if (input$measure == "year.change"){
      .title = "Rain, change from previous year"
      fill_var = "year.change.class"
      .colors  = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850')
      .labels = c( "0-25%", "25-75%", "75-100%", "100-150%", "150-500%", "500% +")

      } else if (input$measure == "rain"){
      .title = "Rain (mm)"
      fill_var = "rain.class"
      .labels = c( "0", "1-10", "10-25", "25-100", "100-200", "200+")
      .colors = c("#fef0d9", "#ffffe5", "#ffffbf", "#b8e186", "#4dac26", "#008837")

      }
   
   if (input$country %in% "Africa"){
      
      centroids = adm1.africa.centroids[0,]
      
      adm.map = africa 
      
   } else {
      
      centroids = adm1.africa.centroids %>% 
         filter( NAME_0 %in% input$country )
      
      adm.map = adm1.africa.gg.s %>%
                      filter( NAME_0 %in% input$country ) %>%
                      group_by(group, id)
   }
   
   
   # d = d.map %>% mutate( f = rain.class ) 
      
   vis = d.map %>%
      group_by(group, id) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(strokeOpacity:=0.5, 
                  stroke:="#7f7f7f",
                  opacity := 0.9,
                  fill = ~rain.class
                  ) %>%
      layer_paths( data = adm.map,
            strokeOpacity:=0.2, stroke:= "#252525"
                  ) %>%
      scale_ordinal('fill', 
                    range = .colors ) %>%
      add_legend(scales = 'fill', values = .labels, title = .title) %>%
      layer_text(data = centroids,
             x = ~x , y = ~y,
             text := ~NAME_1,
             baseline:="middle", fontSize:=8) %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      set_options(keep_aspect=TRUE, height = 600, width = 700) 
  
   if ( !input$country %in% "Africa"){
      vis = vis %>% add_tooltip(hover_detail, "hover")
   }
      
   
   return(vis)
})

   hover_detail <- function(x){
   
      if( is.null(x$id)) return(NULL)
   
      # pick one of many map selected map rows (all have same rain data)
      row_id = which( d.map()$id==x$id )[1] 
      
      if( length(row_id) == 0 ) return(NULL)
      
      detail <- d.map()[ row_id, ]  

      paste0("<b>", detail[,"adm2"], ", ", detail[,"adm1"], "</b><br>",
                "This year: ", format(detail[,"rain"], digits = 2), " mm<br>",
                "Previous year: ", format(detail[,"prev.year"], digits = 2), " mm<br>",
                "<b>% of previous year: ", percent(detail[,"year.change"]), "</b><br>",
                "Baseline, 2000-2006: ", format(detail[,"base.rain"], digits = 2), " mm<br>",
                "<b>% of baseline: ", percent(detail[,"base.change"]), "</b>"
      )
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
         group_by( year ) %>%
         summarise(
            rain_total = sum( rain_total, na.rm = TRUE),
            rain_n = sum( rain_n, na.rm = TRUE) / 12 # area repeated for each 12 months
         ) %>%
         mutate(
            rain = ifelse(rain_n>0, rain_total / rain_n, 0),
            selected = ifelse( year %in% input$year, 1, 0)
         ) %>% 
         group_by( selected ) %>%
         ggvis(~ year, ~ rain) %>%
         layer_bars( fill = ~ selected  ) %>%
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
         hide_legend( "fill") %>%
         add_tooltip( histo_detail, "hover") %>%
         set_options( width = "auto", height = "300px" )
   
   return(vis)
})

   month_histo_vis %>% bind_shiny("month_histo", "month_histo_ui")   
   
})

# Run the application  ####
shinyApp(ui = ui, server = server)

