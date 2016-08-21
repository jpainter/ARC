#  package for busy indictor
# devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
# shinysky::run.shinysky.example()

# turning leaflet into module.  see 'leaflet choropleth.R'
library(leaflet)
library(dplyr)
library(sp)

      load("jan2015chirps.rda")
      load( 'adm1.africa')
      load( 'adm1.africa.s')
      adm1 = adm1.africa.s
      adm1 = spp <- SpatialPolygonsDataFrame(adm1, data=adm1.africa@data )
      

# Module UI function
leaflet_choropleth_UI <- function(id, label = "leaflet_choropleth") {
  # Create a namespace function using the provided id
  ns <- NS(id)
      
  fluidRow(
     column(12,
            selectInput(ns("country"), "Country", choices = unique(adm1@data$NAME_0),
                        selected = "Algeria")
  ),
   column(12,
         textOutput(ns("title"))
   ),
  column(12,
         leafletOutput(ns("choropleth")))
  )
}

# Module server function
leaflet_choropleth <- function(input, output, session, 
                               dataset = tmp_3, 
                               var = 'day24',
                               ncolors = 5,
                               title = "Title of map here...",
                               leaflet_style = "Stamen.Watercolor"
                               # "CartoDB.Positron"
                               # "Stamen.Watercolor"
                               # "Stamen.TonerHybrid"
                               ) {

  
  output$title = renderText(title)

  adm1@data = left_join( adm1@data, dataset, by = c("NAME_0" = "country", "NAME_1" = "adm1"))
  adm1@data$z = adm1@data[, var]
        
  dpoly <- reactive({
      
      if (length(input$country > 3)){
         d = adm1[ adm1@data$NAME_0 %in% input$country,]
      }

     return(d)
  })

  pal <- reactive({
     colorBin("YlGn", domain = dpoly()@data$z, bins = ncolors, na.color = "#bdbdbd")
})
  # function to calculate number of rainy days (>0mm) in month
   
   
  popup <- reactive({
     
     rain_days = function(min = 10){
      
      rowSums( 
         apply(
            dpoly()@data %>% select( starts_with("day")), 
            2, FUN = function(x) x >= min)
         ) 
     }
     
     paste0("<strong>", dpoly()@data$NAME_0, ", ", dpoly()@data$NAME_1, "</strong>", 
                      "<br>Rainfall(mm): ", 
                      dpoly()@data$z,
                "<br>Rainy days (>5mm): ",
                rain_days(5),
                "<br>Rainy days (>20mm): ",
                rain_days(20),
                "<br>Rainy days (>40mm): ",
                rain_days(40)
)
  })

   choro  = reactive({
      leaflet() %>%
              addProviderTiles( leaflet_style ) %>%

              addPolygons( data = dpoly(),
                          fillColor = ~pal()(z),
                          fillOpacity = 0.65,
                          color = "#BDBDC3",
                          weight = 1,
                          popup = popup()
                          )  %>%

               addLegend( pal = pal(),
                          values = adm1@data$z,
                          opacity = 0.7,
                          position = 'bottomright',
                          title = paste(quote(rain), 'mm/month') )
   })

  output$choropleth =   renderLeaflet({

     choro()

     })

}



