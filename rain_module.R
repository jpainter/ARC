# rainfall widget

# packages ####

   suppressMessages( library(leaflet) ) 
   
   suppressMessages( library(tidyverse) )
   suppressMessages( library(lubridate) )
   suppressMessages( library(countrycode) )
   
   suppressMessages( library(sp) )
   suppressMessages( library(shiny) )
   
   library(DT)
   library(dygraphs)
   library(htmlwidgets)
   library(webshot)
   

# dataset ####

      
   load( 'adm1.africa')
   load( 'adm1.africa.s')
   adm1 = adm1.africa.s
   adm1 = spp <- SpatialPolygonsDataFrame(adm1, data=adm1.africa@data )
   
   adm1_rain = readRDS("arc_adm1_rainfall.rds")
   
   adm1_rain$date = ymd( paste( adm1_rain$year, adm1_rain$month, 1, sep = "-"))
   
   choices_month <- format(seq.Date(from = min(adm1_rain$date),
                                    to = max(adm1_rain$date),
                                    by = "month" ), "%Y-%B")
   
   vars = colnames(  adm1_rain )
   
   

# UI functions ####
   
   rain_map_UI = function(id, country = "Benin", dataset = NULL) {
      
      # Create a namespace function using the provided id
      ns <- NS(id)
      
      country.iso3  = countrycode( country, "country.name", "iso3c")
      
      fillCol(  height = "100%" , #  height = 600,
                flex = c(NA, 1),
                
                fluidPage(
                   fluidRow( # style = "padding-bottom: 20px;",
                             column(3,
                                    
                                    selectInput( ns("var"), "Variable", 
                                                 choices = vars,
                                                 selected = 'rain' 
                                    )
                                    ),
                              column( 6,     
                                    
                                    sliderInput(
                                       inputId = ns("date_range") ,
                                       label = "Choose Month:",
                                       width = "100%",
                                       value = c(min(adm1_rain$date), max(adm1_rain$date)) ,
                                       min = min(adm1_rain$date),
                                       max = max(adm1_rain$date),
                                       step = 30 ,
                                       timeFormat = "%Y-%m",
                                       animate = animationOptions(interval = 1500)
                                    )
                                    
                                    ),
                             
                              column(3,    
                                    downloadLink('maplink' , 'Save map' ),
                                    
                                    downloadLink('dylink' , 'Save time series chart' )
                                    
                             ))  ,
                
                # leafletOutput( ns('mymap'), width = "50%", height = "50%"  ) ,
                # 
                # dygraphOutput( ns("dygraph"), width = "50%", height = "100%" )
                             
                      fixedRow( style='padding:0px;' ,

                                    column(6,  offset = 0, style='padding:0px;', 
                                            leafletOutput( ns('mymap') )
                                            # ,
                                            # textOutput( ns('selected'))
                                            # ,
                                            # dataTableOutput( ns('admin'))
                                            ),
                                    column(6, offset = 0, style='padding:0px;', 
                                           dygraphOutput( ns("dygraph") )
                                           # ,
                                           # dataTableOutput( ns("dataset" ))
                                           )

                             )
                
      )
      )
}
                
                
# Server functions ####
   
   rain_map_Output = function(  input, output, session ,
                                
                               dataset = NULL,
                               country = "Angola" ,
                               var = 'day24',
                               ncolors = 5,
                               title = "Rainfall",
                               leaflet_style = "Stamen.TonerLite" 
   ) {
  
        output$title = renderText(title)

        data <- reactive({
           
          # country
           if ( country() %in% 'Africa'){
              
              data =  dataset 
                                           
           } else {
              
              data =  dataset %>%  filter( country %in% country() )
           }
           
           # date range
           input_date1 = input$date_range[1]
           input_date2 = input$date_range[2]

           date_interval = interval( input_date1 , input_date2  )
           
           select.date = data$date  %within% date_interval

           data =  data %>%  filter( select.date  ) 
           
           data$z = data[, input$var] 
           
           return( data )
           
           })
        
        dpoly <- reactive({
           
           data = data() %>% 
              group_by( country, adm1 ) %>%
              summarise( z = mean(z, na.rm = TRUE) )
           
           adm = adm1[ adm1@data$NAME_0 %in% country() , ]
           
           adm@data = inner_join( adm1@data, 
                                  data , 
                                  by = c("NAME_0" = "country", "NAME_1" = "adm1")
           )
           
           return( adm )
        })
       
        pal <- reactive({
           
           req( dpoly() )
           
           colorBin("YlGn", domain = dpoly()@data$z, bins = ncolors, na.color = "#bdbdbd")
      })
        
        # function to calculate number of rainy days (>0mm) in month

        popup <- reactive({

           rain_days = function(min = 10){

            rowSums(
               apply(
                  dpoly()@data ,
                  2, FUN = function(x) x >= min)
               )
           }

           paste0("<strong>", dpoly()@data$NAME_0, ", ", dpoly()@data$NAME_1, "</strong>",
                  "<br>Rainfall(mm): ",
                  round( dpoly()@data$z )
                  
           # paste0("<strong>", dpoly()@data$NAME_0, ", ", dpoly()@data$NAME_1, "</strong>",
           #                  "<br>Rainfall(mm): ",
           #                  dpoly()@data$z,
           #            "<br>Rainy days (>5mm): ",
           #            rain_days(5),
           #            "<br>Rainy days (>20mm): ",
           #            rain_days(20),
           #            "<br>Rainy days (>40mm): ",
           #            rain_days(40)
      )
        })
        
        labels <- reactive({
              sprintf(
                  "<strong>%s</strong><br/>%g rain / mm<sup>2</sup>",
                  dpoly()@data$NAME_1 ,
                  round( dpoly()@data$z )
              ) %>% 
              lapply(htmltools::HTML)
        })
        
        
        output$mymap <- renderLeaflet({

           leaflet() %>%
              addProviderTiles( leaflet_style ) %>%
              addPolygons( data = adm1[ adm1@data$NAME_0 %in% country() , ],
                           fillColor = NULL,
                           color = "#BDBDC3",
                           weight = 1
              ) 
           })

        observe({
           leafletProxy("mymap") %>%
           clearShapes() %>%
              addPolygons( data = dpoly(),
                           layerId = ~NAME_1 , 
                           fillColor = ~pal()(z),
                           fillOpacity = 0.5,
                           color = "#BDBDC3",
                           weight = 3,
                           popup = popup() ,
                           highlight = highlightOptions(
                              weight = 4,
                              color = "gray",
                              dashArray = "",
                              fillOpacity = 0.7,
                              bringToFront = TRUE),
                           label = labels(),
                           labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto")
                           )
        })

        observe({
           leafletProxy("mymap") %>%
              clearControls() %>%
              addLegend( pal = pal(),
                         values = dpoly()@data$z,
                         opacity = 0.7,
                         position = 'bottomright',
                         title = paste(quote(rain), 'mm/month') )
        })
        
# Dygraph ####
         library(xts)
        
        click_outside_map = reactive({
           FALSE
           # if ( !is.null(input$mymap_click) ){
           #    !identical(input$mymap_click[[1]], input$mymap_shape_click[[1]])
           # }
        })
        
        
        tsd = reactive({

           d.spread = data() %>% 
              select( country, date, adm1, z ) %>%
              spread( adm1 , z )
           
           if ( country() %in% 'Africa' ){
              d.spread = data() %>% 
                 select( country, date, adm1, z ) %>%
                 mutate( adm = paste(country, adm1, sep='-')) %>%
                 spread( adm , z )
           }
           
           # if ( !is.null( input$mymap_shape_click[[1]] ) & !click_outside_map() ){
           #    d.spread = data() %>% 
           #       select( country, date, adm1, z ) %>%
           #       filter( adm1 %in% input$mymap_shape_click[[1]] ) %>%
           #       spread( adm1 , z )
           # }

           tsd = xts( d.spread %>% select( -date, -country ) ,
                      order.by = d.spread$date
           )

           return(tsd)

        })

         color_palette  = RColorBrewer::brewer.pal( 12 , "Paired")
         # remove yellow
         color_palette = color_palette[ !color_palette %in% "#FFFF99"]

         dy = reactive({

            # return(NULL)

            dygraph( tsd() ,
                     # main = paste( "Reported", input$measure ) , ,
                     group = "a"
            ) %>%
               dyLegend(show = "always",
                        hideOnMouseOut = TRUE,
                        width = session$clientData[["output_dygraph_width"]]
                        )   %>%
               dyOptions(colors = color_palette,
                         strokeWidth = 3 ,
                         axisLabelFontSize = 16
                         # ,
                         # axisLabelFormatter = Dygraph.ANNUAL
               ) %>%
               dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
               dyRangeSelector(height = 50) %>%
               dyRoller(rollPeriod = 1, showRoller = TRUE) %>%
               dyAxis("x", axisLabelFormatter="function(d) { return d.getFullYear() }") %>%
               dyAxis("x", ticker="function(a, b, pixels, opts, dygraph, vals) {
                      return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
                      // or TWO_HOURLY or SIX_HOURLY...
         }")

   })
         
         observe({  output$dygraph <- renderDygraph({ dy() }) })

# Print ####        
         output$maplink <- downloadHandler(
            
            filename = function() { paste0( input$country, '_', input$date , '.png') } ,
            
            content = function(file) {
               
               p = choro()
               thumbName = "rain_map"
               ff <- paste0(thumbName, ".html")
               
               # padding
               p$sizingPolicy$padding <- 10
               
               suppressMessages( saveWidget( p, ff, selfcontained = FALSE) )
               
               webshot(ff, file = file , zoom = 3 )
            }
            
            
         )
         
         # output$dylink <- downloadHandler(
         #    
         #    filename = function() { paste0( input$country, '_', input$date , '.png') } ,
         #    
         #    content = function(file) {
         #       
         #       p = dy()
         #       thumbName = "rain_chart"
         #       ff <- paste0(thumbName, ".html")
         #       
         #       # padding
         #       p$sizingPolicy$padding <- 10
         #       
         #       suppressMessages( saveWidget( p, ff, selfcontained = FALSE) )
         #       
         #       webshot(ff, file = file , zoom = 3 )
         #    }
         #    
         #    
         # )
         


}
  



