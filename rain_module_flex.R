# rainfall widget


library(tidyverse)
library(lubridate)
library(countrycode)
# library(plotly)

library(crosstalk)
library(sp)

library(shiny)
library(flexdashboard)

library(leaflet)
library(DT)
library(dygraphs)
library(htmlwidgets)
library(webshot)
library(ggalt)


# dataset ####


   # load( 'adm1.africa')
   # load( 'adm1.africa.s')

   # directory for gadm files
   # base.fileneame = "../malaria_atlas/gadm/" 
   
   # adm1.africa = readRDS( paste0( base.fileneame, "adm1.africa.RDS" ) )
   
   # Consider simplifying maps: 
   # https://gis.stackexchange.com/questions/236340/simplifying-and-plotting-polygons-in-leaflet-package-in-r/236465#236465
   # the ones below will not display kenya or tz in leaflet 
   
# set data directory
   dataset_folder =  "Malaria_Dashboard_Data_Files/"  # as part of dashboard
   dataset_folder = "" # standalone


   # load( "adm1.africa.s"  )
   load( paste0( dataset_folder, "adm1.africa.s") )
   
   adm.map = adm1.africa.s


   # adm0_rain = readRDS("arc_adm0_rainfall.rds")
   
   # adm1_rain = readRDS("arc_adm1_rainfall.rds")
   
   adm1_rain = readRDS( paste0( dataset_folder, "arc_adm1_rainfall.rds") )
   
   # adm2_rain = readRDS("arc_adm2_rainfall.rds")


   adm1_rain$date = ymd( paste( adm1_rain$year, adm1_rain$month, 1, sep = "-"))

   vars = colnames(  adm1_rain )



# UI functions ####

   rain_map_UI = function(id) {

      # Create a namespace function using the provided id
      ns <- NS(id)

      # country.iso3  = countrycode( country, "country.name", "iso3c")

      fillCol(
         height = "100%" ,
                flex = c(NA, 2, 1, 1),
      fluidPage(
                fluidRow(
                   column(4 ,

                             selectInput( ns("var"), "Variable",
                                          choices = vars,
                                          selected = "rain_mean"
                             ) 
                          # ,
                          #     
                          #     checkboxInput( ns("lines"), "Monthly lines")
                          )
                             ,
                   column( 8 ,
                              sliderInput(
                                 inputId = ns("date_range") ,
                                 label = "Choose Month:",
                                 width = "100%",
                                 value = ymd("2017-01-01") ,
                                 min = min(adm1_rain$date),
                                 max = max(adm1_rain$date) ,
                                 step = 30 ,
                                 timeFormat = "%Y-%m",
                                 animate = animationOptions(interval = 1000)
                              )

                      )
                   )
                ) ,
                
            leafletOutput( ns('mymap'), height = "100%", width = "100%") 
            ,

            plotOutput( ns("year_histo"), height = "100%", width = "100%" )
            ,

            plotOutput( ns("month_histo"), height = "100%", width = "100%"
                        # , hover = hoverOpts(id =ns("plot_hover")) 
            # plotly::plotlyOutput( ns("month_histo"), height = "100%", width = "100%"
                        )
      )
}


# Server functions ####

   rain_map_Output = function(  input, output, session ,

                               dataset = adm1_rain ,
                               country = "Burundi" ,
                               ncolors = 5 ,
                               title = "Rainfall" ,
                               leaflet_style = "Stamen.TonerLite"
   ) {

      session$onSessionEnded(stopApp) # kills app when browser closes

      output$title = renderText(title)

      updateSliderInput(session, "date_range", value = min(adm1_rain$date) )

      country_iso3 = reactive({
         iso3 = countrycode( country() , "country.name", "iso3c" )
         # iso3 = countrycode( country , "country.name", "iso3c" )
      })
         
      selected_country_iso3 = reactive({

         # country
         if ( country() %in% 'Africa'){

            TRUE

         } else {

            selected_country_iso3 = dataset$iso3 %in% country_iso3()
            # selected_country_iso3 = dataset$iso3 %in% iso3
            
            return( selected_country_iso3 )

         }

         })
      

   data <- reactive({

           req( input$date_range, selected_country_iso3() )

          # country
           if ( country() %in% 'Africa'){

              data =  dataset

           } else {

              # data =  dataset[ selected_country_iso3, ]
              data =  dataset[ selected_country_iso3(), ]
           }

           # date range
           input_date1 = input$date_range # input$date_range[1]

           input_date2 = input_date1 + days_in_month( input_date1 ) - 1

           date_interval = interval( input_date1 , input_date2  )

           select.date = data$date  %within% date_interval

           data =  data %>%  filter( select.date  )
           
           data$z = data[, input$var]
           # data$z = data[, "mean_rain"]
           
           # convert NaN to NA
           data$z[ is.nan(data$z) ] = NA

           return( data )

           })

   dpoly <- reactive({

           req( data() )

          # mean over time-period selected
           data = data() %>%
              group_by( NAME_0, NAME_1 ) %>%
              summarise( z = mean(z, na.rm = TRUE) )
           
           # data = data %>%
           #    group_by( NAME_0, NAME_1  ) %>%
           #    summarise( z = mean(z, na.rm = TRUE) )

           country.map = adm.map[ adm.map@data$iso3 %in% country_iso3() , ]
           # country.map = adm.map[ adm.map@data$iso3 %in% iso3 , ]

           country.map@data = inner_join( country.map@data,
                                  data ,
                                  by = c("NAME_0" , "NAME_1")
           )

           # convert NaN to NA
           country.map@data$z[ is.nan(country.map@data$z) ] = NA

           return( country.map )
        })

   pal <- reactive({
           values = dataset[ , input$var ] %>% unlist %>% unname
           
           # colorNumeric("YlGn",
           #              # dataset[ dataset$iso3 %in% selected_country_iso3() , "rain" ],
           #              domain = values  ,
           #              alpha = TRUE, na.color = "#bdbdbd")

           # colorBin("YlGn", domain = values , bins = ncolors, na.color = "#bdbdbd")
            
           # values = dataset[ , input$var ] %>% unlist %>% unname
           # # values = dataset[ , "mean_rain" ] %>% unlist 
           # 
           colorBin( "YlGn" ,
                     domain =  values ,
                     bins = quantile( values ,
                                      probs = seq(0, 1, 0.2) ,
                                      na.rm = TRUE
                                      )  %>% round(0) %>% unique ,
                     pretty = TRUE, alpha = TRUE, na.color = "#bdbdbd")

           # colorQuantile( "YlGn" , domain =  values ,
           #           n = ncolors,
           #           alpha = TRUE, na.color = "#bdbdbd")

      })


   popup <- reactive({

           rain_days = function( min = 10  ){

            # rowSums(
            #    apply(
            #       dpoly()@data ,
            #       2, FUN = function(x) x >= min)
            #    )
              
               x = dpoly()@data$z
               y = sum(  x >= min , na.rm = TRUE )
               return(y)
              
           }

           paste0("<strong>", dpoly()@data$NAME_0, ", ", dpoly()@data$NAME_1, "</strong>",
                  "<br>Rainfall(mm): ",
                  round( dpoly()@data$z ) ,
                  "<br>Days with >10mm:" , rain_days()

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

           req( country() )

           leaflet( data = dpoly() ) %>%
              addProviderTiles( leaflet_style , group = 'background map'
                                ) %>%
              # addPolygons(
              # 
              #              data = adm ,
              #              fillColor = ~pal()(z) ,
              #              color = "#BDBDC3",
              #              weight = 1
              # )

              addLayersControl(
                 baseGroups = c("background map" ),
                 overlayGroups = c("monthly rainfall (mm)"),
                 options = layersControlOptions(collapsed = TRUE)
              )  %>%
              
              addPolygons( 
                           # data = dpoly() ,
                           group = "monthly rainfall (mm)" ,
                           label = labels(),
                           popup = popup() ,
                           fillColor = ~pal()(z) ,
                           fillOpacity = .5,
                           color = "black",
                           weight = 1 ,
                           smoothFactor = 0.2
                           # highlight = highlightOptions(
                           #    weight = 4,
                           #    color = "gray",
                           #    dashArray = "",
                           #    fillOpacity = 0.7,
                           #    bringToFront = TRUE),
                           # labelOptions = labelOptions(
                           #    style = list("font-weight" = "normal",
                           #                 padding = "3px 8px"),
                           #    textsize = "15px",
                           #    direction = "auto")
              ) %>%
              
              addLegend( pal = pal(),
                         layerId = "legend" ,
                         values = dataset[ , input$var ] ,
                         # values = dataset[ dataset$iso3 %in% selected_country_iso3() , "rain" ] ,
                         opacity = 0.7,
                         position = 'bottomright',
                         title = paste(quote(rain), 'mm/month') )


           })
        
        # HISTO VIS ####
        
        # histo_detail <- function(x){
        #    
        #    if(is.null(x)) return(NULL)
        #    paste( x[1,1], " - ", x[1,2])
        #    # row_id = row_idx <- which(
        #    #                          tr2r()$year == x[1,1] &
        #    #                          tr2r()$month == round(x[1,2]) + 1
        #    #                          )
        #    # 
        #    # data <- tr2r()[ row_id, ] %>% filter( year %in% input$year) 
        #    # 
        #    # paste0("<b>", data[,"year"], "</b><br>",
        #    #           format(data[,"rain"], digits = 2), " mm<br>",
        #    #           "Previous year: ", format(data[,"prev.year"], digits = 2), " mm<br>",
        #    #           "<b>% of previous year: ", percent(data[,"year.change"]), "</b><br>",
        #    #           "Baseline, 2000-2006: ", format(data[,"base.rain"], digits = 2), " mm<br>",
        #    #           "<b>% of baseline: ", percent(data[,"base.change"]), "</b>"
        #    # )
        # }  
        
        output$year_histo = renderPlot({
           
           # country
           if ( country() %in% 'Africa'){
              
              data =  dataset
              
           } else {
              
              # data =  dataset[ selected_country_iso3, ]
              data =  dataset[ selected_country_iso3(), ] 
           }

           data  %>%
              
              group_by( year ) %>%
              summarise(
                 rain_total = sum( rain_total, na.rm = TRUE),
                 rain_observations = sum( rain_observations, na.rm = TRUE)
              ) %>%
              mutate(
                 rain = ifelse( rain_observations > 0 , rain_total / rain_observations, 0),
                 selected = ifelse( year %in% year( input$date_range ) , 1, 0)
                 # selected = ifelse( year %in% year( "2017-1-1") , 1, 0)
              ) %>%
              group_by( selected ) %>%
              
              ggplot( aes( year, rain ) ) +
              geom_col( aes( fill = selected ) ) +
              # geom_line( color = "black" , alpha = .35 ) +
              # geom_lollipop( aes( color = selected ) , size = 1 ) +
              guides(fill = FALSE, color = FALSE, size = FALSE) +
              labs( x = "Year", title = "Yearly" )
           
        })

  
        # output$month_histo = plotly::renderPlotly({
        output$month_histo = renderPlot({

           # country
           if ( country() %in% 'Africa'){

              data =  dataset

           } else {

              # data =  dataset[ selected_country_iso3, ]
              data =  dataset[ selected_country_iso3(), ] 
           }

           d = data %>%
              group_by( year, month ) %>%
              summarise(
                 rain_total = sum( rain_total, na.rm = TRUE),
                 rain_observations = sum( rain_observations, na.rm = TRUE)
              ) %>%
              mutate(
                 rain = ifelse( rain_observations >0, rain_total / rain_observations , 0),
                 month_selected = ifelse( month %in% month( input$date_range ) , 1, 0) ,
                 year_selected = ifelse( year %in% year( input$date_range ) , 1, 0)
              ) %>% ungroup()
           
           plot = 

              ggplot( d, aes( factor(month), rain ) ) +
              guides(fill = FALSE, color = FALSE, alpha = FALSE ) +
              # scale_x_discrete( labels = format("%m") ) +
              labs( x = "Month", title = "Monthly" )
           
           # if ( input$lines ){
              plot  = plot + 
                  geom_line( aes( color = year_selected , 
                                 alpha = year_selected , 
                                 group = year )
                            # , alpha = .35 
                            ) +
                 geom_point( aes( color = month_selected  
                                  , alpha = year_selected ) )
           # } else {
           #    plot  = plot + geom_col( aes( fill = month_selected ) ) 
           # }

         plot
         # plotly::ggplotly( plot ) 
        })
 
}




