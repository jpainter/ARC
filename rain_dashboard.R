

# Packages ####
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet) # library(devtools); devtools::install_github("rstudio/leaflet")
library(sp)
library(countrycode)
library(crosstalk)

# Datasets ####

load( 'adm0.africa')

adm_data = adm0.africa@data

country.list = dplyr::count(adm_data, ISO) %>% 
   mutate(
     country = countrycode(ISO , "iso3c", "country.name" )
     ) %>% .$country

country.list = c("Africa" ,  country.list )

source("rain_module.R")

source("test_module.R")

adm1_rain = readRDS("arc_adm1_rainfall.rds") %>%
   mutate( date = ymd( paste( year, month , 1, sep = "-") ) )

# crosstalk dataset
shared_adm1_rain = SharedData$new(adm1_rain, function(data){ 
   paste0(data$country, data$adm1, as.character(data$date))
}
   )  


# Header ####
header = dashboardHeader(title = "Rain Dashboard")

# Sidebar ####
sidebar = dashboardSidebar(

   sidebarMenu(id="tabs",
               
               menuItem("Rain Map", tabName = "map", icon = icon("dashboard"),
                        selected = TRUE)
   ),
   
   selectInput("country", "Country",
               choices = country.list ,
               selected = "Angola")
   ,
   
   h5( " Rainfall estimated from the NOAA 
       Climate Prediction Center (CPC) Africa Rainfall Climatology Version 2.0 (ARC2)" ),
   
   br(), br(),
   
   h4( " Clicking on an area in map gives detailed estimated for that region" )
   )


# Body  ####

body = dashboardBody(

   # First tab content
   tabItem(tabName = "map", background = NULL ,
           fluidRow(
              column( 12,
                      
                      box(
                         
                         tags$style(type = "text/css", ".box-body {height: calc(100vh - 80px) }"),
                         
                         title = NULL ,
                         # solidHeader = TRUE,
                         width = 12 ,
                         background = NULL ,
      
                         # rain_map_UI( "map" , country = "Angola", dataset = adm1_rain)
                         rain_map_UI( "map" , country = "Angola", dataset = shared_adm1_rain)
                         
                      )
              )
           )
   ) 

) #dashboardbody

# UI  ####
ui = dashboardPage( header, sidebar, body )


# Server ####

server <- function(input, output, session) {
   
 #  rain module ####
   country = reactive({ input$country })
   # 
   
   callModule( rain_map_Output, "map",
              dataset = shared_adm1_rain,
              country = country 
   )
   
   # triggers display of tab at startup
   # isolate({updateTabItems(session, "tabs", "map")})
   
}

# compile

shinyApp(ui, server)


