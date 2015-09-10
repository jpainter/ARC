# ARC CHART
library(ggplot2)
library(scales)
library(grid)
library(dplyr)

load('total_rainfall.rda') # saved from ARC_total_rainfall.R

#    ggplot( data = total_rainfall, aes(color = year, y = rain, x = month)) +
#       geom_line()


# PMI countries

pmi = c( "Angola", "Benin", "Democratic Republic of the Congo", 
         "Ethiopia", "Ghana", "Guinea", "Kenya", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", 
         "Nigeria", "Rwanda", "Senegal", "United Republic of Tanzania", "Uganda", "Zambia", "Zimbabwe" )

pmi_abrev = c( "Ang", "Ben", "DRC", 
               "Eth", "Gha", "Gui", "Ken", "Lib", "Mad", "Mal", "Mali", "Moz", 
               "Nig", "Rwa", "Sen", "Tanz", "Uga", "Zam", "Zim" )

pmi_world =  world[world$ADMIN.1 %in% pmi,] 


# use country abbreviations
total_rainfall$abrev =  pmi_abrev[match(total_rainfall$country, pmi)]


# monthly rainfall
monthly = 
   ggplot( data = total_rainfall, aes(color = year, 
                                      fill = year, 
                                      y = rain_pmi_mean, 
                                      x = factor(month)) ) +
   geom_bar(stat = 'identity') + labs( x = 'Month') +
   ylab("Mean Monthly Rainfall (mm) / 11 sq km") +
   ggtitle("Mean Rainfall in PMI Countries") +
   facet_grid(abrev ~ ., scales = "free")

ggsave("monthly_rainfall.pdf", dpi=300, height=9, width = 6)

# yearly rainfall
ggplot( data = total_rainfall %>% group_by(country, abrev, year) %>% 
           summarize( yearly = sum(rain_pmi_mean)), 
        aes(y = yearly, x = year)) +
   geom_line( aes(group = country)) +
   ylab("Mean Yearly Rainfall (mm) / 11 sq km") +
   ggtitle("Mean Rainfall in PMI Countries") +
   # geom_bar(stat = 'identity') +
   facet_grid(abrev ~ .)

ggsave("yearly_rainfall.pdf", dpi=300, height=11, width = 6)
