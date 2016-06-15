
# Country Rainfall

total_rainfall %>%
   filter( country %in% "Angola", year %in% 2011) %>%
   mutate( month = factor(month, ordered = TRUE, levels = 1:12, labels = month.name)) %>%

   ggplot( aes(y = rain_pmi_mean, x = month )) +
   xlab("Month") +
   ylab("Rain") + 
   geom_bar( stat = 'identity') +
   coord_flip() +
   ggtitle("Angola Rainfall, 2011")