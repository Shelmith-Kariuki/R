x<-    highchart() %>% 
  hc_chart(type = "pie",options3d = list(enabled = TRUE, alpha = 70, beta = 0)) %>% 
  hc_plotOptions(pie = list(depth = 50)) %>% 
  hc_add_series_labels_values(dataLoc$Cluster, round(dataLoc$percent,2), innerSize = "60%", 
                              size='100%', dataLabels = list(distance = -40, format = '{point.y}%')) %>%
  hc_tooltip(valueDecimals = 2,
             pointFormat = "Percent: {point.y}%")
x



library(ggplot2)
g <- ggplot(mpg, aes( x = class, tooltip = class,
                      data_id = class ) ) +
  geom_bar_interactive()
ggiraph(code = print(g))

dat <- data.frame( name = c( "David", "Constance", "Leonie" ),
                   gender = c( "Male", "Female", "Female" ),
                   height = c(172, 159, 71 ) )
g <- ggplot(dat, aes( x = name, y = height, tooltip = gender,
                      data_id = name ) ) +
  geom_bar_interactive(stat = "identity")
ggiraph(code = print(g))