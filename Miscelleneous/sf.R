nc = st_read(dsn = "Desktop/KenyanCensus2019/Kenya_Counties (2)/Kenya_Counties/KenyaCounties.shp")

setwd("Desktop/KenyanCensus2019/Kenya_Counties (2)/Kenya_Counties/")
require(rgdal)
shape <- readOGR(dsn = "KenyaCounties.shp", layer = "KenyaCounties")

# require(sf)
# shape <- read_sf(dsn = ".", layer = "KenyaCounties")

plot(shape,main = "Kenya")

shapefile_df <- fortify(shape)

ggplot() +
  geom_path(data = shapefile_df, aes(x = long, y = lat, group = group)) +
  labs(title = "Kenya")+
  theme_bw()

ggplot(shapefile_df)+
  geom_sf(aes(fill = Population))+
  scale_fill_viridis_c(option = "plasma")

bg = "grey92"
ggplot() + 
  geom_polygon(data = shapefile_df, aes(long, lat, group = group, fill = hole), 
               colour = alpha("darkred", 1/2), size = 0.7) + 
  scale_fill_manual(values = c("skyblue", bg)) + 
  theme(panel.background = element_rect(fill = bg),
        legend.position = "none")

x <- shape@data
x$id <- row.names(x)
shapefile_df <- left_join(shapefile_df, x)

bg = "grey92"
ggplot() + 
  geom_polygon(data = shapefile_df, aes(long, lat, group = group, fill = population)) + 
  scale_fill_manual(values = c("skyblue", bg)) + 
  theme(panel.background = element_rect(fill = bg),
        legend.position = "none")


shapefile_df$Population <- as.numeric(as.character(shapefile_df$Population))
ggplot(shapefile_df, aes(x = long, y = lat, group = group, fill = population)) +
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  labs(title = "Hospital Density in Scotland (2018)") +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))

