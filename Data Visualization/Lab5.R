library(tmap)
nc <- st_read(system.file("shape/nc.shp", package = "sf"),quiet =TRUE)
tmap_mode("view")
choropleth_map <- tm_shape(nc) +
  tm_polygons("BIR74",
              style="pretty", 
              palette="viridis", 
              title="Total Births", 
              id="NAME", 
              popup.vars=c("Births(1974))"="BIR74", "SIDS Cases" = "SID74"), 
              aplha=0.8) +
  tm_layout(title = "North Carolina Birth Data")
choropleth_map


data_india <- statepop %>%
  mutate(Density = pop_2011 / area)
india_map <- plot_india(regions = "states", 
                        data = data_india, 
                        values = "pop_2011") +
  scale_fill_distiller(palette = "YlOrRd", 
                       direction = 1, 
                       name = "Population (2011)") +
  labs(title = "Population Distribution across Indian States",
       subtitle = "Based on 2011 Census Data",
       caption = "Source: mapindia package") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

print(india_map)

world<-map_data("world")
eq<- read_csv("D:/Data/Github/SheldonC2005/Academics/Data Visualization/all_day.csv")
ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray60"
  ) +
  geom_point(
    data = eq,
    aes(x = longitude, y = latitude, size = mag),
    alpha = 0.6,
    color = "red"
  ) +
  scale_size(range = c(1, 6)) +
  labs(
    title = "Real-Time Earthquake Dot Map (Last 24 Hours)",
    subtitle = "Each dot represents one earthquake event",
    size = "Magnitude"
  ) +
  theme_minimal()

map("world", col="lightblue", fill=TRUE, bg="white", lwd=0.5)
map("world", "India", col="red", fill=TRUE, lwd=0.5)
map("world", "Australia", col="blue", fill=TRUE, lwd=0.5)

cities<- data.frame(
  name=c("Delhi","Mumbai","Chennai","Kolkota"),
  lon=c(77.1025, 72.8777, 80.2707, 88.3639),
  lat=c(28.7041, 19.0760, 13.0827, 22.5726)
)
map("world", "India", col="lightyellow", fill=TRUE)
points(cities$lon, cities$lat, col="red", pch=16, cex=1.5)
