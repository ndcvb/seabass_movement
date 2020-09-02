#### Load packages ####
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)

#### Read shape files ####
bight <- readOGR("Data/Mapping/world_bay_gulf", layer = "world_bay_gulf")
eez <- readOGR("Data/Mapping/eez", layer = "eez")
netherlands_coast <- readOGR("Data/Mapping/netherlands_coast", layer = "world_countries_coasts")
belgium <- readOGR("Data/Mapping/belgium", layer = "worldcountries")

#### Fortify and organize ####
#coastline
bightfort <- fortify(bight)
netherlands_coastfort <- fortify(netherlands_coast)
netherlands_coastfort <- netherlands_coastfort[80:5110,]

#Belgium
belfort <- fortify(belgium)
belfort[37:43, ]$long <- NA
belfort[37:43, ]$lat <- NA
eezfort <- fortify(eez)
eezfort <- eezfort[c(10:30),]
belnew <- rbind(belfort[1:36,], eezfort, belfort[44:203,])

#remove shapefiles from environment
rm(eez, bight, netherlands_coast, belgium, belfort)

map_base <-   function() {
  ggplot() +
  coord_map(xlim = c(2.2,4.5), ylim = c(51,51.9)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "moccasin"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10)) +
  geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "lavender") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "moccasin") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.3) 
}
map_base()
