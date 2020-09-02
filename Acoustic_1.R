#### Load packages ####
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(grid)
library(ggforce)
library(raster)
library(ggsn)

setwd("~/Desktop/Seabass/Internship Noelia/Maps")

source("Scripts/0_Functions_map_base1.R")

#### Get data ####
df <- read.csv("Data/Acoustic/actel_det_zeeb.csv", stringsAsFactors = F)
#df <- df %>% dplyr::select(-X)
df$datetime <- parse_date_time(df$datetime, orders = "ymd HMS")

an <- read.csv("Data/Acoustic/actel_an_zeeb_1.csv", stringsAsFactors = F)
#an <- an %>% dplyr::select(-X)
an$utc_release_date_time <- parse_date_time(an$utc_release_date_time, orders = "ymd HMS")
an$recapture_date <- parse_date_time(an$recapture_date, orders = "ymd")

#### Data prep ####
# change factor levels
an$animal_id_pk <-factor(an$animal_id_pk, levels = unique(an$animal_id_pk[order(an$utc_release_date_time,decreasing = T)]))
df$animal_id_pk <- factor(df$animal_id_pk, levels = levels(an$animal_id_pk))
an$station_group <- as.factor(an$station_group)

#### Abacas plot ####
cols <- c("East coast" = "firebrick4",
          "Offshore windfarms" = "#00a3a6",
          "West coast" = "#00843d",
          "Midshore" = "#144e8b",
          "Scheldt estuary" = "#f3943e",
          "Offshore wrecks" = "darkmagenta",
          "Zeebrugge_port" = "#e84d0e",
          "Zeebrugge_out" = "#e84d0e",
          "Zeebrugge_inner" = "#e84d0e",
          "Oostende" = "green2")
x11()
ggplot(df, aes(x = datetime, y = animal_id_pk)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'gray98'),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        axis.text.x=element_text(hjust=0, size = 12)) +
  geom_point(data = an, 
             aes(x = utc_release_date_time, y = animal_id_pk), 
             colour = "black", shape = 23, size = 1.5) +
  geom_point(aes(colour = station_group), shape = 15, size = 2) +
  geom_point(data = an, 
             aes(x = recapture_date, y = animal_id_pk), 
             colour = "black", shape = 9, size = 2.5) +
  geom_point(data = an, 
             aes(x = utc_release_date_time, y = animal_id_pk, fill = station_group), 
             colour = "black", shape = 23, size = 2.5) +
  
  scale_colour_manual(values = cols, name = "Location") +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y", position = "top")

ggsave(filename = 'acoustic_1.png', plot = last_plot())

ggplot(df, aes(x = datetime, y = animal_id_pk)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'gray98'),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        axis.text.x=element_text(hjust=0, size = 12)) +
  geom_point(data = an, 
             aes(x = utc_release_date_time, y = animal_id_pk), 
             colour = "black", shape = 23, size = 1.5) +
  geom_point(aes(colour = station_group), shape = 73, size = 2) +
  geom_point(data = an, 
             aes(x = recapture_date, y = animal_id_pk), 
             colour = "black", shape = 9, size = 2.5) +
  geom_point(data = an, 
             aes(x = utc_release_date_time, y = animal_id_pk, fill = station_group), 
             colour = "black", shape = 23, size = 2.5) +
  scale_colour_manual(values = cols, name = "Location") +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y", position = "top")


#### Depth plot per station_group ####
# df %>% 
#   ggplot() + theme_bw() + 
#   geom_density(aes(x = -sensor_value_depth_meters, fill = station_group), alpha = 0.5) +
#   scale_fill_manual(values = cols)+ coord_flip() + labs(x= "", y = "") + 
#   ggtitle("Depth at different station groups")

df %>% 
  ggplot() + theme_bw() + 
  geom_density(aes(x = -sensor_value_depth_meters, fill = station_group)) +
  scale_fill_manual(values = cols)+ coord_flip() + 
  labs(x= "Depth (m)", y = "", fill = "") + 
  facet_wrap(~station_group, nrow = 1, scales = "free_x")

#### Map ####
# Manipulate spatial data
bight <- readOGR("Data/Mapping/world_bay_gulf", layer = "world_bay_gulf")
coords = matrix(c(2.2, 51,
                  2.2, 51.9,
                  4.2, 51.9,
                  4.2, 51,
                  2.2, 51), 
                ncol = 2, byrow = TRUE)
P1 = Polygon(coords)
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

bight_new <- intersect(bight, Ps1)
bight_new <- erase(Ps1, bight_new)
rm(P1, Ps1)

# Summarise df
df_sum <-  df %>% 
  group_by(station_group, station_name, deployment_lat, deployment_long) %>% 
  summarise()

df_sum2 <-  df_sum %>% 
  group_by(station_group) %>% 
  summarise(latitude = mean(deployment_lat),
            longitude = mean(deployment_long))

# Read receiver data
rec <- read.csv("Data/Acoustic/receivers.csv", stringsAsFactors = F)

# Map
 ggplot() +
  coord_map(xlim = c(2.2,4.1), ylim = c(51.05,51.9)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10, colour = "gray50"),
        legend.position="none") +
  
  geom_vline(xintercept = seq(2.2, 4, 0.2), size = 0.1, colour = "gray80") + 
  geom_hline(yintercept = seq(51, 51.9, 0.2), size = 0.1, colour = "gray80") +
  
  # geom_point(aes(3.20,51.36), size = 22, colour = "#e84d0e") +  # east coast
  # geom_point(aes(2.78,51.65), size = 22, colour = "#00a3a6") + # windfarms
  # geom_point(aes(2.78,51.22), size = 42, colour = "#00843d") +  # west coast
  # geom_point(aes(2.811205,51.42251), size = 22, colour = "#144e8b") + # offshore
  # geom_point(aes(3.5,51.42), size = 32, colour = "#f3943e") + #westerschelde
  # geom_point(aes(3.8,51.40), size = 42, colour = "#f3943e") + #westerschelde
  # geom_point(aes(4,51.4), size = 25, colour = "#f3943e") + #westerschelde
  # geom_point(aes(4.12,51.37), size = 23, colour = "#f3943e") + #westerschelde
  
  geom_point(data = rec, aes(deploy_lon, deploy_lat), shape = 20, size = 1.2) +
  
  geom_polygon(aes(x=long, y=lat, group=group), data = bight_new, fill = "paleturquoise4") +
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "paleturquoise4") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.3) +
  
  scale_x_continuous(breaks = seq(2.4, 4, 0.4)) +
  scale_y_continuous(breaks = seq(51, 51.9, 0.2)) +
  
  geom_point(data = df_sum, aes(deployment_long, deployment_lat, fill = station_group), shape = 21, size = 2) +
  scale_fill_manual(values = cols) +
  
  geom_label(aes(3.20,51.36), label = "Zeebrugge", hjust = -0.3, vjust = 1.7, fill = "#e84d0e", size = 4) +
  geom_label(aes(3.20,51.4), label = "East coast", vjust = -0.2, fill = "firebrick4", size = 4) +
  geom_label(aes(2.78,51.65), label = "Offshore windfarms", vjust = 1.7, fill = "#00a3a6", size = 4) +
  geom_label(aes(2.78,51.22), label = "West coast", vjust = 1.5, fill = "#00843d", size = 4) +
  geom_label(aes(2.811205,51.42251), label = "Midshore", hjust = -0.4, vjust = 0.9, fill = "#144e8b", size = 4) +
  geom_label(aes(3.9,51.4), label = "Scheldt estuary", vjust = 1.8, fill = "#f3943e", size = 4) +
  geom_label(aes(2.4,51.7), label = "Offshore wrecks", vjust = 5, fill = "darkmagenta", size = 4) +
  geom_label(aes(3.1,51.2), label = "Oostende", hjust = 1, vjust = 0, fill = "green2", size = 4) 

ggsave('map_acoustic2.png',plot = last_plot())
