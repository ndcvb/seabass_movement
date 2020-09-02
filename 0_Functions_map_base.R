#### Load packages ####
library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggformula)
library(ggpmisc)
library(FSA)

#### Read shape files ####
setwd("~/Documents/Seabass/Internship Noelia/Maps")
bight <- readOGR("Data/Mapping/world_bay_gulf", layer = "world_bay_gulf")
eez <- readOGR("Data/Mapping/eez", layer = "eez")
netherlands_coast <- readOGR("Data/Mapping/netherlands_coast", layer = "world_countries_coasts")
belgium <- readOGR("Data/Mapping/belgium", layer = "worldcountries")
france = readOGR('Data/Mapping/france_coast/', layer = 'coasts_per_ocean')

#### Fortify and organize ####
#coastline

bightfort   = fortify(bight)
#bightfort   = bightfort %>%
 # filter(long >= 2)

francefort  = fortify(france)
netherlands_coastfort <- fortify(netherlands_coast)
#netherlands_coastfort <- netherlands_coastfort[80:5110,]

#Belgium
belfort <- fortify(belgium)
#belfort[37:43, ]$long <- NA
#belfort[37:43, ]$lat <- NA
eezfort <- fortify(eez)
eezfort <- eezfort[c(10:30),]
belnew <- rbind(belfort[1:36,], eezfort, belfort[44:203,])


#remove shapefiles from environment
#rm(eez, bight, netherlands_coast, belgium, belfort,france)

data = read.csv(file ="Data/Mark-recapture/zeebaars_updateN3.csv", header = T)
data = na.omit(data[,9:12])

ggplot() +
  coord_map(xlim = c(-0.5,4.5), ylim = c(49.5,52)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size=10))+
  geom_path(data = francefort, aes(x = long, y = lat, group = group), size = 0.1)  +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.2)+ 
  geom_path(data = netherlands_coastfort, aes(x = long, y = lat, group = group), size = 0.15) + 
  geom_point(data=data, aes(x=Recapture_lon, y=Recapture_lat, color=factor(specificity.grade))) + 
  geom_path(data = belfort, aes(x = long, y = lat, group = group), size = 0.2) +
  guides(color=guide_legend(title = 'Degree of specificity')) + labs(x='Longitude', y='Latitude')
  
ggsave('mark_recapture4.png', plot=last_plot())

data = read.csv(file ="Data/Mark-recapture/zeebaars_updateN3.csv", header = T)

data %>% filter(Catch_location!='') %>%
  group_by(Catch_location, Catch_lon,Catch_lat) %>% summarise(average_length = mean(Catch_length,na.rm = T)) %>%
  
ggplot() +
  coord_map(xlim = c(2,4.5), ylim = c(50.5,52)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size=10))+
  geom_path(data = francefort, aes(x = long, y = lat, group = group), size = 0.2)  +
  geom_path(data = belfort, aes(x = long, y = lat, group = group), size = 0.3)+ 
  geom_path(data = netherlands_coastfort, aes(x = long, y = lat, group = group), size = 0.2) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.2) +
  geom_point(aes(x=Catch_lon, y=Catch_lat, color=factor(Catch_location))) + 
  labs(x='Longitude', y='Latitude', color= 'Location')

ggsave('length_distribution4.png', plot=last_plot())

data_text = data %>% 
  group_by(Catch_location) %>%
  filter(n() > 15) %>%summarise(label=n())

pos = data.frame(x=c(70, 70, 70,70, 70, 70, 70),y=c(75, 100, 7.5,40, 200, 180, 40))

x11()
data %>% 
  group_by(Catch_location) %>%
  filter(n() > 15) %>%
  ggplot()+ geom_histogram(aes(Catch_length)) + facet_wrap(~Catch_location, ncol=2, scales = 'free_y') + theme_bw() + labs(x= 'Length (cm)', y= 'Frequency') +  geom_text(
    data    = data_text,
    mapping = aes(pos$x, pos$y, label = paste0('n = ',data_text$label)),
    hjust   = 0.1,
    vjust   = 0.5
  )

data %>% 
  group_by(Catch_location) %>%
  filter(n() > 15) %>%
  ggplot()+ geom_histogram(aes(Catch_length, color = Catch_method)) + facet_wrap(~Catch_location, ncol=2, scales = 'free_y') + theme_bw() + labs(x= 'Length (cm)', y= 'Frequency') +  geom_text(
    data    = data_text,
    mapping = aes(pos$x, pos$y, label = paste0('n = ',data_text$label)),
    hjust   = 0.1,
    vjust   = 0.5
  )

data %>% 
  filter(Catch_method!='') %>%  
  group_by(Catch_method) %>%
  filter(n() > 15) %>%
  ggplot()+ geom_histogram(aes(Catch_length)) + facet_wrap(~Catch_method, ncol=1, scales = 'free_y') + theme_bw() + labs(x= 'Length (cm)', y= 'Frequency') 

ggsave('length_catchmeth_hist5.png', plot=last_plot())
  
ggsave('length_distribution_hist6.png', plot=last_plot())

model    = nls(Catch_weight~a*Catch_length^b,data=data,start=list(a=0.01,b=3))

length_t =  Catch_length = sort(unique(data$Catch_length))
weight_t =  coef(model)[1]*(length_t)^coef(model)[2]

plot(length_t,weight_t)

data_t   = data.frame(cbind(length_t,weight_t))

data %>%
ggplot() + geom_point(aes(x=Catch_length, y=Catch_weight))  + theme_bw() + labs(x= 'Length (cm)', y= 'Weight (g)') + geom_line(data=data_t, aes(x=length_t, y=weight_t), color='gray43', size=1.5) + annotate('text', x = 40, y = 4000, label = paste0('weight = ',{round(coef(model)[1],2)},'* length^',{round(coef(model)[2],2)}))

ggsave('length_weight2.png', plot=last_plot())

