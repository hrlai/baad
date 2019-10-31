library(baad.data)
library(ggplot2)
library(dplyr)
library(stringr)

# baad_data_version_current()

baad <- baad_data("1.0.1")

baad.map <- 
  baad$data %>% 
  group_by(studyName) %>% 
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            N = length(unique(speciesMatched)))

worldmap <- map_data("world")

ggplot(worldmap, aes(long, lat, group = group)) +
  geom_path() +
  geom_point(data = baad.map, 
             aes(longitude, latitude, size = N), 
             colour = "red",
             inherit.aes = FALSE) +
  coord_equal()





baad.nz <- 
  baad$data %>% 
  filter(str_detect(location, "Zealand"))




baad$dictionary %>% 
  select(variable, description)

