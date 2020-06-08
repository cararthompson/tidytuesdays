library(rnaturalearth)
library(sf)
library(gganimate)
library(gifski)
library(tidyverse)

# Data ####
volcanoes <- read.csv("../data/202005_volcanoes.csv", 
                      header = T, stringsAsFactors = F) %>%
  select(Volcano.Name, Volcano.Number, Last.Known.Eruption, Longitude, Latitude) %>%
  mutate(Last.Year = ifelse(grepl("BCE", Last.Known.Eruption),
                            - as.numeric(str_extract(Last.Known.Eruption, "[0-9]+")),
                            as.numeric(str_extract(Last.Known.Eruption, "[0-9]+"))),
         Last.Eruption.Century = as.integer(floor(Last.Year/100))) %>%
  arrange(Last.Eruption.Century) %>%               
  mutate(Last.Eruption.Century.Text = ifelse(Last.Eruption.Century < 0, 
                                             paste0(abs(Last.Eruption.Century), "00s BCE"),
                                             paste0(Last.Eruption.Century, "00s CE")),
         Last.Eruption.Century.Text = factor(Last.Eruption.Century.Text, 
                                             levels = unique(Last.Eruption.Century.Text)),
         Extinct = factor(ifelse(is.na(Last.Eruption.Century), "Yes", "No"), 
                          levels = c("No", "Yes")))

# need two dataframes to keep the original points on the map in the animation
volcanoes_static <- volcanoes %>%
  select(Volcano.Name, Volcano.Number, Longitude, Latitude, Extinct)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot ####
background_map <- ggplot(data = world) +
  geom_sf(fill = "grey20", colour = "grey20") +
  theme(plot.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color ="black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.title = element_text(color = "white", size = 14),
        legend.text = element_text(color = "white", size = 14),
        legend.position = "top", 
        legend.justification= "left",
        legend.key = element_rect(fill = "black"),
        plot.caption = element_text(color = "white", size = 10)) +
  geom_point(data = volcanoes_static,
             aes(x = Longitude, y = Latitude, colour = Extinct), 
             size = 1.5, alpha = 0.8) +
  scale_colour_manual(
    guide = guide_legend(title = " and volcanoes that ", 
                         title.position = "left", title.hjust = 1, 
                         direction = "horizontal",
                         label.hjust = 0),
    values = c(No = "orange", Yes = "darkgreen"),
    labels = c("have and   ", "haven't erupted since then")) +
  labs(caption = "#TidyTuesdays | @crthompson | Data: The Smithsonian Institution") +
  xlab("") +
  ylab("") 

eruptions <- background_map + 
  geom_point(data = volcanoes, 
             aes(x = Longitude, y = Latitude), 
             size = 4, alpha = 0.9,
             colour = "orangered") +
  transition_states(Last.Eruption.Century.Text, transition_length = 1, 
                    state_length = 2, wrap = F) +
  shadow_mark(colour = "darkgreen", fill = "darkgreen", size = 1.5, alpha = 1) +
  labs(title = "Volcanoes that last erupted in the {closest_state}") +
  theme(plot.title = element_text(colour = "orangered", face = "bold", size = 20),
        legend.position = "top")

animate(eruptions, renderer = gifski_renderer(), start_pause = 5, end_pause = 15, 
        nframes = length(unique(volcanoes$Last.Eruption.Century.Text))*2 + 20, # for pauses
        height = 500, width = 700)

anim_save(filename = "../plots/202005_volcanoes.gif")
