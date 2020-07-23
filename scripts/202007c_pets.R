# Pets
library(tidyverse)
library(tidytuesdayR)
library(ggmap)
library(ggimage)
library(gganimate)
library(ggthemes)
library(extrafont)
library(ggtext)

## Get the data and tidy it for plotting ####
tt_data <- tt_load(2020, week = 30)
readme(tt_data)

# Geo data from here, 21/07/2020: https://www.corra.com.au/australian-postcode-location-data/
geo <- read.csv("../data/202007c_animals.csv") %>%
  filter(state == "QLD") %>%
  distinct(suburb, .keep_all = TRUE) # where there are several postcodes by suburb, keep the first one only

# Tidy, tidy, tidy!
ferals <- tt_data$brisbane_complaints %>%
  filter(grepl("[Ff]eral", category)) %>%
  mutate(suburb = toupper(suburb)) %>% 
  merge(geo, by = "suburb", all.x = T, all.y = F) %>%
  mutate(lat = ifelse(suburb == "BRISBANE CITY", -27.470125, lat), 
         lat = ifelse(suburb == "BRISBANE AIRPORT", -27.3942, lat),
         lat = ifelse(suburb == "KALINGA", -27.4104, lat),
         lat = ifelse(suburb == "CHANDLER", -27.5140, lat),
         lat = ifelse(suburb == "MACKENZIE", -27.54678, lat),
         lon = ifelse(suburb == "BRISBANE CITY", 153.021072, lon),
         lon = ifelse(suburb == "BRISBANE AIRPORT", 153.121, lon),
         lon = ifelse(suburb == "KALINGA", 153.0479, lon),
         lon = ifelse(suburb == "CHANDLER", 153.1560, lon),
         lon = ifelse(suburb == "MACKENZIE", 153.12460, lon)) %>%
  filter(!is.na(suburb)) %>%
  # create column of emoji for easy plotting
  mutate(em = ifelse(grepl("[Pp]ig", category), "1f437", NA),
         em = ifelse(grepl("[Gg]oat", category), "1f410", em),
         em = ifelse(grepl("[Cc]at", category), "1f431", em),
         em = ifelse(is.na(em), "1f47d", em),
         # in order to plot the UFOs first, so they don't hide the others
         em = relevel(as.factor(em), "1f47d")) %>%
  # tidy up dates
  mutate(year = str_extract(date_range, "20[0-9]{2}"),
         # seems to be a max length of string for str_extract?? this gets round it
         date_range = str_remove(date_range, "cars-srsa-open-data-animal-related-complaints-"),
         month = str_extract(tolower(date_range), 
                             paste0(tolower(month.abb[1:12]), 
                                    sep = "|", collapse = "")),
         month = ifelse(grepl("1st-quarter", date_range), "jan", month),
         quarter_text = recode(month, jan = "1st", apr = "2nd", jul = "3rd", oct = "4th"),
         quarter = recode(month, jan = "Q1", apr = "Q2", jul = "Q3", oct = "Q4"),
         tidy_date = lubridate::yq(paste0(year, "-", quarter)))

# to make text less blurry, have only one data point per date (transition frame)
subtitleText <- ferals %>%
  distinct(tidy_date, .keep_all = TRUE)
# don't want it to move with gganimate
minLat <- min(ferals$lat) 

## Plot it! ####
ggmap(get_stamenmap(bbox = c(min(ferals$lon) - .25,
                             min(ferals$lat) - .55,
                             max(ferals$lon) + .25, 
                             max(ferals$lat) + .05),
                    maptype ='toner-background',
                    zoom = 10))  +
  annotate("rect", xmin = min(ferals$lon) - .25, xmax = max(ferals$lon) + .25, 
           ymin = min(ferals$lat) - .55, ymax = max(ferals$lat) + .05, 
           fill = "#315705", alpha = 0.4) +
  geom_point(data = ferals, 
             aes(x = lon, y = lat),
             alpha = 0.3, colour = "#52db16", size = 15) +
  # creating shadow for the text
  geom_text(data = subtitleText, 
            aes(x = 152.9765, y = minLat - .4215,
                label = paste0("*Unidentified Feral Others\n\nReported in the ", 
                               quarter_text, " \nquarter of ", year)), lineheight = .8,
            family = "Arial Black", fontface = "bold",
            colour = "black",
            size = 7) +
  geom_text(data = subtitleText, 
            aes(x = 152.975, y = minLat - .42,
                label = paste0("*Unidentified Feral Others\n\nReported in the ", 
                               quarter_text, " \nquarter of ", year)), lineheight = .8,
            family = "Arial Black", fontface = "bold",
            colour = "white",
            size = 7) +
  geom_emoji(data = ferals, 
             aes(x = lon, y = lat, image = em)) +
  theme_map() +
  theme(plot.margin = margin(10, 10, 10, 10),
        plot.caption = element_text(size = 8, family = "Arial Black", hjust = .5)) +
  # adding text shadow
  annotate("text", x = 152.9765, y = minLat - .2015, lineheight = .8,
           label = "FERAL PIGS, CATS,\nGOATS AND UFOS*\nOF BRISBANE", 
           family = "Arial Black", fontface = "bold",
           colour = "black", 
           size = 11) +
  annotate("text", x = 152.975, y = minLat - .2, lineheight = .8,
           label = "FERAL PIGS, CATS,\nGOATS AND UFOS*\nOF BRISBANE", 
           family = "Arial Black", fontface = "bold",
           colour = "white", 
           size = 11) +
  labs(caption = "#TidyTuesday | Graphic: @crthompson | Source: Brisbane Open Data - Animal Complaints") +
  transition_time(tidy_date) 

# Export to create making-of gif, adapting the approach used by Georgios Karamanis (@geokaramanis)
ggsave(filename = file.path("../making-of/temp", paste0("202007c_ferals-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 6, height = 7)

## Export final gif ####
anim_save(filename = "../plots/202007c_pets.gif")
