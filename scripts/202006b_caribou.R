library(tidyverse)
library(gganimate)
library(lubridate)
library(ggpomological)
library(ggmap)
library(ggtext)

# Load the data
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

youngAndOld <- locations %>%
  filter(timestamp > ymd(19991231), # avoinding the gap in the data
         animal_id %in% 
           subset(individuals, individuals$life_stage %in% 
                    c("2-3", "8-10", "10-12", "12-15"))$animal_id) %>%
  mutate(ageGroup = ifelse(animal_id %in% 
                             filter(individuals, life_stage == "2-3")$animal_id, 
                           "Youngest",
                           "Oldest"))

# For ease in plot
pomo_pal <- c(ggpomological:::pomological_palette,
              unname(unlist(ggpomological:::pomological_base)))

# To use timestamp summary in title
getSeasonYear <- function(input.date){
  numeric.date <- 100 * month(input.date) + day(input.date)
  cuts <- base::cut(numeric.date, breaks = c(0,415,1015,1231)) 
  levels(cuts) <- c("Winter", "Summer","Winter")
  return(paste0(" the  ", cuts, " of ", year(input.date)))
}

ggmap(get_stamenmap(bbox = c(min(youngAndOld$longitude) - .5,
                             min(youngAndOld$latitude) - .5,
                             max(youngAndOld$longitude) + .5, 
                             max(youngAndOld$latitude) + .5),
                    zoom = 8, # scale = 2,
                    maptype ='watercolor',
                    color = 'color')) +
  geom_point(alpha = 0.2, data = youngAndOld, 
             shape = 21, size = 3, stroke = 0.3, 
             aes(x = longitude, y = latitude,
                 fill = ageGroup, colour = season),
             show.legend = F) +
  scale_fill_manual(values = c("Youngest" = pomo_pal[5],
                               "Oldest" = pomo_pal[9])) +
  scale_colour_manual(values = c("Summer" = pomo_pal[4], 
                                 "Winter" = pomo_pal[10])) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) + 
  labs(title = "Caribou travel through the seasons of life \n ", 
       subtitle = "  
Tracing the steps of the <span style='color:#e68c7c;'>nine youngest</span>  
and <span style='color:#6f5438;'>nine oldest</span> caribou through  
{getSeasonYear(frame_time)}.  

",
       x = "Longitude", 
       y = "Latitude", 
       colour = "",
       caption =  "\n\n#TidyTuesdays | Graphic: @cararthompson | Data: Movebank") +
  transition_time(timestamp) + 
  theme_pomological_fancy() %+replace%
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_markdown(hjust = 0.5, size = 11, lineheight = 1),
        plot.caption = element_text(size = 8, family = "", hjust = .5),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) 

anim_save(filename = "../plots/202006b_caribou.gif")

