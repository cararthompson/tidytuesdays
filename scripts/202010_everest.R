# Everest

library(tidyverse)
library(tidytuesdayR)
library(extrafont)

# Get data ----
# tt_data <- tidytuesdayR::tt_load(2020, 39)
# readme(tt_data)

# Reshape data to look at ascents and deaths of Everest  ----
everest <- tt_data$members %>%
  filter(peak_name == "Everest",
         success == "TRUE" | died == "TRUE")  %>%
  mutate(plotage = age * 100)

everestDecades <- everest %>%
  mutate(year = floor(year/10)*10 + 5) %>% # centre of decade
  group_by(year, hired) %>%
  summarise(successes = sum(success == "TRUE" & died == "FALSE"),
            deaths = sum(died == "TRUE")) %>%
  ungroup() %>%
  mutate(deaths = -deaths,
         cummean_successes = cummean(successes),
         cummean_deaths = cummean(deaths)) %>%
  # interleave means with sums to get peaks + troughs
  gather(key = "group", value = "value", -c(year, hired)) %>%
  mutate(plotyear = ifelse(grepl("cummean", group),
                           year + 5,
                           year),
         plotgroup = ifelse(grepl("deaths", group),
                            "deaths",
                            "successes"),
         plotyear = ifelse(hired == "TRUE",
                           plotyear - 1,
                           plotyear + 1))

everestDeaths <- everest %>%
  filter(died == "TRUE") %>%
  mutate(value = age * 100 + 1200) # age for plotting


# Create Everest theme
hired <- "#E87EA1"
notHired <- "#FD994F"
bgCol <- "#1F2133"
textCol <- "#82889B"

theme_everest <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = bgCol, 
                                         colour = bgCol),
          panel.grid = element_line(color = bgCol),
          panel.background = element_rect(fill = bgCol, 
                                          colour = bgCol),
          text = element_text(colour = textCol, family = "Segoe UI"), 
          plot.title = element_text(family = "Reprise Stamp", 
                                    colour = textCol, 
                                    hjust = 0, size = 20),
          axis.text = element_text(size = 8),
          plot.caption = element_text(size = 8, hjust = 1),
          plot.subtitle = element_markdown(hjust = 0, size = 9, lineheight = 1, family = "Segoe UI"),
          axis.title = element_text(size = 8, hjust = 0.15),
          axis.ticks = element_blank())
}

# Plot it! ----
ggplot() +
  geom_area(data = filter(everestDecades, plotgroup == "successes"),
            aes(x = plotyear, y = value, colour = hired, stat = "bin"), 
            fill = textCol, alpha = 0.5, show.legend = F) +
  geom_area(data = filter(everestDecades, plotgroup == "deaths"),
            aes(x = plotyear, y = value, colour = hired, stat = "bin"), 
            fill = textCol, alpha = 0.5, show.legend = F) +
  # scale_fill_manual(values = c(notHired, hired)) + 
  scale_colour_manual(values = c(notHired, hired)) + 
  geom_jitter(data = everestDeaths,
              aes(x = year, y = value, colour = hired, 
                  alpha = death_height_metres, shape = success), show.legend = F) +
  scale_shape_manual(values=c(3, 8)) +
  scale_x_continuous(name = "", breaks=seq(1920, 2020, 20)) +
  scale_y_continuous(name = "Number of climbers", breaks=c(0, 1000, 2000, 3000),
                     sec.axis = sec_axis(trans=~(.-1200)/100, name="Age",
                                         breaks = seq(30, 90, 20))) +
  geom_hline(yintercept = 0, size = 0.5, colour = textCol) +
  labs(title = "  
The Shadow of the Ascent
       ",
       subtitle = "Each peak above zero represents the number of people who made it to the  
summit of Everest and back in each decade. Each peak below zero shows the   
number of poeple who died on expeditions in each decade. The valleys   
show the cumulative means for each group. The peaks and valleys  
are staggered for ease of viewing.  
  
Because the deaths risk being dwarved by the successes, each star   
represents a person who died on an expedition each year. The height  
of the star represents their age. The higher they were when they died,   
the brigher their star. Those who had succeeded in making it to the  
summit are represented by asterisks, others by crosses.   
  
Throughout this plot, <span style='color:#E87EA1'>hired workers</span>, 94% of whom  
are from Nepal, are compared to <span style='color:#FD994F'>other climbers</span>  
who hail from 142 different nations.",
       caption = "@cararthompson | #TidyTuesday | Source: The Himalayan Database") +
  theme_everest()

# Export ----
ggsave(filename = "../plots/202010_everest.png", height = 7, width = 6, dpi = 400)



