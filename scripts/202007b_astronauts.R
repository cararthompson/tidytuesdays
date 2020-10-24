# Astronauts!
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(gridExtra)
library(extrafont)

## Get the data ####
tt_data <- tt_load(2020, week = 29)
astronauts <- tt_data$astronauts %>%
  mutate(age_at_mission = year_of_mission - year_of_birth)

## Build space theme ####
theme_astro <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#1d1330", colour = "#1d1330"),
          panel.grid = element_line(color = "#1d1330"),
          panel.background = element_rect(fill = "#1d1330", colour = "#1d1330"),
          text = element_text(colour = "white", family = "Corbel Light"), 
          plot.title = element_text(hjust = 0, size = 20, family = "AR DESTINE"),
          axis.text = element_text(color = "white", size = 10),
          plot.subtitle = element_markdown(hjust = 0, size = 13, lineheight = 1),
          axis.title = element_text(color = "white", size = 10),
          axis.ticks = element_blank())
}

## Add text for annotation bubbles ####
texts <- tibble(
  age = c(77, 72, 15, 20),
  year = c(2010, 1985, 1968, 2005),
  text = c(
    "This is not a typo! Meet **John Herschel Glenn Jr.**, who travelled to space aged 77 in 1999. What a legend!",
    "1985 was the year that saw the **most astronauts in space**, with a total of 62 on 28 missions.",
    "The **two youngest astronauts** were Gherman Titov and Valentina Tereshkova, both aged 26. They each flew only one mission. It would be 1982 before the next female astronaut took to space.",
    "**Sergei Krikalev** went on his first of six missions aged 30. Only two astronauts have been on more missions: Franklin R. Chang-Diaz and Jerry L. Ross, who both started their careers in the 1980 NASA-9 selection."),
  vjust = c(.5, .5, .5, .5)
) 

## Plot it! ####
ggplot(astronauts) +
  geom_point(aes(y = year_of_mission, x = age_at_mission, colour = sex,
                 size = hours_mission, alpha = total_number_of_missions),
             show.legend = F) +
  scale_colour_manual(values = c(male = "#e1f7fa", female = "#ffa72b")) +
  labs(title = "
Ages through Time and Space
       ",
       subtitle = "  
**Astronauts have got older, missions have got longer, and starting younger is no guarantee  
of going more often.** 
  
Each dot is an astronaut on a mission. The larger the dot, the more hours the mission took,  
ranging from 0 to over 10,000 (14 months!). The more transparent the dot, the fewer times  
that astronaut went to space.  

The slope of age by year of mission is similar for <span style='color:#e1f7fa'>male</span> and <span style='color:#ffa72b'>female</span> astronauts, with a 20-year  
time lag.  

All this with a few notable exceptions...",
       x = "Age at start of mission",
       y = "",
       caption =  "\n\n#TidyTuesday | Graphic: @cararthompson | Data:  Mariya Stavnichuk and Tatsuya Corlett") +
  xlim(c(10, 85)) +
  geom_textbox(data = texts,
               aes(age, year, 
                   label = text,
                   vjust = vjust),
               colour = "white",
               box.colour = "#1d1330",
               size = 3.8,
               fill = "#1d1330",
               family = "Corbel Light",
               maxwidth = unit(8, "lines"),
               hjust = .5,
               show.legend = F) +
  annotate("curve", x = 77, xend = 77, y = 2005, yend = 1999.5, curvature = 0, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#938ca1") +
  annotate("curve", x = 65, xend = 60, y = 1985, yend = 1985, curvature = 0, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#938ca1") +
  annotate("curve", x = 21, xend = 34, y = 1963, yend = 1981, curvature = .3, 
           size = .5, linetype = 2, arrow = arrow(length = unit(3, "mm")), colour = "#ffa72b") +
  annotate("curve", x = 21, xend = 26, y = 1970, yend = 1964, curvature = -0.4, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#938ca1") +
  annotate("curve", x = 25, xend = 30, y = 2000, yend = 1990, curvature = -0.3, 
           size = .75, arrow = arrow(length = unit(2, "mm")), colour = "#938ca1") +
  theme_astro()

# Export to create making-of gif, adapting the approach used by Georgios Karamanis (@geokaramanis)
ggsave(filename = file.path("../making-of/temp", paste0("202007b_astronauts-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), 
       dpi = 400, width = 10, height = 10)
