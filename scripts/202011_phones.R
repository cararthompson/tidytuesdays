# Phones
library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggtext)

# Get data ----
tt_data <- tidytuesdayR::tt_load(2020, 46)
readme(tt_data)


# Plot it ----
theme_phone <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "#1d1330", colour = "#1d1330"),
          panel.grid = element_line(color = "#1d1330"),
          panel.background = element_rect(fill = "#1d1330", colour = "#1d1330"),
          text = element_text(colour = "white", family = "Corbel Light"), 
          plot.title = element_text(hjust = 0, size = 20, family = "NokiaKokia"),
          axis.text = element_text(color = "white", size = 10),
          plot.subtitle = element_markdown(hjust = 0, size = 12, lineheight = 1),
          axis.title = element_text(color = "white", size = 10),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())
}

tt_data$mobile %>%
  mutate(phones_per_person = mobile_subs / 100) %>%
  ggplot() +
  geom_smooth(aes(x = year, y = phones_per_person, 
                  colour = continent)) +
  geom_point(aes(x = year, y = phones_per_person, 
                 colour = continent,
                 size = gdp_per_cap),
             alpha = 0.6,
             show.legend = F) +
  geom_hline(yintercept = 1, lty = 3, colour = "white") +
  labs(title = "  
THE RISE OF THE  
INFORMATION WAVE  
       ",
       subtitle = "Each dot represents a country. The bigger its size, the higher its GDP per capita.  
The countries with the greatest number of mobile phone subscriptions per  
person have also tended to have the highest GDP per capita, with a  
notable exception for Europe: Montenegro* - peak phones per person  
in Europe in 2009 at 2.08, with a GPD per capita rank of 34/40.") +
  scale_size(range = c(.5, 2)) +
  labs(x = "Year",
       y = "Average Mobile Phone Subscriptions Per Person",
       legent = "Continent",
       caption = "@cararthompson | #TidyTuesday | Source: Our World In Data") +
  geom_text(aes(x = 2007.2, y = 2.27, 
                label = "*"),
            colour = "white",
            family = "Corbel Light",
            size = 8,
            hjust = .5,
            show.legend = F) +
  annotate("curve", x = 2007.6, xend = 2008.8, y = 2.28, yend = 2.13, curvature = -.2,
           size = .5, arrow = arrow(length = unit(2, "mm")), colour = "white") +
  guides(colour=guide_legend(override.aes=list(fill=NA))) +
  theme_phone()


# Export plot ----
ggsave(filename = "../plots/202011_phones.png", dpi = 400, width = 8, height = 9)



