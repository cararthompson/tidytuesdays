# Flights!
library(tidyverse)
library(extrafont)
library(ggtext)

# Get the data
flight_data <- tidytuesdayR::tt_load(x = "2022-07-12")

# Wrangle and plot
p <- flight_data$flights %>%
  filter(APT_NAME == "Paris-Charles-de-Gaulle") %>%
  ggplot(aes(x = FLT_DATE,
             y = FLT_ARR_1)) +
  geom_line(aes(colour = APT_NAME), 
            colour = "white",
            alpha = 0.8, size = 1.5) +
  labs(x = "", y = "",
       title = "\"We're just a flight away!\"",
       subtitle = "The number of flights arriving at Paris Charles De Gaulle went from an average of<br>over 650 a day
       to 43 at the start of April 2020.<br>Suddenly, being \"just a flight away\"<br>was no source of comfort.<br>This simple
       graph is an ode<br>to the friends and families<br>who found themselves suddenly separated<br>during the pandemic.",
       caption = "@cararthompson | #TidyTuesday | Source: Eurocontrol") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue"),
        panel.grid = element_blank(),
        plot.title = element_markdown(family = "Daniel", colour = "#59516c", size = 24,
                                      hjust = 0.9, margin = unit(c(1.5, 0, 0, 0), "cm")),
        plot.subtitle = element_markdown(family = "DM Sans", colour = "#716A81", size = 16, 
                                         hjust = 0.9, lineheight = 1.3, margin = unit(c(1.5, 0, 1, 0), "cm")),
        plot.caption = element_markdown(family = "Daniel", colour = "#59516c", size = 12, 
                                        hjust = 0.9, margin = unit(c(1.5, 0, 0, 0), "cm")),
        axis.line.x = element_line(colour = "#C5C2CC"),
        axis.text = element_markdown(family = "Daniel", colour = "#59516c", size = 10))

# Export
ggsave(plot = p, filename = file.path("plots/", "202207_flights.png"), 
       dpi = 400, width = 11, height = 12, bg = "lightblue")
